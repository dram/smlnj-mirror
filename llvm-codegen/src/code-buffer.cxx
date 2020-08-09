/// \file codegen.cxx
///
/// \copyright 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
/// All rights reserved.
///
/// \brief This file implements the methods for the `code_buffer` class
///
/// \author John Reppy
///

#include "code-buffer.hxx"
#include "target-info.hxx"
#include "mc-gen.hxx"
#include "cfg.hxx" // for argument setup

#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/InlineAsm.h"
#include "llvm/IR/Verifier.h"

#include <exception>

/* FIXME: for now, these are all zero, but we should do something else */
/* address spaces for various kinds of ML data that are necessarily disjoint */
#define ML_HEAP_ADDR_SP		0		// immutable heap objects
#define ML_REF_ADDR_SP		0		// mutable heap objects

/***** class code_buffer member functions *****/

code_buffer *code_buffer::create (std::string const & target)
{
    auto tgtInfo = target_info::InfoForTarget (target);
    if (tgtInfo == nullptr) {
	return nullptr;
    }

    code_buffer *buf = new code_buffer (tgtInfo);

    return buf;
}

code_buffer::code_buffer (target_info const *target)
  : _target(target),
    _context(), _builder(this->_context),
    _gen(nullptr),
  // initialize the register info
    _regInfo(target),
    _regState(this->_regInfo)
{
    this->_gen = new mc_gen (this->_context, target),

  // initialize the standard types that we use
    this->i8Ty = llvm::IntegerType::get (this->_context, 8);
    this->i16Ty = llvm::IntegerType::get (this->_context, 16);
    this->i32Ty = llvm::IntegerType::get (this->_context, 32);
    this->i64Ty = llvm::IntegerType::get (this->_context, 64);
    this->f32Ty = Type::getPrimitiveType (this->_context, Type::FloatTyID);
    this->f64Ty = Type::getPrimitiveType (this->_context, Type::DoubleTyID);

    if (this->_target->wordSz == 32) {
	this->intTy = this->i32Ty;
	this->_wordSzB = 4;
    }
    else { // info.wordSz == 64
	this->intTy = this->i64Ty;
	this->_wordSzB = 8;
    }
    this->mlValueTy = this->intTy->getPointerTo ();
    this->objPtrTy = this->mlValueTy->getPointerTo ();
    this->bytePtrTy = this->i8Ty->getPointerTo (ML_HEAP_ADDR_SP);
    this->voidTy = Type::getVoidTy (this->_context);

  // "call-gc" types
    {
	int n = target->numCalleeSaves + 4;
	std::vector<Type *> gcTys = this->createParamTys (frag_kind::STD_FUN, n);
	for (int i = 0;  i < n;  ++i) {
	    gcTys.push_back (this->mlValueTy);
	}
	this->gcRetTy = llvm::StructType::create(gcTys, "gc_ret_ty");
	this->gcFnTy = llvm::FunctionType::get(this->gcRetTy, gcTys, false);
    }

} // constructor

void code_buffer::beginModule (std::string const & src, int nClusters)
{
    this->_module = new llvm::Module (src, this->_context);

    this->_gen->beginModule (this->_module);

  // prepare the label-to-cluster map
    this->_clusterMap.clear();
    this->_clusterMap.reserve(nClusters);

  // clear the cached intrinsic functions
    this->_sadd32WO = nullptr;
    this->_ssub32WO = nullptr;
    this->_smul32WO = nullptr;
    this->_sadd64WO = nullptr;
    this->_ssub64WO = nullptr;
    this->_smul64WO = nullptr;
    this->_fabs32 = nullptr;
    this->_fabs64 = nullptr;
    this->_sqrt32 = nullptr;
    this->_sqrt64 = nullptr;
    this->_readReg = nullptr;
    this->_spRegMD = nullptr;

} // code_buffer::beginModule

void code_buffer::optimize ()
{
    this->_gen->optimize (this->_module);
}

void code_buffer::endModule ()
{
    this->_gen->endModule();
    delete this->_module;
}

void code_buffer::beginCluster (CFG::cluster *cluster, llvm::Function *fn)
{
    this->_overflowBB = nullptr;
    this->_fragMap.clear();
    this->_curFn = fn;
    this->_curCluster = cluster;

} // code_buffer::beginCluster

void code_buffer::endCluster ()
{
/* TODO: emit common GC code? */

} // code_buffer::endCluster

void code_buffer::beginFrag ()
{
    this->_vMap.clear();

} // code_buffer::beginFrag

llvm::Function *code_buffer::newFunction (
    llvm::FunctionType *fnTy,
    std::string const &name,
    bool isFirst)
{
    llvm::Function *fn = llvm::Function::Create (
	    fnTy,
	    isFirst ? llvm::GlobalValue::ExternalLinkage : llvm::GlobalValue::PrivateLinkage,
	    name,
	    this->_module);

  // set the calling convention to our "Jump-with-arguments" convention
    fn->setCallingConv (llvm::CallingConv::JWA);

  // assign attributes to the function
    fn->addFnAttr (llvm::Attribute::Naked);

    return fn;

}

llvm::FunctionType *code_buffer::createFnTy (frag_kind kind, std::vector<Type *> const & tys) const
{
    std::vector<Type *> allParams = this->createParamTys (kind, tys.size());

  // add the types from the function's formal parameters
    for (auto ty : tys) {
	allParams.push_back (ty);
    }

    return llvm::FunctionType::get (
	this->voidTy,
	llvm::ArrayRef<Type *>(allParams),
	false);

}

std::vector<Type *> code_buffer::createParamTys (frag_kind kind, int n) const
{
    std::vector<Type *> tys;

    int nExtra = this->_regInfo.numMachineRegs();

  // standard continuations do not use the first two registers of
  // the JWA convention (STD_LINK and STD_CLOS).
    int nUnused = (kind == frag_kind::STD_CONT) ? 2 : 0;

    tys.reserve(tys.size() + nExtra + nUnused);

  // the parameter list starts with the special registers (i.e., alloc ptr, ...),
  //
    for (int i = 0;  i < nExtra;  ++i) {
	if (this->_regInfo.machineReg(i)->id() <= sml_reg_id::STORE_PTR) {
	    tys.push_back (this->objPtrTy);
	} else {
	    tys.push_back (this->mlValueTy);
	}
    }

  // we give the unused registers the ML value type
    for (int i = 0;  i < nUnused;  ++i) {
	tys.push_back (this->mlValueTy);
    }

    return tys;

}

Args_t code_buffer::createArgs (frag_kind kind, int n)
{
    Args_t args;

    int nExtra = this->_regInfo.numMachineRegs();

  // standard continuations do not use the first two registers of
  // the JWA convention (STD_LINK and STD_CLOS).
    int nUnused = (kind == frag_kind::STD_CONT) ? 2 : 0;

    args.reserve (n + nExtra + nUnused);

  // seed the args array with the extra arguments
    for (int i = 0;  i < nExtra;  ++i) {
	args.push_back (this->_regState.get (this->_regInfo.machineReg(i)));
    }

  // we assign the unused argument registers the undefined value
    for (int i = 0;  i < nUnused;  ++i) {
	args.push_back (llvm::UndefValue::get(this->mlValueTy));
    }

    return args;
}

// setup the incoming arguments for a standard function entry
//
void code_buffer::setupStdEntry (CFG::attrs *attrs, CFG::frag *frag)
{
  // the order of incoming arguments is:
  //
  //	1. special registers: ALLOC_PTR, LIMIT_PTR, STORE_PTR, EXN_HNDLR, VAR_PTR
  //
  //    2. STD_LINK, STD_CLOS, STD_CONT
  //
  //	3. general purpose callee-saves (MISC0, MISC1, ...)
  //
  //	4. floating-point callee-saves (FPR0, FPR1, ...)
  //
  //    5. argument registers: STDARG, MISC{n}, MISC{n+1}, ... / FPR{m}, FPR{m+1}, ...
  //	   where "n" is the number of callee saves and "m" is the number of floating-point
  //	   callee-saves
  //
  // For continuations, the STD_LINK and STD_CLOS registers are undefined and do not
  // correspond to CFG parameters

    llvm::Function *fn = this->_curFn;

  // initialize the base pointer (if necessary)
    if (attrs->get_needsBasePtr() && this->_regInfo.usesBasePtr()) {
/* FIXME: need different code for continuations!!! */
      // STDLINK holds the function's address and is the first non-special argument.
	this->_regState.setBasePtr (this->_curFn->getArg(this->_regInfo.numMachineRegs()));
    }

  // initialize the register state
    for (int i = 0, hwIx = 0;  i < reg_info::NUM_REGS;  ++i) {
	reg_info const *info = this->_regInfo.info(static_cast<sml_reg_id>(i));
	if (info->isMachineReg()) {
	    llvm::Argument *arg = this->_curFn->getArg(hwIx++);
#ifndef NDEBUG
	    arg->setName (info->name());
#endif
	    this->_regState.set (info->id(), arg);
	}
	else { // stack-allocated register
	    this->_regState.set (info->id(), nullptr);
	}
    }

    int nExtra = this->_regInfo.numMachineRegs();
    int nUnused = ((frag->get_kind() == frag_kind::STD_CONT) ? 2 : 0);

    std::vector<CFG::param *> params = frag->get_params();
    for (int i = 0;  i < params.size();  i++) {
	params[i]->bind (this, fn->getArg(nExtra + i));
    }

// FIXME: need initialize the base pointer
}

void code_buffer::setupFragEntry (CFG::frag *frag, std::vector<llvm::PHINode *> &phiNodes)
{
    int nExtra = this->_regInfo.numMachineRegs();

  // initialize the register state
    for (int i = 0;  i < nExtra;  ++i) {
	reg_info const *info = this->_regInfo.machineReg(i);
	this->_regState.set (info->id(), phiNodes[i]);
    }

  // bind the formal parameters to the remaining PHI nodes
    std::vector<CFG::param *> params = frag->get_params();
    for (int i = 0;  i < params.size();  i++) {
	params[i]->bind (this, phiNodes[nExtra + i]);
    }

} // code_buffer::setupFragEntry

Value *code_buffer::evalLabel (llvm::Function *fn)
{
    Value *basePtr = this->_regState.getBasePtr();

    if (basePtr == nullptr) {
      // define an alias for `(lab - 0)`
	auto alias = llvm::GlobalAlias::create (
	    this->intTy,
	    0,
	    llvm::GlobalValue::PrivateLinkage,
	    fn->getName() + "_alias",
	    llvm::ConstantExpr::getIntToPtr(
		llvm::ConstantExpr::getSub (
		    llvm::ConstantExpr::getPtrToInt(fn, this->intTy),
		    llvm::Constant::getNullValue(this->intTy)),
		this->mlValueTy),
	    this->_module);
	alias->setUnnamedAddr (llvm::GlobalValue::UnnamedAddr::Global);
	return alias;
    }
    else {
      // define an alias for the value `(lab - curFn)`
	auto delta = llvm::GlobalAlias::create (
	    this->intTy,
	    0,
	    llvm::GlobalValue::PrivateLinkage,
	    fn->getName() + "_sub_" + this->_curFn->getName(),
	    llvm::ConstantExpr::getIntToPtr(
		llvm::ConstantExpr::getSub (
		    llvm::ConstantExpr::getPtrToInt(fn, this->intTy),
		    llvm::ConstantExpr::getPtrToInt(this->_curFn, this->intTy)),
		this->mlValueTy),
	    this->_module);
	delta->setUnnamedAddr (llvm::GlobalValue::UnnamedAddr::Global);

      // compute basePtr + (lab - curFn)
	return this->_builder.CreateIntToPtr(
	    this->createAdd (
		this->_builder.CreatePtrToInt(basePtr, this->intTy),
		delta),
	    this->mlValueTy);
    }

} // code_buffer::evalLabel

void code_buffer::_initSPAccess ()
{
    assert ((this->_readReg == nullptr) && (this->_spRegMD == nullptr));
    this->_readReg = _getIntrinsic (llvm::Intrinsic::read_register, this->intTy);
    this->_spRegMD = llvm::MDNode::get (
	this->_context,
	llvm::MDString::get(this->_context, "rsp"));

}

// utility function for loading a value from the stack
inline Value *_loadFromStack (code_buffer *buf, int offset, std::string const &name)
{
    return buf->build().CreateAlignedLoad (
	buf->stkAddr (buf->objPtrTy, offset),
	buf->wordSzInBytes(),
	name);
}

// private function for loading a special register from memory
Value *code_buffer::_loadMemReg (sml_reg_id r)
{
    auto info = this->_regInfo.info(r);
    return _loadFromStack (this, info->offset(), info->name());

} // code_buffer::_loadMemReg

// private function for setting a special memory register
void code_buffer::_storeMemReg (sml_reg_id r, Value *v)
{
    auto info = this->_regInfo.info(r);
    auto stkAddr = this->stkAddr (v->getType()->getPointerTo(), info->offset());
    this->_builder.CreateAlignedStore (
	v,
	stkAddr,
	this->_wordSzB);

} // code_buffer::_storeMemReg

// utility function for allocating a record of ML values (pointers or
// tagged ints).
//
Value *code_buffer::allocRecord (uint64_t desc, Args_t const & args)
{
    int len = args.size();
    Value *allocPtr = this->mlReg (sml_reg_id::ALLOC_PTR);

  // write object descriptor
    this->build().CreateAlignedStore (
	this->createIntToPtr(this->uConst(desc), this->mlValueTy),
	allocPtr,
	(unsigned)this->_wordSzB);

  // initialize the object's fields
    for (int i = 1;  i <= len;  ++i) {
	this->build().CreateAlignedStore (
	    this->asMLValue (args[i-1]),
	    this->createGEP (allocPtr, i),
	    (unsigned)this->_wordSzB);
    }

  // compute the object's address and cast it to an ML value
    Value *obj = this->asMLValue (this->createGEP (allocPtr, 1));

  // bump the allocation pointer
    this->setMLReg (sml_reg_id::ALLOC_PTR, this->createGEP (allocPtr, len + 1));

    return obj;
}

// generate code to invoke the garbage collector.  This code is responsible
// for packaging up excess live variables into heap objects and then
// restoring them after the garbage collection.
//
llvm::BasicBlock *code_buffer::invokeGC (CFG::frag const *frag, frag_kind kind)
{
    std::vector<CFG::param *> params = frag->get_params();
    int numParams = params.size();

  // switch to a new block to the code
    llvm::BasicBlock *saveBB = this->_builder.GetInsertBlock ();
    llvm::BasicBlock *bb = this->newBB();
    this->_builder.SetInsertPoint (bb);

  // In addition to the special "SML" registers, the GC calling convention
  // uses the standard linkage registers (STD_LINK, STD_CLOS, STD_CONT),
  // the callee-save registers (MISC1, ...), and STD_ARG.
  //
    int numCalleeSaves = this->targetInfo()->numCalleeSaves;
    int numGCArgs = 4 + numCalleeSaves;

  // classify arguments into ML values (pointers and tagged ints), raw
  // 32-bit values (ints and floats), and raw 64-bit values (ints and floats)
  //
    std::vector<int> mlArgs;		// indices of ML-value parameters
    std::vector<int> raw32Args;		// indices of 32-bit raw parameters
    std::vector<int> raw64Args;		// indices of 64-bit raw parameters
    std::vector<Type *> paramTys;	// LLVM types of the parameters
    for (int i = 0;  i < numParams;  ++i) {
	CFG::ty *ty = params[i]->get_ty();
	if (ty->isNUMt()) {
	    int sz = dynamic_cast<CFG::NUMt *>(ty)->get_sz();
	    if (sz == 32) {
		raw32Args.push_back (i);
	    } else {
		raw64Args.push_back (i);
	    }
	} else if (ty->isFLTt()) {
	    int sz = dynamic_cast<CFG::FLTt *>(ty)->get_sz();
	    if (sz == 32) {
		raw32Args.push_back (i);
	    } else {
		raw64Args.push_back (i);
	    }
	} else {
	    mlArgs.push_back (i);
	}
	paramTys.push_back (ty->codegen (this));
    }

  // compute the size of the raw live data in words (including padding)
  // and whether we need to force 64-bit alignment for the allocation pointer
    int rawSz = 4 * raw32Args.size(); // bytes
    bool align64 = (raw64Args.size() > 0) || this->is64Bit();
    if (align64) {
      // pad to 8 bytes and add 64-bit space
	rawSz = 8 * (((rawSz + 7) >> 3) + raw64Args.size());
    }
  // convert to words
    rawSz /= this->_wordSzB;

    Value *rawObj = nullptr;
    if (rawSz > 0) {
      // allocate a raw record for the live values
	Value *allocPtr = align64
	    ? this->alignedAllocPtr() // align the allocation pointer
	    : this->mlReg (sml_reg_id::ALLOC_PTR);
      // write the descriptor, which is `(rawSz << 7) | tag`, where `tag` is
      // either 0x12 (for tag_raw) or 0x16 (for tag_raw64).
	unsigned int desc = (rawSz << 7) | ((raw64Args.size() > 0) ? 0x16 : 0x12);
	this->_builder.CreateAlignedStore (
	    this->createIntToPtr(this->uConst(desc), this->mlValueTy),
	    allocPtr,
	    (unsigned)this->_wordSzB);
      // object address as `char *`
	Value *objAdr = this->createBitCast (
	    this->_builder.CreateInBoundsGEP (allocPtr, { this->uConst(1) }),
	    this->bytePtrTy);
      // write the 32-bit contents first
	unsigned int offset = 0;
	for (auto ix : raw32Args) {
	    auto param = params[ix];
	    auto ty = paramTys[ix];
	    this->_builder.CreateAlignedStore (
		this->lookupVal (param->get_name()),
		this->createBitCast(
		    this->createGEP(objAdr, offset),
		    ty->getPointerTo()),
		4);
	    offset += 4;
	}
      // pad the offset to 64-bits
	offset = (offset + 7) & ~7;
      // write the 64-bit contents
	for (auto ix : raw64Args) {
	    auto param = params[ix];
	    auto ty = param->get_ty()->codegen(this);
	    this->_builder.CreateAlignedStore (
		this->lookupVal (param->get_name()),
		this->createBitCast(
		    this->createGEP(objAdr, offset),
		    ty->getPointerTo()),
		8);
	    offset += 8;
	}
      // bump the allocation pointer
	this->setMLReg (sml_reg_id::ALLOC_PTR,
	    this->asObjPtr(this->createGEP (objAdr, offset)));
      // the raw object
	rawObj = this->asMLValue(objAdr);
    }

  // the number of ML arguments includes the raw object (if present)
    int nMLArgs = mlArgs.size() + (rawObj == nullptr ? 0 : 1);

  // if we have too many ML arguments, we need to put the excess values
  // in a record
    Value *extraObj = nullptr;
    if (nMLArgs > numGCArgs) {
      // allocate a record for the extra ML values
	Args_t extra;
	extra.reserve (nMLArgs - numGCArgs + 1);
	for (int i = numGCArgs - 1;  i < mlArgs.size();  ++i) {
	    extra.push_back (this->lookupVal(params[mlArgs[i]]->get_name()));
	}
	if (rawObj != nullptr) {
	    extra.push_back (rawObj);
	}
      // NOTE: record descriptors == (len << 7) | 2
	extraObj = this->allocRecord ((extra.size() << 7) | 2, extra);
    }

  // The GC calling convention uses seven registers to pass arguments
  // (assuming that there are three callee save registers).  These have
  // the following order in the JWA calling convention:
  //
  //	STD_LINK
  //	STD_CLOS
  //	STD_CONT
  //	MISC1 (callee save)
  //	MISC2 (callee save)
  //	MISC3 (callee save)
  //	STD_ARG
  //

  // the number of fragment parameters that will pass through to the GC
    int nPassThrough = std::min(nMLArgs, numGCArgs);
    if ((extraObj != nullptr) || (rawObj != nullptr)) {
	nPassThrough--;
    }

  // setup the GC arguments (not counting extras)
    std::vector<Type *> gcArgTys = this->createParamTys (frag_kind::STD_FUN, numGCArgs);
    Args_t gcArgs = this->createArgs (frag_kind::STD_FUN, numGCArgs);
    for (int i = 0;  i < nPassThrough;  ++i) {
	gcArgTys.push_back (this->mlValueTy);
	gcArgs.push_back (this->lookupVal(params[mlArgs[i]]->get_name()));
    }
    if (extraObj != nullptr) {
	gcArgTys.push_back (this->mlValueTy);
	gcArgs.push_back (extraObj);
    }
    else if (rawObj != nullptr) {
	gcArgTys.push_back (this->mlValueTy);
	gcArgs.push_back (rawObj);
    }
  // null out any uninitialized arguments
    int actualNumGCArgs = gcArgs.size();
    for (int i = actualNumGCArgs;  i < numGCArgs;  ++i) {
	gcArgTys.push_back (this->mlValueTy);
	gcArgs.push_back (this->uConst(1)); // == SML unit value
    }

  // get the address of the "call-gc" entry
    Value *callGCFn = _loadFromStack (this, this->_target->callGCOffset, "callGC");

llvm::dbgs() << "\nInvokeGC: callGCFn = " << *callGCFn << "\n";

  // call the garbage collector.  The return type of the GC is a struct
  // that contains the post-GC values of the argument registers
    auto call = this->_builder.CreateCall (
	this->gcFnTy,
	this->createBitCast(callGCFn, this->gcFnTy->getPointerTo()),
	gcArgs);
    call->setCallingConv (llvm::CallingConv::JWA);
    call->setTailCallKind (llvm::CallInst::TCK_NoTail);

  // restore the register state from the return struct
    for (unsigned i = 0, hwIx = 0;  i < reg_info::NUM_REGS;  ++i) {
	reg_info const *info = this->_regInfo.info(static_cast<sml_reg_id>(i));
	if (info->isMachineReg()) {
	    gcArgs[hwIx] = this->_builder.CreateExtractValue(call, { hwIx });
	    this->setMLReg (info->id(), gcArgs[hwIx]);
	    hwIx++;
	}
    }
    for (unsigned i = this->_regInfo.numMachineRegs();  i < actualNumGCArgs;  i++) {
	gcArgs[i] = this->_builder.CreateExtractValue(call, { i });
    }

  // the arguments for the return to the limit test
    Args_t retArgs = this->createArgs (kind, numParams);

  // restore registers from the extra and raw objects
    if (extraObj != nullptr) {
assert (false && "extra stuff");
    }
    else if (rawObj != nullptr) {
assert (false && "raw stuff");
    }
    else { // no extra work required
	for (int i = 0;  i < numParams;  ++i) {
	    Value *v = gcArgs[this->_regInfo.numMachineRegs() + i];
	    if (v->getType() != paramTys[i]) {
		v = this->castTy (v->getType(), paramTys[i], v);
	    }
	    retArgs.push_back (v);
	}
    }

  // transfer control back to the limit test
    if (kind == frag_kind::INTERNAL) {
      // update fragment's PHI nodes
assert (false && "phi nodes");
	this->createBr (frag->bb());
    } else {
      // args[0] holds the entry address of the cluster, so we transfer control to it.
	auto fnTy = this->_curCluster->fn()->getFunctionType();
	llvm::CallInst *call = this->_builder.CreateCall(
	    fnTy,
	    this->createBitCast(retArgs[0], fnTy->getPointerTo()),
	    retArgs);
	call->setCallingConv (llvm::CallingConv::JWA);
	call->setTailCallKind (llvm::CallInst::TCK_Tail);
      // terminate the block
	this->_builder.CreateRetVoid();
    }

    this->_builder.SetInsertPoint (saveBB);

    return bb;

} // code_buffer::invokeGC

// return the basic-block that contains the Overflow trap generator
llvm::BasicBlock *code_buffer::getOverflowBB ()
{
    if (this->_overflowBB == nullptr) {
	auto saveBB = this->_builder.GetInsertBlock ();
	this->_overflowBB = this->newBB ();
	this->_builder.SetInsertPoint (this->_overflowBB);
	auto fnTy = llvm::FunctionType::get (this->voidTy, false);	// void trap()
	llvm::InlineAsm *trap =
	    llvm::InlineAsm::get (
		fnTy,
		"int $$4",
		"",
		true);
	this->_builder.CreateCall (fnTy, trap);
	this->_builder.CreateRetVoid ();
      // restore current basic block
	this->_builder.SetInsertPoint (saveBB);
    }

    return this->_overflowBB;

} // code_buffer::getOverflowBB

// return branch-weight meta data, where `prob` represents the probability of
// the true branch and is in the range 1..999.
llvm::MDNode *code_buffer::branchProb (int prob)
{
    auto name = llvm::MDString::get(this->_context, "branch_weights");
    auto trueProb = llvm::ValueAsMetadata::get(this->i32Const(prob));
    auto falseProb = llvm::ValueAsMetadata::get(this->i32Const(1000 - prob));
    auto tpl = llvm::MDTuple::get(this->_context, {name, trueProb, falseProb});

    return tpl;

} // code_buffer::branchProb

// get the branch-weight meta data for overflow-trap branches
//
llvm::MDNode *code_buffer::overflowWeights ()
{
  // we use 1/1000 as the probability of an overflow
    return this->branchProb(1);

} // code_buffer::overflowWeights

// generate a type cast for an actual to formal transfer.  The type of the
// formal (`tgtTy`) is limited to be an objPtrTy, mlValueTy, intTy, or realTy.
// We should never need a cast in the latter two cases.
//
Value *code_buffer::castTy (Type *srcTy, Type *tgtTy, Value *v)
{
    if (tgtTy == this->mlValueTy) {
	if (srcTy->isPointerTy()) {
	    return this->_builder.CreateBitCast(v, this->mlValueTy);
	} else {
	    return this->_builder.CreateIntToPtr(v, this->mlValueTy);
	}
    }
    else if (tgtTy == this->objPtrTy) {
	return this->_builder.CreateBitCast(v, this->mlValueTy);
    }
    else {
	assert (false && "invalid type cast");
	return nullptr;
    }

} // code_buffer::castTy

llvm::Function *code_buffer::_getIntrinsic (llvm::Intrinsic::ID id, Type *ty) const
{
    return llvm::Intrinsic::getDeclaration (
	this->_module, id, llvm::ArrayRef<Type *>(ty));
}

void code_buffer::compile () const
{
    this->_gen->compile (this->_module);
}

void code_buffer::dumpAsm () const
{
    this->_gen->dumpCode (this->_module, "-", true);
}

void code_buffer::dumpAsm (std::string const &stem) const
{
    this->_gen->dumpCode (this->_module, stem, true);
}

void code_buffer::dumpObj (std::string const &stem) const
{
    this->_gen->dumpCode (this->_module, stem, false);
}

// dump the current module to stderr
void code_buffer::dump () const
{
#ifndef RELEASE_BUILD
    this->_module->dump();
#endif
}

// run the LLVM verifier on the module
bool code_buffer::verify () const
{
    return llvm::verifyModule (*this->_module, &llvm::dbgs());
}
