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
#include "cfg.hxx" // for argument setup

#include <exception>

/* FIXME: for now, these are all zero, but we should do something else */
/* address spaces for various kinds of ML data that are necessarily disjoint */
#define ML_HEAP_ADDR_SP		0		// immutable heap objects
#define ML_REF_ADDR_SP		0		// mutable heap objects

/***** class code_buffer member functions *****/

code_buffer *code_buffer::create (std::string const & target)
{
    code_buffer *buf = new code_buffer (target);

    return buf;
}

code_buffer::code_buffer (std::string const & target)
  : _target(target_info::InfoForTarget(target)),
    _context(), _builder(this->_context), _module(nullptr),
  // initialize the register info
    _regInfo(this->_target),
    _regState(this->_regInfo)
{
    if (this->_target == nullptr) {
	throw std::invalid_argument ("invalid target " + target);
    }

  // initialize the standard types that we use
    this->i8Ty = llvm::IntegerType::get (this->_context, 8);
    this->i16Ty = llvm::IntegerType::get (this->_context, 16);
    this->i32Ty = llvm::IntegerType::get (this->_context, 32);
    this->i64Ty = llvm::IntegerType::get (this->_context, 64);
    this->f32Ty = Type::getPrimitiveType (this->_context, Type::FloatTyID);
    this->f64Ty = Type::getPrimitiveType (this->_context, Type::DoubleTyID);

    if (this->_target->wordSz == 32) {
	this->intTy = this->i32Ty;
    }
    else { // info.wordSz == 64
	this->intTy = this->i64Ty;
    }
    this->mlValueTy = this->intTy->getPointerTo ();
    this->objPtrTy = this->mlValueTy->getPointerTo ();
    this->bytePtrTy = this->i8Ty->getPointerTo (ML_HEAP_ADDR_SP);
    this->voidTy = Type::getVoidTy (this->_context);

} // constructor

void code_buffer::beginModule (std::string const & src, int nClusters)
{
    this->_module = new llvm::Module (src, this->_context);

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

} // code_buffer::beginModule

void code_buffer::beginCluster (llvm::Function *fn)
{
    this->_overflowBB = nullptr;
    this->_fragMap.clear();
    this->_curFn = fn;

} // code_buffer::beginCluster

void code_buffer::endCluster ()
{
    if (this->_overflowBB != nullptr) {
/* FIXME: initialize the overflow BB */
    }

/* TODO: call GC code? */

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

llvm::FunctionType *code_buffer::createFnTy (std::vector<Type *> const & tys) const
{
    std::vector<Type *> allParams = this->createParamTys (tys.size());

  // add the types from the function's formal parameters
    for (auto ty : tys) {
	allParams.push_back (ty);
    }

    return llvm::FunctionType::get (
	this->voidTy,
	llvm::ArrayRef<Type *>(allParams),
	false);

}

std::vector<Type *> code_buffer::createParamTys (int n) const
{
    std::vector<Type *> tys;

    int nExtra = this->_regInfo.numMachineRegs();

    tys.reserve(tys.size() + nExtra);

  // the parameter list starts with the special registers (i.e., alloc ptr, ...),
  //
    for (int i = 0;  i < nExtra;  ++i) {
	if (this->_regInfo.machineReg(i)->id() <= sml_reg_id::STORE_PTR) {
	    tys.push_back (this->objPtrTy);
	} else {
	    tys.push_back (this->mlValueTy);
	}
    }

    return tys;

}

Args_t code_buffer::createArgs (int n)
{
    Args_t args;
    int nExtra = this->_regInfo.numMachineRegs();
    args.reserve (n + nExtra);

  // seed the args array with the extra arguments
    for (int i = 0;  i < nExtra;  ++i) {
	args.push_back (this->_regState.get (this->_regInfo.machineReg(i)));
    }

    return args;
}

// setup the incoming arguments for a standard function entry
//
void code_buffer::setupStdEntry (CFG::frag *frag)
{
  // the order of incoming arguments is:
  //
  //	1. special registers: ALLOC_PTR, LIMIT_PTR, STORE_PTR, EXN_HNDLR, VAR_PTR, BASE_PTR
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

    llvm::Function *fn = this->_curFn;
    int nExtra = this->_regInfo.numMachineRegs();

  // initialize the register state
    for (int i = 0;  i < nExtra;  ++i) {
	reg_info const *info = this->_regInfo.machineReg(i);
	llvm::Argument *arg = this->_curFn->getArg(i);
#ifndef NDEBUG
	arg->setName (info->name());
#endif
	this->_regState.set (info->id(), arg);
    }

    std::vector<CFG::param *> params = frag->get_params();
    for (int i = 0;  i < params.size();  i++) {
	this->insertVal (params[i]->get_0(), fn->getArg(nExtra + i));
    }

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
	this->insertVal (params[i]->get_0(), phiNodes[nExtra + i]);
    }

} // code_buffer::setupFragEntry

// return the basic-block that contains the Overflow trap generator
llvm::BasicBlock *code_buffer::getOverflowBB ()
{
    if (this->_overflowBB == nullptr) {
	this->_overflowBB = this->newBB ();
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

