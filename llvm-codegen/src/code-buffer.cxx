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

#include "llvm/IR/InlineAsm.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm/Support/FileSystem.h"

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
    _context(), _builder(this->_context), _module(nullptr),
  // initialize the register info
    _regInfo(target),
    _regState(this->_regInfo)
{
  // set up the target machine
    llvm::Triple triple;
    triple.setArch (target->arch);

    std::string errMsg;
//    auto tgt = llvm::TargetRegistry::lookupTarget(triple.getTriple(), errMsg);
    auto tgt = llvm::TargetRegistry::lookupTarget("x86_64-apple-macosx10.15.0", errMsg);
    assert (tgt && "unable to find target");

    llvm::TargetOptions tgtOpts;
    tgtOpts.GuaranteedTailCallOpt = true;
    this->_tgtMachine = tgt->createTargetMachine(
	triple.getTriple(),
	"generic",
	"",
	tgtOpts,
	llvm::None);

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

  // tell the module about the target machine
    this->_module->setTargetTriple(this->_tgtMachine->getTargetTriple().getTriple());
    this->_module->setDataLayout(this->_tgtMachine->createDataLayout());

  // setup the pass manager
    this->_passMngr = new llvm::legacy::FunctionPassManager (this->_module);

  // Do simple "peephole" optimizations and bit-twiddling optzns.
    this->_passMngr->add(llvm::createInstructionCombiningPass());
  // Reassociate expressions.
    this->_passMngr->add(llvm::createReassociatePass());
  // Eliminate Common SubExpressions.
    this->_passMngr->add(llvm::createGVNPass());
  // Simplify the control flow graph (deleting unreachable blocks, etc).
    this->_passMngr->add(llvm::createCFGSimplificationPass());

    this->_passMngr->doInitialization();

} // code_buffer::beginModule

void code_buffer::optimize ()
{
  // run the function optimizations over every function
    for (auto it = this->_module->begin();  it != this->_module->end();  ++it) {
	this->_passMngr->run (*it);
    }

}

void code_buffer::dumpAsm (std::string const &asmFile)
{
    std::error_code EC;
    llvm::raw_fd_ostream asmStrm(asmFile, EC, llvm::sys::fs::OF_None);
    if (EC) {
	llvm::errs() << "unable to open assembly output file\n";
	return;
    }

    llvm::legacy::PassManager pass;
    if (this->_tgtMachine->addPassesToEmitFile(pass, asmStrm, nullptr, llvm::CGFT_AssemblyFile))
    {
	llvm::errs() << "unable to generate assembly code\n";
	return;
    }

    pass.run(*this->_module);

    asmStrm.flush();

}

void code_buffer::endModule ()
{
    delete this->_passMngr;
    this->_passMngr = nullptr;

    delete this->_module;
    this->_module = nullptr;
}

void code_buffer::beginCluster (llvm::Function *fn)
{
    this->_overflowBB = nullptr;
    this->_fragMap.clear();
    this->_curFn = fn;

} // code_buffer::beginCluster

void code_buffer::endCluster ()
{
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

    llvm::Function *fn = this->_curFn;

  // initialize the base pointer (if necessary)
    if (attrs->get_needsBasePtr() && this->_regInfo.usesBasePtr()) {
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

    std::vector<CFG::param *> params = frag->get_params();
    int nExtra = this->_regInfo.numMachineRegs();
    for (int i = 0;  i < params.size();  i++) {
	this->insertVal (params[i]->get_0(), fn->getArg(nExtra + i));
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
	this->insertVal (params[i]->get_0(), phiNodes[nExtra + i]);
    }

} // code_buffer::setupFragEntry

Value *code_buffer::evalLabel (llvm::Function *fn)
{
    Value *basePtr = this->_regState.getBasePtr();

#ifdef XXX
    if (basePtr == nullptr) {
	return this->createBitCast(fn, this->mlValueTy);
    }
    else {
llvm::dbgs() << "\n## evalLabel (" << *fn << ")\n";
      // compute basePtr + (lab - curFn)
	return this->_builder.CreateIntToPtr(
	    this->createAdd (
		this->_builder.CreatePtrToInt(basePtr, this->intTy),
		this->createSub (
		    this->_builder.CreatePtrToInt(fn, this->intTy),
		    this->_builder.CreatePtrToInt(this->_curFn, this->intTy))),
	    this->mlValueTy);
    }
#else
      // compute basePtr + (lab - curFn)
	auto delta = this->createSub (
	    this->_builder.CreatePtrToInt(fn, this->intTy),
	    this->_builder.CreatePtrToInt(this->_curFn, this->intTy));
	auto adr = this->createAdd (this->_builder.CreatePtrToInt(basePtr, this->intTy), delta);
	return this->_builder.CreateIntToPtr(adr, this->mlValueTy);
#endif

} // code_buffer::evalLabel

#define USE_INLINE_ASM_TO_ACCESS_STK

// private function for loading a special register from memory
Value *code_buffer::_loadMemReg (sml_reg_id r)
{
#ifdef USE_INLINE_ASM_TO_ACCESS_STK
    auto info = this->_regInfo.info(r);
    auto fnTy = llvm::FunctionType::get (this->mlValueTy, false); // i64Ty *load();
    llvm::InlineAsm *load = llvm::InlineAsm::get (
	fnTy,
	"movq " + std::to_string(info->offset()) + "(%rsp),$0", // "movq offset(%rsp),dst"
	"=r",
	false);
    return this->_builder.CreateCall (fnTy, load);
#else
    auto info = this->_regInfo.info(r);
    auto fp = this->_builder.CreateCall(this->_frameAddress(), { this->i32Const(0) });
    auto adr = this->_builder.CreateBitCast(
	this->_builder.CreateInBoundsGEP(fp, { this->iConst(info->offset()) }),
	this->objPtrTy);

    return this->_builder.CreateAlignedLoad (this->mlValueTy, adr, this->_wordSzB, info->name());
#endif

} // code_buffer::_loadMemReg

// private function for setting a special memory register
void code_buffer::_storeMemReg (sml_reg_id r, Value *v)
{
#ifdef USE_INLINE_ASM_TO_ACCESS_STK
    auto info = this->_regInfo.info(r);
    auto fnTy = llvm::FunctionType::get (this->voidTy, { this->mlValueTy }, false); // void store(i64Ty *v);
    llvm::InlineAsm *store = llvm::InlineAsm::get (
	fnTy,
	"movq $0," + std::to_string(info->offset()) + "(%rsp)", // "movq src,offset(%rsp)"
	"r",
	true);
    this->_builder.CreateCall (fnTy, store, { v });
#else
    auto info = this->_regInfo.info(r);
    auto fp = this->_builder.CreateCall(this->_frameAddress(), { this->i32Const(0) });
    auto adr = this->_builder.CreateBitCast(
	this->_builder.CreateInBoundsGEP(fp, { this->iConst(info->offset()) }),
	this->objPtrTy);

    this->_builder.CreateAlignedStore (v, adr, this->_wordSzB);
#endif

} // code_buffer::_storeMemReg

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


// dump the current module to stderr
void code_buffer::dump () const { this->_module->dump(); }

// run the LLVM verifier on the module
bool code_buffer::verify () const
{
    return llvm::verifyModule (*this->_module, &llvm::dbgs());

}

