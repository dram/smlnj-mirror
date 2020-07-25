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
    this->f32Ty = llvm::Type::getPrimitiveType (this->_context, llvm::Type::FloatTyID);
    this->f64Ty = llvm::Type::getPrimitiveType (this->_context, llvm::Type::DoubleTyID);

    if (this->_target->wordSz == 32) {
	this->intTy = this->i32Ty;
    }
    else { // info.wordSz == 64
	this->intTy = this->i64Ty;
    }
    this->mlRefTy = this->intTy->getPointerTo (ML_REF_ADDR_SP);
    this->mlPtrTy = this->intTy->getPointerTo (ML_HEAP_ADDR_SP);
    this->bytePtrTy = this->i8Ty->getPointerTo (ML_HEAP_ADDR_SP);
    this->voidTy = llvm::Type::getVoidTy (this->_context);

} // constructor

void code_buffer::initModule (std::string const & src)
{
    this->_module = new llvm::Module (src, this->_context);

  // clear the cached intrinsic functions
    this->_sadd32WO = nullptr;
    this->_ssub32WO = nullptr;
    this->_smul32WO = nullptr;
    this->_sadd64WO = nullptr;
    this->_ssub64WO = nullptr;
    this->_smul64WO = nullptr;

} // code_buffer::initModule

void code_buffer::beginCluster ()
{
    this->_overflowBB = nullptr;
    this->_fragMap.clear();
    this->_vMap.clear();

} // code_buffer::beginCluster

void code_buffer::endCluster ()
{
    if (this->_overflowBB != nullptr) {
/* FIXME: initialize the overflow BB */
    }

/* TODO: call GC code? */

} // code_buffer::endCluster

llvm::Function *code_buffer::newFunction (std::vector<llvm::Type *> paramTys, bool isFirst)
{
    llvm::Type *allParams[32];	// no target machine has more than 32 registers

  // the parameter list starts with the special registers (i.e., alloc ptr, ...), which
  // are all given the SML pointer type
    int nExtra = this->_regInfo.numSpecialRegs();
    int i = 0;
    while (i < nExtra) {
	allParams[i++] = this->mlPtrTy;
    }

  // then add the types from the function's formal parameters
    for (auto paramTy : paramTys) {
	allParams[i++] = paramTy;
    }

    llvm::FunctionType *fnTy = llvm::FunctionType::get (
	this->voidTy,
	llvm::ArrayRef<llvm::Type *>(allParams, i),
	false);

    this->_curFn = llvm::Function::Create (
	    fnTy,
	    isFirst ? llvm::GlobalValue::ExternalLinkage : llvm::GlobalValue::PrivateLinkage,
	    isFirst ? "main" : "",
	    this->_module);

    return this->_curFn;

}

// setup the argument/parameter lists for a fragment
Args_t code_buffer::setupFragArgs (CFG::frag *frag, Args_t &args)
{
    Args_t newArgs;

    int nExtra = this->_regInfo.numSpecialRegs();
    newArgs.reserve (args.size() + nExtra);

  // add initial arguments for those reserved registers that are mapped to hardware registers
    for (int i = 0;  i < nExtra;  i++) {
	sml_reg_id id = this->_regInfo.specialId(i);
	assert (this->_regInfo.info(id)->isMachineReg() && "not a machine register");
	newArgs.push_back (this->_regState.get(id));
    }

  // copy the rest of the arguments
    for (auto arg : args) {
	newArgs.push_back (arg);
    }

    return newArgs;

} // code_buffer::setupFragArgs

void code_buffer::setupFragEntry (CFG::frag *frag)
{
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

