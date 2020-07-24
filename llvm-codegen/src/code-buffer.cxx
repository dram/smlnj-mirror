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

/* FIXME: for now, these are all zero, but we should do something else */
/* address spaces for various kinds of ML data that are necessarily disjoint */
#define ML_HEAP_ADDR_SP		0		// immutable heap objects
#define ML_REF_ADDR_SP		0		// mutable heap objects

/***** class code_buffer member functions *****/

code_buffer::code_buffer (std::string const & target)
  : _target(target_info::InfoForTarget(target)),
    _context(), _builder(this->_context), _module(nullptr)
{

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

} // initModule

llvm::Function *code_buffer::newFunction (llvm::FunctionType *fnTy, bool isFirst)
{
    this->_curFn = llvm::Function::Create (
	    fnTy,
	    isFirst ? llvm::GlobalValue::ExternalLinkage : llvm::GlobalValue::PrivateLinkage,
	    isFirst ? "main" : "",
	    this->_module);

    return this->_curFn;

}

// the extra arguments that are added to thread the state of the reserved
// registers through the control-flow graph.
static const int NumReservedRegArgs = 6;
static sml_reg_id ReservedRegArgs[NumReservedRegArgs] = {
	sml_reg_id::ALLOC_PTR,
	sml_reg_id::LIMIT_PTR,
	sml_reg_id::STORE_PTR,
	sml_reg_id::EXN_HNDLR,
	sml_reg_id::VAR_PTR,
	sml_reg_id::BASE_PTR
    };

// setup the argument/parameter lists for a fragment
Args_t code_buffer::setupFragArgs (CFG::frag *frag, Args_t &args)
{
    Args_t newArgs;

    newArgs.reserve (args.size() + NumReservedRegArgs);

  // add initial arguments for those reserved registers that are mapped to hardware registers
    for (int i = 0;  i < NumReservedRegArgs;  i++) {
	sml_reg_id id = ReservedRegArgs[i];
	if (this->_regInfo.info(id)->isMachineReg()) {
	    newArgs.push_back (this->_regState.get(id));
	}
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
	/* FIXME: need to allocate and initialize the overflow block */
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

