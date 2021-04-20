/// \file overflow.cxx
///
/// \copyright 2021 The Fellowship of SML/NJ (http://www.smlnj.org)
/// All rights reserved.
///
/// \brief This file contains the CodeBuffer methods that support
/// signaling an Overflow exception.  We factor them out of the
/// code-buffer.cxx file because they are fairly complicated.
///
/// \author John Reppy
///

#include "code-buffer.hxx"
#include "target-info.hxx"
#include "cfg.hxx" // for argument setup

#include "llvm/IR/Constants.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/GlobalVariable.h"


// return the basic-block that contains the Overflow trap generator
// We also update the PHI nodes for the overflow basic block.
//
llvm::BasicBlock *code_buffer::getOverflowBB ()
{
    auto srcBB = this->_builder.GetInsertBlock ();
    int nArgs = this->_regInfo.numMachineRegs();

    if (this->_overflowBB == nullptr) {
      // if this is the first overflow BB for the module, then we need to define
      // the overflow function name
	if (this->_overflowFn == nullptr) {
	    this->_overflowFn = this->newFunction (
		this->_overflowFnTy, "raiseOverflow", false);
	}

	this->_overflowBB = this->newBB ("trap");
	this->_builder.SetInsertPoint (this->_overflowBB);
      // allocate PHI nodes for the SML special registers.  This is necessary
      // to ensure that any changes to the ML state (e.g., allocation) that
      // happened before the arithmetic operation are correctly recorded
        this->_overflowPhiNodes.reserve (nArgs);
    	Args_t args;
	args.reserve (nArgs);
	for (int i = 0;  i < nArgs;  ++i) {
	    llvm::Type *ty;
	    if (this->_regInfo.machineReg(i)->id() <= sml_reg_id::STORE_PTR) {
		ty = this->objPtrTy;
	    } else {
		ty = this->mlValueTy;
	    }
	    auto phi = this->_builder.CreatePHI(ty, 0);
	    this->_overflowPhiNodes.push_back(phi);
	    args.push_back(phi);
	}
      // create a call to the per-module overflow function
	this->createJWACall(this->_overflowFnTy, this->_overflowFn, args);
	this->_builder.CreateRetVoid ();

      // restore current basic block
	this->_builder.SetInsertPoint (srcBB);
    }

  // add PHI-node dependencies
    for (int i = 0;  i < nArgs;  ++i) {
	reg_info const *rInfo = this->_regInfo.machineReg(i);
	this->_overflowPhiNodes[i]->addIncoming(this->_regState.get (rInfo->id()), srcBB);
    }

    return this->_overflowBB;

} // code_buffer::getOverflowBB

// create the per-module overflow function (if necessary).  This function has
// just the special registers as arguments.  Its implementation is roughly:
//
//    raiseOverflow ()
//	overflowExn = sp[overflowExnSlot]
//      exn = ALLOC { overflowExn, UNIT, FILENAME :: NIL }
//	exnHndlr = gethdlr()
//	hndlrAddr = exnHndlr[0]
//	APPLY hndlrAddr(hndlrAddr, exhHndlr, UNIT, UNIT, UNIT, UNIT, exn)
//
void code_buffer::_createOverflowFn ()
{
    if (this->_overflowFn == nullptr) {
      // the module does not need an overflow function
	return;
    }

    this->_curFn = this->_overflowFn;

    auto bb = this->newBB ();
    this->_builder.SetInsertPoint (bb);

  // set up the incoming parameters for the overflow function, which are just
  // the hardware CMachine registers
    for (int i = 0, hwIx = 0;  i < reg_info::NUM_REGS;  ++i) {
	reg_info const *info = this->_regInfo.info(static_cast<sml_reg_id>(i));
	if (info->isMachineReg()) {
	    llvm::Argument *param = this->_curFn->getArg(hwIx++);
#ifndef NO_NAMES
	    param->setName (info->name());
#endif
	    this->_regState.set (info->id(), param);
	}
	else { // stack-allocated register
	    this->_regState.set (info->id(), nullptr);
	}
    }

  // define the filename as a statically allocated singleton list of a string constant.

// FIXME: we should be using getSourceFileName() here, but we currently don't set
// the file name when we create the module.
    std::string msg = "<" + this->_module->getModuleIdentifier() + ">";
  // the length of the message string including null termination and rounded up to
  // a word-size multiple of bytes
    uint64_t strLen = this->_target->roundToWordSz(msg.size() + 1);
    std::vector<uint8_t> bytes;
    bytes.reserve (strLen);
    for (int i = 0;  i < msg.size();  ++i) {
	bytes.push_back (static_cast<uint8_t>(msg[i]));
    }
  // add padding
    for (int i = msg.size();  i < strLen;  ++i) {
	bytes.push_back (0);
    }
  // the global that holds the traceback message for the exception
    auto msgGV = new llvm::GlobalVariable (
	llvm::ArrayType::get(this->i8Ty, strLen),
	true,
	llvm::GlobalValue::PrivateLinkage,
	llvm::ConstantDataArray::get (this->_context, bytes),
	"filenameData");

  // for initializing the constant arrays
    std::vector<llvm::Constant *> elts;

  // representation of a SML pair object as an array of integers
    auto pairObjTy = llvm::ArrayType::get(this->intTy, 3);

  // define the array header object
/* FIXME: eventually we should use the descriptor definitions from the runtime system */
    elts.reserve(3);
    elts.push_back (this->uConst(0x86)); // 8-bit vector header object
    elts.push_back (llvm::ConstantExpr::getPtrToInt (msgGV, this->intTy));
    elts.push_back (llvm::ConstantInt::get (this->intTy, 2*msg.size()+1));
    auto msgHdrObj = new llvm::GlobalVariable (
	pairObjTy,
	true,
	llvm::GlobalValue::PrivateLinkage,
	llvm::ConstantArray::get (pairObjTy, elts),
	"filenameHdr");

  // create the singleton list
    elts.clear();
    elts.reserve(3);
    elts.push_back (this->uConst(0x102)); // pair object
    elts.push_back (
	llvm::ConstantExpr::getPtrToInt (
	    llvm::ConstantExpr::getInBoundsGetElementPtr (
		this->i8Ty,
		llvm::ConstantExpr::getBitCast (msgHdrObj, this->bytePtrTy),
		llvm::ConstantInt::get (this->intTy, this->_wordSzB )),
	    this->intTy));
    elts.push_back (llvm::ConstantInt::get (this->intTy, 1)); // nil
    auto traceList = new llvm::GlobalVariable (
	pairObjTy,
	true,
	llvm::GlobalValue::PrivateLinkage,
	llvm::ConstantArray::get (pairObjTy, elts),
	"trace");

  // fetch the overflow exception from the stack
    Value *exnId = _loadFromStack (this->_target->overflowExnOffset, "Overflow");

  // generate code to allocate the exception packet
    Value *exn = this->allocRecord (
	0x182, // triple object
	{ exnId, this->uConst(1), traceList });

  // get the exception handler
    Value *exnHdlr = this->mlReg (sml_reg_id::EXN_HNDLR);

  // get the handler's code address
    Value *cp = this->createLoad (
	this->mlValueTy,
	this->createGEP (this->asObjPtr(exnHdlr), 0));

  // the function type of the handler
    int nArgs = this->_target->numCalleeSaves + 4; /* link, clos, cont, cs[], arg */
    std::vector<Type *> argTys;
    argTys.reserve (nArgs);
    for (int i = 0;  i < nArgs;  ++i) {
	argTys.push_back (this->mlValueTy);
    }
    auto handlerFnTy = this->createFnTy (frag_kind::STD_FUN, argTys);

  // set up the vector of arguments: cp, exnHdlr, unit, unit, unit, unit,
    Args_t args;
    arg_info info = this->_getArgInfo (frag_kind::STD_FUN);
    args.reserve (info.numArgs(1));
    this->_addExtraArgs (args, info);
    args.push_back (cp);
    args.push_back (exnHdlr);

  // the stdcont and callee-save registers are unused; we pass ML unit to avoid
  // confusing the GC with stale values
    for (int i = 0;  i < this->_target->numCalleeSaves + 1;  ++i) {
	args.push_back (this->unitValue());
    }
    args.push_back (exn);
    assert (args.size() == info.numArgs(nArgs) && "incorrect number of arguments");

  // call the handler
    this->createJWACall(
	handlerFnTy,
	this->build().CreateBitCast(cp, handlerFnTy->getPointerTo()),
	args);
    this->build().CreateRetVoid();

}

// get the branch-weight meta data for overflow branches
//
llvm::MDNode *code_buffer::overflowWeights ()
{
  // we use 1/1000 as the probability of an overflow
    return this->branchProb(1);

} // code_buffer::overflowWeights
