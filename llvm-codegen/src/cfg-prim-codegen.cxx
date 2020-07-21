///! \file cfg-prim-codegen.cxx
///
/// \copyright 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
/// All rights reserved.
///
/// \brief This file holds the implementations of the `codegen` methods
/// for the CFG primitive operations (defined in the `CFG_Prim` module).
///
/// \author John Reppy
///

#include "cfg.hxx"

namespace CFG_Prim {

  /***** code generation for the `alloc` type *****/
    llvm::Value *RECORD::codegen (code_buffer & buf, Args_t & args)
    {
    } // RECORD::codegen

    llvm::Value *RAW_RECORD::codegen (code_buffer & buf, Args_t & args)
    {
    } // RAW_RECORD::codegen

    llvm::Value *RAW_ALLOC::codegen (code_buffer & buf, Args_t & args)
    {
    } // RAW_ALLOC::codegen


  /***** code generation for the `arith` type *****/

    llvm::Value *ARITH::codegen (code_buffer & buf, Args_t & args)
    {
	llvm::Value *pair;
	switch (this->get_oper()) {
	    case arithop::IADD:
		pair = buf.build().CreateCall(
		    (this->get_sz() == 32) ? buf.sadd32WOvflw() : buf.sadd64WOvflw(),
		    args);
		break;

	    case arithop::ISUB:
		pair = buf.build().CreateCall(
		    (this->get_sz() == 32) ? buf.ssub32WOvflw() : buf.ssub64WOvflw(),
		    args);
		break;

	    case arithop::IMUL:
		pair = buf.build().CreateCall(
		    (this->get_sz() == 32) ? buf.smul32WOvflw() : buf.smul64WOvflw(),
		    args);
		break;

	    case arithop::IDIV:
	      // can trap on `minInt / ~1`, but the x86-64 hardware generates that trap,
	      // so we do not need to do anything special.  May want to add explicit
	      // test in the future.
		return buf.build().CreateSDiv (args[0], args[1]);

	    case arithop::IREM:
		return buf.build().CreateSRem (args[0], args[1]);
	}

	llvm::Value *res = buf.build().CreateExtractValue(pair, 0);
	llvm::Value *obit = buf.build().CreateExtractValue(pair, 1);
	llvm::BasicBlock *next = buf.newBB ();
	buf.build().CreateCondBr(obit, buf.getOverflowBB(), next, buf.overflowWeights());
      // switch to the new block for the continuation
	buf.setInsertPoint (next);

	return res;

    } // ARITH::codegen

    llvm::Value *TEST::codegen (code_buffer & buf, Args_t & args)
    {
    } // TEST::codegen

    llvm::Value *TESTU::codegen (code_buffer & buf, Args_t & args)
    {
    } // TESTU::codegen

    llvm::Value *REAL_TO_INT::codegen (code_buffer & buf, Args_t & args)
    {
    } // REAL_TO_INT::codegen


  /***** code generation for the `pure` type *****/

    llvm::Value *PURE_ARITH::codegen (code_buffer & buf, Args_t & args)
    {
	switch (this->get_oper()) {
	    case pureop::ADD:
		return buf.build().CreateAdd(args[0], args[1]);
	    case pureop::SUB:
		return buf.build().CreateSub(args[0], args[1]);
	    case pureop::SMUL:  // same as UMUL
	    case pureop::UMUL:
		return buf.build().CreateMul(args[0], args[1]);
	    case pureop::SDIV:
		return buf.build().CreateSDiv(args[0], args[1]);
	    case pureop::SREM:
		return buf.build().CreateSRem(args[0], args[1]);
	    case pureop::UDIV:
		return buf.build().CreateUDiv(args[0], args[1]);
	    case pureop::UREM:
		return buf.build().CreateURem(args[0], args[1]);
	    case pureop::LSHIFT:
		return buf.build().CreateShl(args[0], args[1]);
	    case pureop::RSHIFT:
		return buf.build().CreateAShr(args[0], args[1]);
	    case pureop::RSHIFTL:
		return buf.build().CreateLShr(args[0], args[1]);
	    case pureop::ORB:
		return buf.build().CreateOr(args[0], args[1]);
	    case pureop::XORB:
		return buf.build().CreateXor(args[0], args[1]);
	    case pureop::ANDB:
		return buf.build().CreateAnd(args[0], args[1]);
	    case pureop::FADD:
		return buf.build().CreateFAdd(args[0], args[1]);
	    case pureop::FSUB:
		return buf.build().CreateFSub(args[0], args[1]);
	    case pureop::FMUL:
		return buf.build().CreateFMul(args[0], args[1]);
	    case pureop::FDIV:
		return buf.build().CreateFDiv(args[0], args[1]);
	    case pureop::FNEG:
		break;
	    case pureop::FABS:
// TODO: call @llvm.fabs.f32 or @llvm.fabs.f64
		break;
	    case pureop::FSQRT:
// TODO: call @llvm.sqrt.f32 or @llvm.sqrt.f64
		break;
	} // switch

    } // PURE_ARITH::codegen

    llvm::Value *EXTEND::codegen (code_buffer & buf, Args_t & args)
    {
    } // EXTEND::codegen

    llvm::Value *INT_TO_REAL::codegen (code_buffer & buf, Args_t & args)
    {
    } // INT_TO_REAL::codegen

    llvm::Value *PURE_SUBSCRIPT::codegen (code_buffer & buf, Args_t & args)
    {
    } // PURE_SUBSCRIPT::codegen

    llvm::Value *PURE_RAW_SUBSCRIPT::codegen (code_buffer & buf, Args_t & args)
    {
    } // PURE_RAW_SUBSCRIPT::codegen


  /***** code generation for the `looker` type *****/

    llvm::Value *DEREF::codegen (code_buffer & buf, Args_t & args)
    {
	return buf.build().CreateLoad (buf.mlRefTy, args[0]);

    } // DEREF::codegen

    llvm::Value *SUBSCRIPT::codegen (code_buffer & buf, Args_t & args)
    {
	llvm::Value *offset = buf.build().CreateMul(args[1], buf.wordSzInBytes());
	llvm::Value *adr = buf.build().CreateAdd(args[0], offset);
	return buf.build().CreateLoad (buf.mlRefTy, adr);

    } // SUBSCRIPT::codegen

    llvm::Value *RAW_LOAD::codegen (code_buffer & buf, Args_t & args)
    {
    } // RAW_LOAD::codegen

    llvm::Value *RAW_SUBSCRIPT::codegen (code_buffer & buf, Args_t & args)
    {
    } // RAW_SUBSCRIPT::codegen

    llvm::Value *GET_HDLR::codegen (code_buffer & buf, Args_t & args)
    {
    } // GET_HDLR::codegen

    llvm::Value *GET_VAR::codegen (code_buffer & buf, Args_t & args)
    {
    } // GET_VAR::codegen


  /***** code generation for the `setter` type *****/
    void UNBOXED_UPDATE::codegen (code_buffer & buf, Args_t & args)
    {
    } // UNBOXED_UPDATE::codegen

    void UPDATE::codegen (code_buffer & buf, Args_t & args)
    {
    } // UPDATE::codegen

    void UNBOXED_ASSIGN::codegen (code_buffer & buf, Args_t & args)
    {
    } // UNBOXED_ASSIGN::codegen

    void ASSIGN::codegen (code_buffer & buf, Args_t & args)
    {
    } // ASSIGN::codegen

    void RAW_UPDATE::codegen (code_buffer & buf, Args_t & args)
    {
    } // RAW_UPDATE::codegen

    void RAW_STORE::codegen (code_buffer & buf, Args_t & args)
    {
    } // RAW_STORE::codegen

    void SET_HDLR::codegen (code_buffer & buf, Args_t & args)
    {
    } // SETHDLR::codegen

    void SET_VAR::codegen (code_buffer & buf, Args_t & args)
    {
    } // SETVAR::codegen


  /***** code generation for the `branch` type *****/
    llvm::Value *CMP::codegen (code_buffer & buf, Args_t & args)
    {
    } // CMP::codegen

    llvm::Value *FCMP::codegen (code_buffer & buf, Args_t & args)
    {
    } // FCMP::codegen

    llvm::Value *FSGN::codegen (code_buffer & buf, Args_t & args)
    {
    } // FSGN::codegen

    llvm::Value *PEQL::codegen (code_buffer & buf, Args_t & args)
    {
    } // PEQL::codegen

    llvm::Value *PNEQ::codegen (code_buffer & buf, Args_t & args)
    {
    } // PNEQ::codegen

    llvm::Value *STRNEQ::codegen (code_buffer & buf, Args_t & args)
    {
    } // STRNEQ::codegen


} // namespace CFG_Prim
