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
    Value *RECORD::codegen (code_buffer * buf, Args_t & args)
    {
	int len = args.size();
	Value *allocPtr = buf->mlReg (sml_reg_id::ALLOC_PTR);

      // write object descriptor
	buf->build().CreateAlignedStore (
	    buf->createIntToPtr(buf->uConst(this->_v_desc.toUInt64()), buf->mlValueTy),
	    allocPtr,
	    (unsigned)buf->wordSzInBytes());

      // initialize the object's fields
	for (int i = 1;  i <= len;  i++) {
	    Value *adr = buf->build().CreateInBoundsGEP (allocPtr, { buf->uConst(i) });
	    buf->build().CreateAlignedStore (
		buf->asMLValue (args[i-1]),
		adr,
		(unsigned)buf->wordSzInBytes());
	}

      // compute the object's address and cast it to an ML value
	Value *obj = buf->createBitCast (
	    buf->build().CreateInBoundsGEP (allocPtr, { buf->uConst(0) }),
	    buf->mlValueTy);

      // bump the allocation pointer
	buf->setMLReg (sml_reg_id::ALLOC_PTR,
	    buf->build().CreateInBoundsGEP (allocPtr, { buf->uConst(len + 1) }));

	return obj;

    } // RECORD::codegen

    Value *RAW_RECORD::codegen (code_buffer * buf, Args_t & args)
    {
	assert (false && "RAW_RECORD");
/* FIXME */return nullptr;
    } // RAW_RECORD::codegen

    Value *RAW_ALLOC::codegen (code_buffer * buf, Args_t & args)
    {
	assert (false && "RAW_ALLOC");
/* FIXME */return nullptr;
    } // RAW_ALLOC::codegen


  /***** code generation for the `arith` type *****/

    Value *ARITH::codegen (code_buffer * buf, Args_t & args)
    {
	Value *pair;
	switch (this->get_oper()) {
	    case arithop::IADD:
		pair = buf->build().CreateCall(
		    (this->get_sz() == 32) ? buf->sadd32WOvflw() : buf->sadd64WOvflw(),
		    args);
		break;

	    case arithop::ISUB:
		pair = buf->build().CreateCall(
		    (this->get_sz() == 32) ? buf->ssub32WOvflw() : buf->ssub64WOvflw(),
		    args);
		break;

	    case arithop::IMUL:
		pair = buf->build().CreateCall(
		    (this->get_sz() == 32) ? buf->smul32WOvflw() : buf->smul64WOvflw(),
		    args);
		break;

	    case arithop::IDIV:
	      // can trap on `minInt / ~1`, but the x86-64 hardware generates that trap,
	      // so we do not need to do anything special.  May want to add explicit
	      // test in the future.
		return buf->createSDiv (args[0], args[1]);

	    case arithop::IREM:
		return buf->createSRem (args[0], args[1]);
	}

	Value *res = buf->createExtractValue(pair, 0);
	Value *obit = buf->createExtractValue(pair, 1);
	llvm::BasicBlock *next = buf->newBB ();
	buf->build().CreateCondBr(obit, buf->getOverflowBB(), next, buf->overflowWeights());
      // switch to the new block for the continuation
	buf->setInsertPoint (next);

	return res;

    } // ARITH::codegen

    Value *REAL_TO_INT::codegen (code_buffer * buf, Args_t & args)
    {
	return buf->createFPToSI (args[0], buf->iType (this->_v_to));

    } // REAL_TO_INT::codegen


  /***** code generation for the `pure` type *****/

    Value *PURE_ARITH::codegen (code_buffer * buf, Args_t & args)
    {
	switch (this->get_oper()) {
	    case pureop::ADD:
		return buf->createAdd(args[0], args[1]);
	    case pureop::SUB:
		return buf->createSub(args[0], args[1]);
	    case pureop::SMUL:  // same as UMUL
	    case pureop::UMUL:
		return buf->createMul(args[0], args[1]);
	    case pureop::SDIV:
		return buf->createSDiv(args[0], args[1]);
	    case pureop::SREM:
		return buf->createSRem(args[0], args[1]);
	    case pureop::UDIV:
		return buf->createUDiv(args[0], args[1]);
	    case pureop::UREM:
		return buf->createURem(args[0], args[1]);
	    case pureop::LSHIFT:
		return buf->createShl(args[0], args[1]);
	    case pureop::RSHIFT:
		return buf->createAShr(args[0], args[1]);
	    case pureop::RSHIFTL:
		return buf->createLShr(args[0], args[1]);
	    case pureop::ORB:
		return buf->createOr(args[0], args[1]);
	    case pureop::XORB:
		return buf->createXor(args[0], args[1]);
	    case pureop::ANDB:
		return buf->createAnd(args[0], args[1]);
	    case pureop::FADD:
		return buf->createFAdd(args[0], args[1]);
	    case pureop::FSUB:
		return buf->createFSub(args[0], args[1]);
	    case pureop::FMUL:
		return buf->createFMul(args[0], args[1]);
	    case pureop::FDIV:
		return buf->createFDiv(args[0], args[1]);
	    case pureop::FNEG:
		return buf->createFNeg(args[0]);
	    case pureop::FABS:
		 return buf->build().CreateCall(
		    (this->get_sz() == 32) ? buf->fabs32() : buf->fabs64(),
		    args);
	    case pureop::FSQRT:
		 return buf->build().CreateCall(
		    (this->get_sz() == 32) ? buf->sqrt32() : buf->sqrt64(),
		    args);
	} // switch

    } // PURE_ARITH::codegen

    Value *EXTEND::codegen (code_buffer * buf, Args_t & args)
    {
	if (this->_v_signed) {
	    return buf->createSExt (args[0], buf->iType(this->_v_to));
	} else {
	    return buf->createZExt (args[0], buf->iType(this->_v_to));
	}

    } // EXTEND::codegen

    Value *INT_TO_REAL::codegen (code_buffer * buf, Args_t & args)
    {
	return buf->createSIToFP (args[0], buf->fType(this->_v_to));

    } // INT_TO_REAL::codegen

    Value *PURE_SUBSCRIPT::codegen (code_buffer * buf, Args_t & args)
    {
	Value *baseAdr = buf->asObjPtr(args[0]);
	Value *adr = buf->build().CreateGEP(baseAdr, buf->asInt(args[1]));
	return buf->build().CreateLoad (buf->mlValueTy, adr);

    } // PURE_SUBSCRIPT::codegen

    Value *PURE_RAW_SUBSCRIPT::codegen (code_buffer * buf, Args_t & args)
    {
	Type *elemTy = (this->_v_kind == numkind::INT
	    ? buf->iType(this->_v_sz)
	    : buf->fType(this->_v_sz));

	Value *adr = buf->build().CreateGEP (elemTy->getPointerTo(), args[0], args[1]);

	return buf->createLoad (elemTy, adr);

    } // PURE_RAW_SUBSCRIPT::codegen


  /***** code generation for the `looker` type *****/

    Value *DEREF::codegen (code_buffer * buf, Args_t & args)
    {
	return buf->build().CreateLoad (buf->mlValueTy, buf->asObjPtr(args[0]));

    } // DEREF::codegen

    Value *SUBSCRIPT::codegen (code_buffer * buf, Args_t & args)
    {
	Value *baseAdr = buf->asObjPtr(args[0]);
	Value *adr = buf->build().CreateGEP(baseAdr, buf->asInt(args[1]));
// QUESTION: should we mark the load as volatile?
	return buf->build().CreateLoad (buf->mlValueTy, adr);

    } // SUBSCRIPT::codegen

    Value *RAW_LOAD::codegen (code_buffer * buf, Args_t & args)
    {
	Type *elemTy = (this->_v_kind == numkind::INT
	    ? buf->iType(this->_v_sz)
	    : buf->fType(this->_v_sz));

      // RAW_LOAD assumes byte addressing, so we compute the address as a `char *`
      // and then bitcast to the desired pointer type for the load
	Value *adr =
	    buf->createPointerCast (
	        buf->build().CreateGEP (buf->bytePtrTy, args[0], args[1]),
		elemTy->getPointerTo());

// QUESTION: should we mark the load as volatile?
	return buf->createLoad (elemTy, adr);

    } // RAW_LOAD::codegen

    Value *RAW_SUBSCRIPT::codegen (code_buffer * buf, Args_t & args)
    {
	Type *elemTy = (this->_v_kind == numkind::INT
	    ? buf->iType(this->_v_sz)
	    : buf->fType(this->_v_sz));

	Value *adr = buf->build().CreateGEP (elemTy->getPointerTo(), args[0], args[1]);

// QUESTION: should we mark the load as volatile?
	return buf->createLoad (elemTy, adr);

    } // RAW_SUBSCRIPT::codegen

    Value *GET_HDLR::codegen (code_buffer * buf, Args_t & args)
    {
	return buf->mlReg (sml_reg_id::EXN_HNDLR);

    } // GET_HDLR::codegen

    Value *GET_VAR::codegen (code_buffer * buf, Args_t & args)
    {
	return buf->mlReg (sml_reg_id::VAR_PTR);

    } // GET_VAR::codegen


  /***** code generation for the `setter` type *****/
    void UNBOXED_UPDATE::codegen (code_buffer * buf, Args_t & args)
    {
    } // UNBOXED_UPDATE::codegen

    void UPDATE::codegen (code_buffer * buf, Args_t & args)
    {
    } // UPDATE::codegen

    void UNBOXED_ASSIGN::codegen (code_buffer * buf, Args_t & args)
    {
    } // UNBOXED_ASSIGN::codegen

    void ASSIGN::codegen (code_buffer * buf, Args_t & args)
    {
    } // ASSIGN::codegen

    void RAW_UPDATE::codegen (code_buffer * buf, Args_t & args)
    {
    } // RAW_UPDATE::codegen

    void RAW_STORE::codegen (code_buffer * buf, Args_t & args)
    {
    } // RAW_STORE::codegen

    void SET_HDLR::codegen (code_buffer * buf, Args_t & args)
    {
	buf->setMLReg (sml_reg_id::EXN_HNDLR, args[0]);

    } // SETHDLR::codegen

    void SET_VAR::codegen (code_buffer * buf, Args_t & args)
    {
	buf->setMLReg (sml_reg_id::VAR_PTR, args[0]);

    } // SETVAR::codegen


  /***** code generation for the `branch` type *****/

    static llvm::CmpInst::Predicate ICmpMap[] = {
	    llvm::ICmpInst::ICMP_SGT,	// signed GT
	    llvm::ICmpInst::ICMP_UGT,	// unsigned GT
	    llvm::ICmpInst::ICMP_SGE,	// signed GTE
	    llvm::ICmpInst::ICMP_UGE,	// unsigned GTE
	    llvm::ICmpInst::ICMP_SLT,	// signed LT
	    llvm::ICmpInst::ICMP_ULT,	// unsigned LT
	    llvm::ICmpInst::ICMP_SLE,	// signed LTE
	    llvm::ICmpInst::ICMP_ULE,	// unsigned LTE
	    llvm::ICmpInst::ICMP_EQ,	// (signed) EQL
	    llvm::ICmpInst::ICMP_EQ,	// (unsigned) EQL
	    llvm::ICmpInst::ICMP_NE,	// (signed) NEQ
	    llvm::ICmpInst::ICMP_NE	// (unsigned) NEQ
	};

    Value *CMP::codegen (code_buffer * buf, Args_t & args)
    {
	int idx = 2 * (static_cast<int>(this->_v_oper) - 1);
	if (this->_v_signed) {
	    idx += 1;
	}

	return buf->createICmp (ICmpMap[idx], buf->asInt(args[0]), buf->asInt(args[1]));

    } // CMP::codegen

    static llvm::CmpInst::Predicate FCmpMap[] = {
	    llvm::FCmpInst::FCMP_OEQ,	// F_EQ
	    llvm::FCmpInst::FCMP_UNE,	// F_ULG
	    llvm::FCmpInst::FCMP_ONE,	// F_UN
	    llvm::FCmpInst::FCMP_ORD,	// F_LEG
	    llvm::FCmpInst::FCMP_OGT,	// F_GT
	    llvm::FCmpInst::FCMP_OGE,	// F_GE
	    llvm::FCmpInst::FCMP_UGT,	// F_UGT
	    llvm::FCmpInst::FCMP_UGE,	// F_UGE
	    llvm::FCmpInst::FCMP_OLT,	// F_LT
	    llvm::FCmpInst::FCMP_OLE,	// F_LE
	    llvm::FCmpInst::FCMP_ULT,	// F_ULT
	    llvm::FCmpInst::FCMP_ULE,	// F_ULE
	    llvm::FCmpInst::FCMP_ONE,	// F_LG
	    llvm::FCmpInst::FCMP_UEQ	// F_UE
	};

    Value *FCMP::codegen (code_buffer * buf, Args_t & args)
    {
	return buf->createFCmp (FCmpMap[static_cast<int>(this->_v_oper) - 1], args[0], args[1]);

    } // FCMP::codegen

    Value *FSGN::codegen (code_buffer * buf, Args_t & args)
    {
      // bitcast to integer type of same size
	Value *asInt = buf->createBitCast (args[0], buf->iType (this->_v0));

	return buf->createICmpSLT(asInt, buf->iConst(this->_v0, 0));

    } // FSGN::codegen

    Value *PEQL::codegen (code_buffer * buf, Args_t & args)
    {
	return buf->createICmpEQ(args[0], args[1]);

    } // PEQL::codegen

    Value *PNEQ::codegen (code_buffer * buf, Args_t & args)
    {
	return buf->createICmpNE(args[0], args[1]);

    } // PNEQ::codegen

} // namespace CFG_Prim
