///! \file cfg-codegen.cxx
///
/// \copyright 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
/// All rights reserved.
///
/// \brief This file holds the implementations of the `codegen` methods
/// for the CFG expression and statement types (defined in the `CFG` module).
///
/// \author John Reppy
///

#include "cfg.hxx"

namespace CFG {

  /***** code generation for the `ty` type *****/

    Type *NUMt::codegen (code_buffer * buf)
    {
	return buf->iType (this->_v0);

    } // NUMt::codegen

    Type *FLTt::codegen (code_buffer * buf)
    {
	return buf->fType (this->_v0);

    } // FLTt::codegen

    Type *PTRt::codegen (code_buffer * buf)
    {
	return buf->mlValueTy;

    } // PTRt::codegen

    Type *FUNt::codegen (code_buffer * buf)
    {
	return buf->mlValueTy;

    } // FUNt::codegen

    Type *CNTt::codegen (code_buffer * buf)
    {
	return buf->mlValueTy;

    } // CNTt::codegen

  // code generation for a vector of types
    static std::vector<Type *> genTypes (code_buffer * buf, std::vector<ty *> const &tys)
    {
	std::vector<Type *> llvmTys;
	llvmTys.reserve(tys.size());
	for (auto ty : tys) {
	    llvmTys.push_back (ty->codegen (buf));
	}
	return llvmTys;
    }

  /***** code generation for the `exp` type *****/

    Value *VAR::codegen (code_buffer * buf)
    {
	return buf->lookupVal (this->_v0);

    } // VAR::codegen

    Value *LABEL::codegen (code_buffer * buf)
    {
	cluster *cluster = buf->lookupCluster (this->_v0);

	assert (cluster && "Unknown cluster label");

/* FIXME */return buf->uConst(42);
    } // LABEL::codegen

    Value *NUM::codegen (code_buffer * buf)
    {
	if (this->get_signed()) {
	    return buf->iConst (this->get_sz(), this->get_iv().toInt64());
	} else {
	    return buf->uConst (this->get_sz(), this->get_iv().toUInt64());
	}

    } // NUM::codegen

    Value *LOOKER::codegen (code_buffer * buf)
    {
	Args_t args;
	for (auto it = this->_v1.begin(); it != this->_v1.end(); ++it) {
	    args.push_back ((*it)->codegen (buf));
	}
	return this->_v0->codegen (buf, args);

    } // LOOKER::codegen

    Value *PURE::codegen (code_buffer * buf)
    {
	Args_t args;
	for (auto it = this->_v1.begin(); it != this->_v1.end(); ++it) {
	    args.push_back ((*it)->codegen (buf));
	}
	return this->_v0->codegen (buf, args);

    } // PURE::codegen

    Value *SELECT::codegen (code_buffer * buf)
    {
	Value *adr = buf->build().CreateInBoundsGEP (
	    buf->asObjPtr(this->_v1->codegen(buf)),
	    { buf->uConst(this->_v0) });
	return buf->createLoad (buf->mlValueTy, adr);

    } // SELECT::codegen

    Value *OFFSET::codegen (code_buffer * buf)
    {
	assert (false && "OFFSET");
/* FIXME */return nullptr;
    } // OFFSET::codegen

  /***** code generation for the `stm` type *****/

    void LET::codegen (code_buffer * buf)
    {
      // record mapping from the parameter to the compiled expression
	this->_v1->bind (buf, this->_v0->codegen(buf));
      // compile continuation
	this->_v2->codegen(buf);

    } // LET::codegen

    void CHK_GC::codegen (code_buffer * buf)
    {
/* FIXME */
      // compile continuation
	this->_v1->codegen(buf);

    } // CHK_GC::codegen

    void ALLOC::codegen (code_buffer * buf)
    {
	Args_t args;
	for (auto it = this->_v1.begin(); it != this->_v1.end(); ++it) {
#ifndef XXX
	    Value *v = (*it)->codegen (buf);
llvm::dbgs() << "ALLOC: type = "; v->getType()->print(llvm::dbgs(), true, true);
llvm::dbgs() << "; value = " << *v << "\n";
	    args.push_back (v);
#else
	    args.push_back ((*it)->codegen (buf));
#endif
	}
	buf->insertVal (this->_v2, this->_v0->codegen(buf, args));

      // compile continuation
	this->_v3->codegen(buf);

    } // ALLOC::codegen

    void APPLY::codegen (code_buffer * buf)
    {
	llvm::FunctionType *fnTy;
	Value *fn;
	LABEL *lab = dynamic_cast<LABEL *>(this->_v0);
	if (lab == nullptr) {
	    fnTy = buf->createFnTy (genTypes (buf, this->_v2));
	    fn = buf->build().CreateBitCast(
		this->_v0->codegen (buf),
		fnTy->getPointerTo());
	} else {
	    cluster *f = buf->lookupCluster (lab->get_0());
	    assert (f && "APPLY of unknown cluster");
	    fn = f->fn();
	    fnTy = f->fn()->getFunctionType();
	}

      // evaluate the arguments
	Args_t args = buf->createArgs (this->_v1.size());
	for (auto arg : this->_v1) {
	    args.push_back (arg->codegen (buf));
	}

	llvm::CallInst *call = buf->build().CreateCall(fnTy, fn, args);
	call->setCallingConv (llvm::CallingConv::JWA);
	call->setTailCallKind (llvm::CallInst::TCK_MustTail);

	buf->build().CreateRetVoid();

    } // APPLY::codegen

    void THROW::codegen (code_buffer * buf)
    {
	llvm::FunctionType *fnTy;
	Value *fn;
	LABEL *lab = dynamic_cast<LABEL *>(this->_v0);
	if (lab == nullptr) {
	    fnTy = buf->createFnTy (genTypes (buf, this->_v2));
	    fn = buf->build().CreateBitCast(
		this->_v0->codegen (buf),
		fnTy->getPointerTo());
	} else {
	    cluster *f = buf->lookupCluster (lab->get_0());
	    assert (f && "THROW of unknown cluster");
	    fn = f->fn();
	    fnTy = f->fn()->getFunctionType();
	}

      // evaluate the arguments
	Args_t args = buf->createArgs (this->_v1.size());
	for (auto arg : this->_v1) {
	    args.push_back (arg->codegen (buf));
	}

	llvm::CallInst *call = buf->build().CreateCall(fnTy, fn, args);
	call->setCallingConv (llvm::CallingConv::JWA);
	call->setTailCallKind (llvm::CallInst::TCK_MustTail);

	buf->build().CreateRetVoid();

    } // THROW::codegen

    void GOTO::codegen (code_buffer * buf)
    {
	llvm::BasicBlock *srcBB = buf->getCurBB();
	frag *dstFrag = buf->lookupFrag (this->_v0);

      // evaluate the arguments
	Args_t args = buf->createArgs (this->_v1.size());
	for (auto arg : this->_v1) {
	    args.push_back (arg->codegen (buf));
	}

      // generate the control transfer
	buf->build().CreateBr (dstFrag->bb());

      // add outgoing values as incoming values to the destination's
      // phi nodes
	buf->setInsertPoint (dstFrag->bb());
	for (int i = 0;  i < args.size();  ++i) {
	  // make sure that the type match!
	    Type *srcTy = args[i]->getType();
	    Type *tgtTy = dstFrag->paramTy(i);
	    if (srcTy != tgtTy) {
		dstFrag->addIncoming (i, buf->castTy(srcTy, tgtTy, args[i]), srcBB);
	    } else {
		dstFrag->addIncoming (i, args[i], srcBB);
	    }
	}

    } // GOTO::codegen

    void SWITCH::codegen (code_buffer * buf)
    {
      // the number of non-default cases; we use the last case as the default
	int nCases = this->_v1.size() - 1;

      // evaluate the argument
// FIXME: do we need to downcast to 32 bits?
	Value *arg = this->_v0->codegen(buf);

      // create the switch; note that we use the last case as the default
	llvm::SwitchInst *sw = buf->build().CreateSwitch(arg, this->_v1[nCases]->bb(), nCases);

      // add the cases to the switch
	for (int i = 0;  i < nCases;  i++) {
	    sw->addCase (buf->iConst(32, i), this->_v1[i]->bb());
	}

      // generate the code for the basic blocks
	reg_state saveRegs;
	buf->saveSMLRegState (saveRegs);
	for (auto it = this->_v1.begin();  it != this->_v1.end();  ++it) {
	    buf->restoreSMLRegState (saveRegs);
	    buf->setInsertPoint ((*it)->bb());
	    (*it)->codegen (buf);
	}

    } // SWITCH::codegen

    void BRANCH::codegen (code_buffer * buf)
    {
      // evaluate the test
	Args_t args;
	for (auto it = this->_v1.begin(); it != this->_v1.end(); ++it) {
	    args.push_back ((*it)->codegen (buf));
	}
	Value *cond = this->_v0->codegen(buf, args);

      // generate the conditional branch
	if (this->_v2 == 0) {
	  // no branch prediction
	    buf->build().CreateCondBr(cond, this->_v3->bb(), this->_v4->bb());
	} else {
	    buf->build().CreateCondBr(
		cond,
		this->_v3->bb(), this->_v4->bb(),
		buf->branchProb (this->_v2));
	}

      // generate code for the true branch
	reg_state saveRegs;
	buf->saveSMLRegState (saveRegs);
	buf->setInsertPoint (this->_v3->bb());
	this->_v3->codegen (buf);

      // generate code for the false branch
	buf->restoreSMLRegState (saveRegs);
	buf->setInsertPoint (this->_v4->bb());
	this->_v4->codegen (buf);

    } // BRANCH::codegen

    void ARITH::codegen (code_buffer * buf)
    {
	Args_t args;
	for (auto it = this->_v1.begin(); it != this->_v1.end(); ++it) {
	    args.push_back ((*it)->codegen (buf));
	}
      // record mapping from the parameter to the compiled expression
	this->_v2->bind (buf, this->_v0->codegen (buf, args));
      // compile continuation
	this->_v3->codegen(buf);

    } // ARITH::codegen

    void SETTER::codegen (code_buffer * buf)
    {
	Args_t args;
	for (auto it = this->_v1.begin(); it != this->_v1.end(); ++it) {
	    args.push_back ((*it)->codegen (buf));
	}
	this->_v0->codegen (buf, args);
      // compile continuation
	this->_v2->codegen(buf);

    } // SETTER::codegen

    void RCC::codegen (code_buffer * buf)
    {
    } // RCC::codegen


  /***** code generation for the `frag` type *****/

    void frag::codegen (code_buffer * buf, bool isEntry)
    {
	buf->beginFrag ();

	buf->setInsertPoint (this->_v_body->bb());

	if (isEntry) {
	    buf->setupStdEntry (this);
	} else {
	    buf->setupFragEntry (this, this->_phiNodes);
	}

      // generate code for the fragment
	this->_v_body->codegen (buf);

    } // frag::codegen


  /***** code generation for the `cluster` type *****/

    void cluster::codegen (code_buffer * buf, bool isFirst)
    {
	buf->beginCluster (this->_fn);

      // initialize the fragments for the cluster
	this->_v_entry->init (buf, true);
	for (auto it = this->_v_frags.begin();  it != this->_v_frags.end();  ++it) {
	    (*it)->init (buf, false);
	}

      // generate code for the cluster
	this->_v_entry->codegen (buf, true);
	for (auto it = this->_v_frags.begin();  it != this->_v_frags.end();  ++it) {
	    (*it)->codegen (buf, false);
	}

	buf->endCluster ();

    } // cluster::codegen


  /***** code generation for the `comp_unit` type *****/

    void comp_unit::codegen (code_buffer * buf)
    {
      // initialize the buffer for the comp_unit
	buf->beginModule (this->_v_srcFile, this->_v_fns.size() + 1);

      // initialize the clusters
	this->_v_entry->init (buf, true);
	for (auto f : this->_v_fns) {
	    f->init (buf, false);
	}

      // generate code
	this->_v_entry->codegen (buf, true);
	for (auto f : this->_v_fns) {
	    f->codegen (buf, false);
	}

    } // comp_unit::codegen

} // namespace CFG
