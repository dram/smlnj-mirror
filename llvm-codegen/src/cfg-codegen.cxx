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

  // helper function for binding a llvm::Value to a CFG parameter
  //
    inline void insert (code_buffer * buf, param *param, llvm::Value *v)
    {
	buf->insertVal (param->get_0(), v);
    }

  /***** code generation for the `ty` type *****/

    llvm::Type *NUMt::codegen (code_buffer * buf)
    {
	return buf->iType (this->_v0);

    } // NUMt::codegen

    llvm::Type *FLTt::codegen (code_buffer * buf)
    {
	return buf->fType (this->_v0);

    } // FLTt::codegen

    llvm::Type *PTRt::codegen (code_buffer * buf)
    {
	return buf->mlPtrTy;

    } // PTRt::codegen

    llvm::Type *FUNt::codegen (code_buffer * buf)
    {
	return buf->mlPtrTy;

    } // FUNt::codegen

    llvm::Type *CNTt::codegen (code_buffer * buf)
    {
	return buf->mlPtrTy;

    } // CNTt::codegen


  /***** code generation for the `exp` type *****/

    llvm::Value *VAR::codegen (code_buffer * buf)
    {
	return buf->lookupVal (this->_v0);
    } // VAR::codegen

    llvm::Value *LABEL::codegen (code_buffer * buf)
    {
/* FIXME */return nullptr;
    } // LABEL::codegen

    llvm::Value *NUM::codegen (code_buffer * buf)
    {
	if (this->get_signed()) {
	    return buf->iConst (this->get_sz(), this->get_iv().toInt64());
	} else {
	    return buf->uConst (this->get_sz(), this->get_iv().toUInt64());
	}

    } // NUM::codegen

    llvm::Value *LOOKER::codegen (code_buffer * buf)
    {
	Args_t args;
	for (auto it = this->_v1.begin(); it != this->_v1.end(); ++it) {
	    args.push_back ((*it)->codegen (buf));
	}
	return this->_v0->codegen (buf, args);

    } // LOOKER::codegen

    llvm::Value *PURE::codegen (code_buffer * buf)
    {
	Args_t args;
	for (auto it = this->_v1.begin(); it != this->_v1.end(); ++it) {
	    args.push_back ((*it)->codegen (buf));
	}
	return this->_v0->codegen (buf, args);

    } // PURE::codegen

    llvm::Value *SELECT::codegen (code_buffer * buf)
    {
/* FIXME */return nullptr;
    } // SELECT::codegen

    llvm::Value *OFFSET::codegen (code_buffer * buf)
    {
/* FIXME */return nullptr;
    } // OFFSET::codegen

  /***** code generation for the `stm` type *****/

    void LET::codegen (code_buffer * buf)
    {
      // record mapping from the parameter to the compiled expression
	insert (buf, this->_v1, this->_v0->codegen(buf));
      // compile continuation
	this->_v2->codegen(buf);

    } // LET::codegen

    void ALLOC::codegen (code_buffer * buf)
    {
    } // ALLOC::codegen

    void APPLY::codegen (code_buffer * buf)
    {
    } // APPLY::codegen

    void THROW::codegen (code_buffer * buf)
    {
    } // THROW::codegen

    void GOTO::codegen (code_buffer * buf)
    {
	llvm::BasicBlock *srcBB = buf->getCurBB();
	frag *dstFrag = buf->lookupFrag (this->_v1);
// QUESTION: what about calling conventions; SML registers?

      // add outgoing values as incoming values to the destination's
      // phi nodes
	for (int i = 0;  i < this->_v2.size(); ++i) {
// FIXME: typecast around value
	    dstFrag->setIncoming (i, srcBB, this->_v2[i]->codegen (buf));
	}

    } // GOTO::codegen

    void SWITCH::codegen (code_buffer * buf)
    {
      // the number of non-default cases; we use the last case as the default
	int nCases = this->_v1.size() - 1;

      // evaluate the argument
// FIXME: do we need to downcast to 32 bits?
	llvm::Value *arg = this->_v0->codegen(buf);

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
	llvm::Value *cond = this->_v0->codegen(buf, args);

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
	insert (buf, this->_v2, this->_v0->codegen (buf, args));
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


  /***** code generation for the `entry` type *****/

    void entry::codegen (code_buffer * buf, Args_t &args)
    {

    } // entry::codegen


  /***** code generation for the `frag` type *****/

    void frag::codegen (code_buffer * buf)
    {
      // initialize the lvar to value map with the incoming parameters
	buf->clearValMap ();
	for (int i = 0;  i < this->_v_params.size();  i++) {
	    buf->insertVal (this->_v_params[i]->get_0(), this->_phiNodes[i]);
	}

      // generate code for the fragment
	buf->setInsertPoint (this->_v_body->bb());
	this->_v_body->codegen (buf);

    } // frag::codegen


  /***** code generation for the `cluster` type *****/

    void cluster::codegen (code_buffer * buf, bool isFirst)
    {
      // define the LLVM function for this cluster
	llvm::FunctionType *fnTy /* = ?? */;
	llvm::Function *fn = buf->newFunction (fnTy, isFirst);

      // set the calling convention to our "Jump-with-arguments" convention
	fn->setCallingConv (llvm::CallingConv::JWA);

      // assign attributes to the function
	 fn->addFnAttr (llvm::Attribute::Naked);

      // first we initialize the fragments for the cluster
	this->_v_entry->init (buf);
	for (auto it = this->_v_frags.begin();  it != this->_v_frags.end();  ++it) {
	    (*it)->init (buf);
	}

/* TODO setup function / function entry / ... */

    } // cluster::codegen

} // namespace CFG
