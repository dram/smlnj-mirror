/// \file cfg-init.cxx
///
/// \copyright 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
/// All rights reserved.
///
/// \brief This file holds the implementation of the `init` methods
/// for the various CFG types (defined in the `CFG` module).
///
/// \author John Reppy
///

#include "cfg.hxx"

namespace CFG {

  /***** initialization for the `stm` type *****/

    void stm::_initBB (code_buffer * buf, bool blkEntry)
    {
	if (blkEntry) {
	    this->_bb = buf->newBB();
	} else {
	    this->_bb = nullptr;
	}
    }

    void LET::init (code_buffer * buf, bool blkEntry)
    {
	this->_initBB (buf, blkEntry);

      // continue initialization
	this->_v2->init (buf, false);

    } // LET::init

    void CHK_GC::init (code_buffer * buf, bool blkEntry)
    {
	this->_initBB (buf, blkEntry);

      // continue initialization
	this->_v1->init (buf, false);

    } // CHK_GC::init

    void ALLOC::init (code_buffer * buf, bool blkEntry)
    {
	this->_initBB (buf, blkEntry);

      // continue initialization
	this->_v3->init (buf, false);

    } // ALLOC::init

    void APPLY::init (code_buffer * buf, bool blkEntry)
    {
	this->_initBB (buf, blkEntry);

    } // APPLY::init

    void THROW::init (code_buffer * buf, bool blkEntry)
    {
	this->_initBB (buf, blkEntry);

    } // THROW::init

    void GOTO::init (code_buffer * buf, bool blkEntry)
    {
	this->_initBB (buf, blkEntry);

    } // GOTO::init

    void SWITCH::init (code_buffer * buf, bool blkEntry)
    {
	this->_initBB (buf, blkEntry);

      // initialize arms of switch
	for (auto it = this->_v1.begin();  it != this->_v1.end();  ++it) {
	    (*it)->init (buf, true);
	}

    } // SWITCH::init

    void BRANCH::init (code_buffer * buf, bool blkEntry)
    {
	this->_initBB (buf, blkEntry);

      // initialize arms of conditional
	this->_v3->init (buf, true);
	this->_v4->init (buf, true);

    } // BRANCH::init

    void ARITH::init (code_buffer * buf, bool blkEntry)
    {
	this->_initBB (buf, blkEntry);

      // continue initialization
	this->_v3->init (buf, false);

    } // ARITH::init

    void SETTER::init (code_buffer * buf, bool blkEntry)
    {
	this->_initBB (buf, blkEntry);

      // continue initialization
	this->_v2->init (buf, false);

    } // SETTER::init

    void RCC::init (code_buffer * buf, bool blkEntry)
    {
	this->_initBB (buf, blkEntry);

      // continue initialization
	this->_v_k->init (buf, false);

    } // RCC::init


  /***** initialization for the `frag` type *****/

    // for each fragment in the cluster, we add the mapping from the fragment's
    // label to it; we also initialize the fragment's body (which creates the
    // entry block for the fragment) and we add phi nodes to the block for each
    // of the parameters.
    //
    void frag::init (code_buffer * buf, bool isEntry)
    {
      // add the fragment to the label to fragment map
	buf->insertFrag (this->_v_lab, this);

      // define the LLVM function for this cluster
	std::vector<llvm::Type *> paramTys;
	paramTys.reserve (this->_v_params.size());
	for (auto param : this->_v_params) {
	    paramTys.push_back (param->get_1()->codegen(buf));
	}
	llvm::Function *fn = buf->newFunction (paramTys, isEntry);

      // set the calling convention to our "Jump-with-arguments" convention
	fn->setCallingConv (llvm::CallingConv::JWA);

      // assign attributes to the function
	 fn->addFnAttr (llvm::Attribute::Naked);

      // initialize the fragment's body */
	this->_v_body->init (buf, true);

      // add a phi node for each parameter of the fragment
	if (! isEntry) {
	    buf->setInsertPoint (this->_v_body->bb());
	    this->_phiNodes.reserve (this->_v_params.size());
	    for (auto it = this->_v_params.begin(); it != this->_v_params.end();  ++it) {
		llvm::Type *ty = (*it)->get_1()->codegen (buf);
		llvm::PHINode *phi = buf->build().CreatePHI(ty, 0);
		this->_phiNodes.push_back (phi);
	    }
	}

    }


  /***** initialization for the `cluster` type *****/

    void cluster::init (code_buffer * buf, bool isEntry)
    {
      // add the cluster to the cluster map
	buf->insertCluster (this->_v_entry->get_lab(), this);

      // create and record the LLVM function for the cluster

    }

} // namespace CFG
