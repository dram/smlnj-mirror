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

    void LET::init (code_buffer & buf, bool blkEntry)
    {
	_initBB (buf, blkEntry);

      // continue initialization
	this->_v2->init (buf, false);

    } // LET::init

    void ALLOC::init (code_buffer & buf, bool blkEntry)
    {
	_initBB (buf, blkEntry);

      // continue initialization
	this->_v3->init (buf, false);

    } // ALLOC::init

    void APPLY::init (code_buffer & buf, bool blkEntry)
    {
	_initBB (buf, blkEntry);

    } // APPLY::init

    void THROW::init (code_buffer & buf, bool blkEntry)
    {
	_initBB (buf, blkEntry);

    } // THROW::init

    void GOTO::init (code_buffer & buf, bool blkEntry)
    {
	_initBB (buf, blkEntry);

    } // GOTO::init

    void SWITCH::init (code_buffer & buf, bool blkEntry)
    {
	_initBB (buf, blkEntry);

      // initialize arms of switch
	for (auto it = this->_v1.begin();  it != this->_v1.end();  ++it) {
	    (*it)->init (buf, true);
	}

    } // SWITCH::init

    void BRANCH::init (code_buffer & buf, bool blkEntry)
    {
	_initBB (buf, blkEntry);

      // initialize arms of conditional
	this->_v3->init (buf, true);
	this->_v4->init (buf, true);

    } // BRANCH::init

    void ARITH::init (code_buffer & buf, bool blkEntry)
    {
	_initBB (buf, blkEntry);

      // continue initialization
	this->_v3->init (buf, false);

    } // ARITH::init

    void SETTER::init (code_buffer & buf, bool blkEntry)
    {
	_initBB (buf, blkEntry);

      // continue initialization
	this->_v2->init (buf, false);

    } // SETTER::init

    void RCC::init (code_buffer & buf, bool blkEntry)
    {
	_initBB (buf, blkEntry);

      // continue initialization
	this->_v_k->init (buf, false);

    } // RCC::init


  /***** initialization for the `entry` type *****/

    void entry::init (code_buffer &buf)
    {
      // initialize the entry fragment's body */
	this->_v_body->init (buf, true);
    }

  /***** initialization for the `frag` type *****/

    void frag::init (code_buffer &buf)
    {
      // initialize the fragment's body */
	this->_v_body->init (buf, true);
    }

} // namespace CFG
