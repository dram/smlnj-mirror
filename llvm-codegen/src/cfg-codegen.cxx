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

  /***** code generation for the `value` type *****/

    llvm::Value *VAR::codegen (code_buffer & buf)
    {
    } // VAR::codegen

    llvm::Value *LABEL::codegen (code_buffer & buf)
    {
    } // LABEL::codegen

    llvm::Value *NUM::codegen (code_buffer & buf)
    {
    } // NUM::codegen

  /***** code generation for the `exp` type *****/

    llvm::Value *VALUE::codegen (code_buffer & buf)
    {
    } // VALUE::codegen

    llvm::Value *LOOKER::codegen (code_buffer & buf)
    {
	Args_t args;
	for (it = this->_v1.begin(); it != this->_v1.end(); ++it) {
	    args.push_back ((*it)->codegen (buf));
	}
	return this->_v0->codegen (buf, args);

    } // LOOKER::codegen

    llvm::Value *ARITH::codegen (code_buffer & buf)
    {
	Args_t args;
	for (it = this->_v1.begin(); it != this->_v1.end(); ++it) {
	    args.push_back ((*it)->codegen (buf));
	}
	return this->_v0->codegen (buf, args);

    } // ARITH::codegen

    llvm::Value *PURE::codegen (code_buffer & buf)
    {
	Args_t args;
	for (it = this->_v1.begin(); it != this->_v1.end(); ++it) {
	    args.push_back ((*it)->codegen (buf));
	}
	return this->_v0->codegen (buf, args);

    } // PURE::codegen

    llvm::Value *SELECT::codegen (code_buffer & buf)
    {
    } // SELECT::codegen

    llvm::Value *OFFSET::codegen (code_buffer & buf)
    {
    } // OFFSET::codegen

  /***** code generation for the `stm` type *****/

    void LET::codegen (code_buffer & buf)
    {
// record mapping from lvar to value
    } // LET::codegen

    void RECORD::codegen (code_buffer & buf)
    {
    } // RECORD::codegen

    void APP::codegen (code_buffer & buf)
    {
    } // APP::codegen

    void SWITCH::codegen (code_buffer & buf)
    {
    } // SWITCH::codegen

    void BRANCH::codegen (code_buffer & buf)
    {
    } // BRANCH::codegen

    void SETTER::codegen (code_buffer & buf)
    {
    } // SETTER::codegen

    void RCC::codegen (code_buffer & buf)
    {
    } // RCC::codegen


} // namespace CFG
