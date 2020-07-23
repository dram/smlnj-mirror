/// \file sml-registers.cxx
///
/// \copyright 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
/// All rights reserved.
///
/// \brief Implementation of methods for the classes defined in "sml-registers.hxx"
///
/// \author John Reppy
///

#include "sml-registers.hxx"
#include "target-info.hxx"

/***** reg_state methods *****/

reg_state::reg_state (sml_registers const *info)
  : _nRegs (info->numRegs ())
{
  // we initialize all of the registers to nullptr
    for (int i = 0;  i < this->_nRegs;  i++) {
	this->_val[i] = nullptr;
    }
}

void reg_state::copyFrom (reg_state const &cache)
{
    this->_nRegs = cache._nRegs;
    for (int i = 0;  i < this->_nRegs;  i++) {
	this->_val[i] = cache._val[i];
    }

}
