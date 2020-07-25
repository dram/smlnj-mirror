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

/***** sml_registers methods *****/

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

sml_registers::sml_registers (struct target_info const *target)
{
    if (target == nullptr) {
	this->_nRegs = 0;
	this->_nSpecialRegs = 0;
	return;
    }

  // initialize the register info for the target
    this->_hasBaseReg = target->needsBasePtr;
    this->_nRegs = target->numRegs;
    for (int i = 0;  i < target->numRegs;  i++) {
	if (target->stkOffset[i] != 0) {
	    this->_info[i] = reg_info::createStkReg (target->stkOffset[i]);
	}
	else {
	    this->_info[i] = reg_info::createReg (i);
	}
    }
    for (int i = target->numRegs;  i < NUM_REGS;  i++) {
	this->_info[i] = nullptr;
    }

  // count the number of special registers
    int nSpecial = 0;
    for (int i = 0;  i < NumReservedRegArgs;  ++i) {
	if (this->info(ReservedRegArgs[i])->isMachineReg()) {
	    nSpecial++;
	}
    }

  // initialize the special register map
    this->_specialRegs = new sml_reg_id[nSpecial];
    this->_nSpecialRegs = nSpecial;
    for (int i = 0, j = 0;  i < NumReservedRegArgs;  ++i) {
	reg_info const *info = this->info(ReservedRegArgs[i]);
	if (info->isMachineReg()) {
	    this->_specialRegs[j++] = ReservedRegArgs[i];
	}
    }

}

/***** reg_state methods *****/

reg_state::reg_state (sml_registers const & info)
  : _nRegs (info.numRegs ())
{
  // we initialize all of the registers to nullptr
    for (int i = 0;  i < this->_nRegs;  i++) {
	this->_val[i] = nullptr;
    }
}

void reg_state::copyFrom (reg_state const & cache)
{
    this->_nRegs = cache._nRegs;
    for (int i = 0;  i < this->_nRegs;  i++) {
	this->_val[i] = cache._val[i];
    }

}
