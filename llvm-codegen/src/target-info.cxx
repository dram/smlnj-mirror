/// \file target-info.cxx
///
/// \copyright 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
/// All rights reserved.
///
/// \brief Implementation of target-specific information.
///
/// \author John Reppy
///

#include "target-info.hxx"

static target_info AMD64Info = {
	"amd64",			// name
	64,				// wordSz
	18,				// numRegs
	3,				// numCalleeSaves
// TODO: change the following to false
	true,				// needsBasePtr
	{				// offsets for memory registers
	    0, 0, 0,			    // ALLOC_PTR, LIMIT_PTR, STORE_PTR
// FIXME: we are using the stack layout used for the MLRISC backend, it may have
// to be adjusted to be compatible with LLVM's stack layout conventions
	    40, 56, 48, 32,		    // EXN_HNDLR, VAR_PTR, GC_LINK, BASE_PTR
	    0, 0, 0, 0,			    // STD_CONT, STD_ARG, STD_LINK, STD_CLOS
	    0, 0, 0, 0,			    // MISC_REG0, MISC_REG1, MISC_REG2, MISC_REG3
	    0, 0, 0			    // MISC_REG4 MISC_REG5, MISC_REG6
	}
    };

/* TODO: AArch64 */

#define NUM_TARGETS	1
static target_info const *Targets[NUM_TARGETS] = {
	&AMD64Info
    };

target_info const *target_info::InfoForTarget (std::string const &name)
{
    for (int i = 0;  i < NUM_TARGETS;  i++) {
	if (Targets[i]->name == name) {
	    return Targets[i];
	}
    }
    return nullptr;

}
