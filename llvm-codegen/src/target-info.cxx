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
	llvm::Triple::x86_64,
	"e-i64:64-n8:16:32:64-S128",	// LLVM data layout string
	64,				// wordSz
	18,				// numRegs
	3,				// numCalleeSaves
// TODO: change the following to false
	true,				// needsBasePtr
	{				// offsets for memory registers
	    0, 0, 0,			    // ALLOC_PTR, LIMIT_PTR, STORE_PTR
// FIXME: we are using the stack layout used for the MLRISC backend, it may have
// to be adjusted to be compatible with LLVM's stack layout conventions
	    40, 56, 32, 48		    // EXN_HNDLR, VAR_PTR, BASE_PTR, GC_LINK
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
