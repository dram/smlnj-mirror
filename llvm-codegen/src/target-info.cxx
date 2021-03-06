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
	"x86-64",			// name
	"e-i64:64-n8:16:32:64-S128",	// LLVM data layout string
	64,				// wordSz
	18,				// numRegs
	3,				// numCalleeSaves
	true,				// hasPCRel
	{				// offsets for memory registers
	    0, 0, 0,			// ALLOC_PTR, LIMIT_PTR, STORE_PTR
	    8224, 8232		   	// EXN_HNDLR, VAR_PTR
	},
	8240,				// call-gc offset
	8*1024				// allocation slop
    };

static target_info AArch64Info = {
	"aarch64",			// name
	"e-m:o-i64:64-i128:128-n32:64-S128",
	64,				// wordSz
	29,				// numRegs
	3,				// numCalleeSaves
	true,				// hasPCRel
	{ 0, 0, 0, 0, 0 },		// no memory registers
	8240,				// call-gc offset *** FIXME ***
	8*1024				// allocation slop
    };

static target_info const *Targets[] = {
	&AMD64Info,
	&AArch64Info
    };

constexpr int kNumTargets = sizeof(Targets) / sizeof(target_info *);

target_info const *target_info::InfoForTarget (std::string const &name)
{
    for (int i = 0;  i < kNumTargets;  i++) {
	if (Targets[i]->name == name) {
	    return Targets[i];
	}
    }
    return nullptr;

}
