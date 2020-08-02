/// \file target-info.hxx
///
/// \copyright 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
/// All rights reserved.
///
/// \brief Information about the target architecture and how SML is mapped
///        onto it.
///
/// \author John Reppy
///

#ifndef _TARGET_INFO_HXX_
#define _TARGET_INFO_HXX_

#include "llvm/ADT/Triple.h"

#include <string>

#include "sml-registers.hxx"

struct target_info {
    std::string name;			// the target's name
    llvm::Triple::ArchType arch;	// LLVM's architecture specifier
    std::string dataLayout;		// LLVM data layout string
    int wordSz;				// size in bits of ML word (should also be the same as
					// the native pointer size)
    int numRegs;			// the number of SML registers used by the target
    int numCalleeSaves;			// the number of registers used for callee-save values
    bool usesBasePtr;			// true if the target needs the module base address
					// to compute code addresses.  Otherwise, we assume
					// PC-relative addressing is supported.
    int stkOffset[reg_info::NUM_REGS];	// byte offset from stack pointer to location where
					// the value is stored.  Will be non-zero only for SML
					// registers that are represented in memory.
    int callGCOffset;			// stack offset of call-gc entry address
    unsigned int allocSlopSzb;		// byte size of allocation slop

    static target_info const *InfoForTarget (std::string const &name);

};

#endif // !_TARGET_INFO_HXX_
