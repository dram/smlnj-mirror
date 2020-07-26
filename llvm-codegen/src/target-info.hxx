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

#include <string>

#include "sml-registers.hxx"

struct target_info {
    std::string name;			// the target's name
    int wordSz;				// size in bits of ML word (should also be the same as
					// the native pointer size)
    int numRegs;			// the number of SML registers used by the target
    int numCalleeSaves;			// the number of registers used for callee-save values
    bool needsBasePtr;			// true if the target needs the module base address
					// to compute code addresses.  Otherwise, we assume
					// PC-relative addressing is supported.
    int stkOffset[NUM_REGS];		// will be non-zero for SML registers that are
					// represented by a slot in the stack frame.
    sml_reg_id argOrder[NUM_REGS];	// order of registers as used by the JWA calling
					// convention (see $LLVM/lib/Target/*/*CallingConv.td)

    static target_info const *InfoForTarget (std::string const &name);

};

#endif // !_TARGET_INFO_HXX_
