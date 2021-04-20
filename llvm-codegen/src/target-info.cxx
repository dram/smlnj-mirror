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

#if defined(OPSYS_DARWIN)
constexpr const char *kVendor = "apple";
constexpr const char *kOS = "macosx";
#elif defined(OPSYS_LINUX)
constexpr const char *kVendor = "unknown";
constexpr const char *kOS = "linuix";
#endif

static target_info AArch64Info = {
	"aarch64",			// name
	"e-m:o-i64:64-i128:128-n32:64-S128", // LLVM data layout string
	"sp",				// stack-pointer name
	llvm::Triple::aarch64,
	8, 64,				// word size in bytes and bits
	29,				// numRegs
	3,				// numCalleeSaves
	true,				// hasPCRel
	{ 0, 0, 0, 0, 0 },		// no memory registers
	8232,				// call-gc offset
	8224,				// overflow exception offset
	8*1024				// allocation slop
    };

static target_info X86_64Info = {
	"x86_64",			// official LLVM triple name
	"e-i64:64-n8:16:32:64-S128",	// LLVM data layout string
	"rsp",				// stack-pointer name
	llvm::Triple::x86_64,
	8, 64,				// word size in bytes and bits
	18,				// numRegs
	3,				// numCalleeSaves
	true,				// hasPCRel
	{				// offsets for memory registers
	    0, 0, 0,			// ALLOC_PTR, LIMIT_PTR, STORE_PTR
	    8224, 8232		   	// EXN_HNDLR, VAR_PTR
	},
	8240,				// call-gc offset
	-1,				// overflow exception offset *** FIXME ***
	8*1024				// allocation slop
    };

static target_info const *Targets[] = {
#ifdef ENABLE_X86
	&X86_64Info,
#endif
#ifdef ENABLE_AARCH64
	&AArch64Info,
#endif
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

llvm::Triple target_info::getTriple() const
{
    return llvm::Triple(this->name, kVendor, kOS);
}
