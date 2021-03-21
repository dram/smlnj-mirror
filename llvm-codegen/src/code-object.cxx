/*! \file code-object.cxx
 *
 * \author John Reppy
 */

/*
 * COPYRIGHT (c) 2021 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#include <iostream>
#include "target-info.hxx"
#include "code-object.hxx"
#include "llvm/Support/Error.h"

//==============================================================================

#ifdef ENABLE_AARCH64

/* include the correct header file for relocation-record definitions */
#if defined(OPSYS_DARWIN)
/* macOS uses MachO as it object-file format */
#define OBJFF_MACHO
#include "llvm/BinaryFormat/MachO.h"
#elif defined(OPSYS_LINUX)
#define OBJFF_ELF
#include "llvm/BinaryFormat/ELF.h"
#else
#  error unknown operating system
#endif

//! specialized CodeObject class for AMD64 target
//
class AArch64CodeObject : public CodeObject {
  public:
    AArch64CodeObject (
	target_info const *target,
	std::unique_ptr<llvm::object::ObjectFile> objFile
    ) : CodeObject(target, std::move(objFile))
    { }

  protected:
    bool _includeDataSect (llvm::object::SectionRef &sect);
    void _resolveRelocs (llvm::object::SectionRef &sect, uint8_t *code);
};

bool AArch64CodeObject::_includeDataSect (llvm::object::SectionRef &sect)
{
    assert (sect.isData() && "expected data section");

  // The ARM processor has hardware floating-point negation and absolute
  // value instructions, so we do not expect any data sections will need
  // to be included.
    return false;
}

// To support instruction patching, we define a union type for 32-bit words
// that includes packed struct types that represent the layout of instructions
// that we must patch with relocation information.
class AArch64InsnWord {
public:
    AArch64InsnWord (uint32_t w) { this->_w.w32 = w; }

    uint32_t value () { return this->_w.w32; }
    
    void patchHi21 (uint32_t v)
    {
	uint32_t hi21 = (v >> 11);  		// hi 21 bits of value
	this->_w.hi21.immlo = hi21 & 3;		// low 2 bits of hi21
	this->_w.hi21.immhi = hi21 >> 2;	// high 19 bits of hi21
    }
    
    void patchLo12 (uint32_t v)
    {
	this->_w.lo12.imm12 = (v & 0xfff);
    }
    
private:
    union {
        uint32_t w32;
	// instructions with 21-bit immediate values that represent the high
	// 21-bits of an offset.  (these are the "PC relative" instructions)
	//
	struct {
	    uint32_t op1 : 1;		// opcode bit
	    uint32_t immlo : 2;		// low two bits of immediate value
	    uint32_t op2 : 6;		// more opcode bits
	    uint32_t immhi : 19;	// high 19 bits of immediate value
	    uint32_t rd : 5;
	} hi21;
	// instructions with as 12-bit immediate value that is used for the
	// low bits of an offset.  (These include the add/sub immediate
	// instructions that are used to compute addresses)
	struct {
	    uint32_t op1 : 10;		// opcode bits
	    uint32_t imm12 : 12;	// 12-bit immediate value
	    uint32_t rn : 5;		// source register
	    uint32_t rd : 5;		// destination register
	} lo12;
    } _w;
};

// for the arm64, patching code is more challenging, because offsets are embedded
// in the instruction encoding and and the patching depends on the relocation
// type.
//
void AArch64CodeObject::_resolveRelocs (llvm::object::SectionRef &sect, uint8_t *code)
{
    for (auto reloc : sect.relocations()) {
      // the patch value; we ignore the relocation record if the symbol is not defined
	auto symb = reloc.getSymbol();
	if (sect.getObject()->symbols().end() != symb) {
          // the address to be patched (relative to the beginning of the file)
	    auto offset = reloc.getOffset();
	  // the patch value; we compute the offset relative to the address of
	  // byte following the patched location.
#if (LLVM_VERSION_MAJOR > 10) /* getValue returns an Expected<> value as of LLVM 11.x */
	    int32_t value = (int32_t)exitOnErr(symb->getValue()) - ((int32_t)offset + 4);
#else
	    int32_t value = (int32_t)symb->getValue() - ((int32_t)offset + 4);
#endif
	  // get the instruction to be patched
	    AArch64InsnWord instr(*(uint32_t *)(code + offset));
	    switch (reloc.getType()) {
#if defined(OBJFF_MACHO)
	    case llvm::MachO::ARM64_RELOC_PAGE21:
#elif defined(OBJFF_ELF)
	    case llvm::ELF::R_AARCH64_ADR_PREL_PG_HI21:
#endif
		instr.patchHi21 (value);
	    	break;
#if defined(OBJFF_MACHO)
	    case llvm::MachO::ARM64_RELOC_PAGEOFF12:
#elif defined(OBJFF_ELF)
	    case llvm::ELF::R_AARCH64_ADD_ABS_LO12_NC:
#endif
		instr.patchLo12 (value);
	    	break;
	    default:
	    	break;
	    }
	  // update the instruction with the patched version
	    *(uint32_t *)(code + offset) = instr.value();
	}
    }

}
#endif // ENABLE_AARCH64

//==============================================================================

#ifdef ENABLE_X86
//! specialized CodeObject class for AMD64 target
//
class AMD64CodeObject : public CodeObject {
  public:
    AMD64CodeObject (
	target_info const *target,
	std::unique_ptr<llvm::object::ObjectFile> objFile
    ) : CodeObject(target, std::move(objFile))
    { }

  protected:
    bool _includeDataSect (llvm::object::SectionRef &sect);
    void _resolveRelocs (llvm::object::SectionRef &sect, uint8_t *code);
};

bool AMD64CodeObject::_includeDataSect (llvm::object::SectionRef &sect)
{
    assert (sect.isData() && "expected data section");

    auto name = sect.getName();
/* FIXME: the following is object-file-format dependent */
  // the "__literal16" section has literals referenced by the code for
  // floating-point negation and absolute value
    return (name && name->equals("__literal16"));
}

// for the x86-64, patching the code is fairly easy, because the offset
// bytes are not embedded in the opcode part of the instruction.
//
void AMD64CodeObject::_resolveRelocs (llvm::object::SectionRef &sect, uint8_t *code)
{
    for (auto reloc : sect.relocations()) {
      // the patch value; we ignore the relocation record if the symbol is not defined
	auto symb = reloc.getSymbol();
	if (sect.getObject()->symbols().end() != symb) {
          // the address to be patched (relative to the beginning of the file)
	    auto offset = reloc.getOffset();
	  // the patch value; we compute the offset relative to the address of
	  // byte following the patched location.
#if (LLVM_VERSION_MAJOR > 10) /* getValue returns an Expected<> value as of LLVM 11.x */
	    int32_t value = (int32_t)exitOnErr(symb->getValue()) - ((int32_t)offset + 4);
#else
	    int32_t value = (int32_t)symb->getValue() - ((int32_t)offset + 4);
#endif
	  // update the offset one byte at a time (since it is not guaranteed to
	  // be 32-bit aligned)
	    for (int i = 0;  i < 4;  i++) {
		code[offset++] = value & 0xff;
		value >>= 8;
	    }
	}
    }

}

#endif // ENABLE_X86

//==============================================================================

// creation function for code objects
//
std::unique_ptr<CodeObject> CodeObject::create (
    target_info const *target,
    llvm::MemoryBufferRef objBuf)
{
    auto objFile = llvm::object::ObjectFile::createObjectFile (objBuf);
    if (objFile.takeError()) {
/* FIXME: error message */
	return std::unique_ptr<CodeObject>(nullptr);
    }

    switch (target->arch) {
#ifdef ENABLE_AARCH64
    case llvm::Triple::aarch64:
	return std::make_unique<AArch64CodeObject>(target, std::move(*objFile));
#endif
#ifdef ENABLE_X86
    case llvm::Triple::x86_64:
	return std::make_unique<AMD64CodeObject>(target, std::move(*objFile));
#endif
    default:
	assert (false && "unsupported architecture");
    }

}

CodeObject::~CodeObject ()
{
}

// copy the code into the specified memory
//
void CodeObject::getCode (uint8_t *code)
{
    for (auto sect : this->_sects) {
	auto contents = sect.getContents();
	if (contents.takeError()) {
	    std::cerr << "unable to get contents of section\n";
	    assert (0);
	}
	else {
	    auto szb = contents->size();
	    assert (sect.getSize() == szb && "inconsistent sizes");
	  /* copy the code into the object */
	    uint8_t *base = code + sect.getAddress();
	    memcpy (base, contents->data(), szb);
	  /* if the section is a text section, then resolve relocations */
	    if (sect.isText()) {
		this->_resolveRelocs (sect, base);
	    }
	}
    }
}

static llvm::ExitOnError exitOnErr;

void CodeObject::dump (bool bits)
{
  // print info about the sections
    llvm::dbgs() << "=== Sections ===\n";
    bool foundTextSect = false;
    llvm::object::SectionRef textSect;
    for (auto sect : this->_obj->sections()) {
	auto name = sect.getName();
	auto addr = sect.getAddress();
	auto sz = sect.getSize();
	if (name) {
	    llvm::dbgs() << "  " << *name;
	} else {
	    llvm::dbgs() << "  <section>";
	}
	if (sect.isText()) {
	    if (! foundTextSect) {
		textSect = sect;
		foundTextSect = true;
	    }
	    llvm::dbgs() << " [TEXT] ";
	}
	else if (sect.isData()) {
	    llvm::dbgs() << " [DATA] ";
	}
	llvm::dbgs() << " " << (void *)addr << ".." << (void *)(addr+sz) << "\n";
    }

  // print the symbols
    llvm::dbgs() << "=== Symbols ===\n";
    for (auto sym : this->_obj->symbols()) {
	auto name = sym.getName();
	auto addr = sym.getAddress();
	if (name && addr) {
	    llvm::dbgs() << "  " << *name << " @ " << (void *)*addr << "\n";
	}
    }

    if (bits && foundTextSect) {
      // dump the bits of the text section
	auto code = exitOnErr(textSect.getContents());
	const unsigned char *bytes = (const unsigned char *)code.data();
	llvm::dbgs () << "CONTENTS OF " << exitOnErr(textSect.getName()) << "\n";
	for (size_t i = 0;  i < code.size(); i += 16) {
	    size_t limit = std::min(i + 16, code.size());
	    llvm::dbgs () << "  " << llvm::format_hex_no_prefix(i, 4) << ": ";
	    for (int j = i;  j < limit;  j++) {
		llvm::dbgs() << " " << llvm::format_hex_no_prefix(bytes[j], 2);
	    }
	    llvm::dbgs () << "\n";
	}
      // dump relocation info
	llvm::dbgs () << "RELOCATION INFO\n";
	for (auto reloc : textSect.relocations()) {
	    auto offset = reloc.getOffset();
	    if (reloc.getSymbol() != this->_obj->symbols().end()) {
		auto symb = *(reloc.getSymbol());
		auto name = symb.getName();
		if (! name.takeError()) {
		    llvm::dbgs () << "  " << *name
			<< ": addr = " << llvm::format_hex(exitOnErr(symb.getAddress()), 10)
			<< "; value = "  << llvm::format_hex(symb.getValue(), 10)
			<< "; offset = " << llvm::format_hex(offset, 10)
// TODO: get the name associated with the type
			<< "; type = " << reloc.getType() << "\n";
		} else {
		    llvm::dbgs () << "  <unknown>: offset = "
			<< llvm::format_hex(offset, 10)
			<< "; type = " << reloc.getType() << "\n";
		}
	    }
	}
    }

}

//! internal helper function for computing the amount of memory required
//! for the code object.
//
size_t CodeObject::_computeSize ()
{
  // iterate over the sections in the object file and identify which ones
  // we should include in the result.  We also compute the size of the
  // concatenation of the sections.
  //
    size_t codeSzb = 0;
    for (auto sect : this->_obj->sections()) {
	if (this->_includeSect (sect)) {
	    this->_sects.push_back (sect);
	    uint64_t addr = sect.getAddress();
	    uint64_t szb = sect.getSize();
	    assert (codeSzb <= addr && "overlapping sections");
	    codeSzb = addr + szb;
	}
    }

  // check that we actual got something
    assert (codeSzb > 0 && "no useful sections in object file");

    return codeSzb;
}
