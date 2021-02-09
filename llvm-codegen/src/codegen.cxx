/// \file codegen.cxx
///
/// \copyright 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
/// All rights reserved.
///
/// \brief Main code generator code.
///
/// \author John Reppy
///

#include "code-buffer.hxx"
#include "cfg.hxx"
#include "codegen.hxx"
#include <iostream>

#include "llvm/Support/Error.h"

// Some global flags for controlling the code generator.
// These are just for testing purposes
bool disableGC = false;

// timer support
#include <time.h>

class Timer {
  public:
    static Timer start ()
    {
	struct timespec ts;
	clock_gettime (CLOCK_REALTIME, &ts);
	return Timer (_cvtTimeSpec(ts));
    }
    void restart ()
    {
	struct timespec ts;
	clock_gettime (CLOCK_REALTIME, &ts);
	this->_ns100 = _cvtTimeSpec(ts);
    }
    double msec () const
    {
	struct timespec ts;
	clock_gettime (CLOCK_REALTIME, &ts);
	double t = double(_cvtTimeSpec(ts) - this->_ns100);
	return t / 10000.0;
    }
  private:
    uint64_t _ns100;	// track time in 100's of nanoseconds
    static uint64_t _cvtTimeSpec (struct timespec &ts)
    {
	return (
	    10000000 * static_cast<uint64_t>(ts.tv_sec)
	    + static_cast<uint64_t>(ts.tv_nsec) / 100);
    }
    Timer (uint64_t t) : _ns100(t) { }
};

static code_buffer *CodeBuf = nullptr;

static llvm::ExitOnError exitOnErr;

void codegen (std::string const & src, bool emitLLVM, bool dumpBits, output out)
{
    asdl::file_instream inS(src);

    if (CodeBuf == nullptr) {
	CodeBuf = code_buffer::create ("amd64");
    }

    std::cout << "read pickle ..." << std::flush;
    Timer pklTimer = Timer::start();
    CFG::comp_unit *cu = CFG::comp_unit::read (inS);
    std::cout << " " << pklTimer.msec() << "ms\n" << std::flush;

    std::cout << " generate llvm ..." << std::flush;;
    Timer genTimer = Timer::start();
    cu->codegen (CodeBuf);
    std::cout << " " << genTimer.msec() << "ms\n" << std::flush;

    if (emitLLVM) {
	CodeBuf->dump ();
    }

    if (! CodeBuf->verify ()) {
	std::cerr << "Module verified\n";
    }

    std::cout << " optimize ..." << std::flush;;
    Timer optTimer = Timer::start();
    CodeBuf->optimize ();
    std::cout << " " << optTimer.msec() << "ms\n" << std::flush;

//    if (emitLLVM) {
//	CodeBuf->dump ();
//    }

    if (! CodeBuf->verify ()) {
	std::cerr << "Module verified after optimization\n";
    }

  // get the stem of the filename
    std::string stem(src);
    auto pos = stem.rfind(".pkl");
    if (pos+4 != stem.size()) {
	stem = "out";
    }
    else {
	stem = stem.substr(0, pos);
    }

    switch (out) {
      case output::PrintAsm:
	CodeBuf->dumpAsm();
	break;
      case output::AsmFile:
	CodeBuf->dumpAsm (stem);
	break;
      case output::ObjFile:
	CodeBuf->dumpObj (stem);
	break;
      case output::Memory: {
	    auto obj = exitOnErr (CodeBuf->compile ());

/* TODO: eventually, we should change the return type of this function to
 *
 *	llvm::Expected<llvm::StringRef>
 *
 * and just return the contents of the text segment.  We might want to verify
 * that the entry function is at the beginning of the section.
 */

	  // print info about the sections
	    llvm::dbgs() << "=== Sections ===\n";
	    bool foundTextSect = false;
	    llvm::object::SectionRef textSect;
	    for (auto sect : obj->sections()) {
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
	    for (auto sym : obj->symbols()) {
		auto name = sym.getName();
		auto addr = sym.getAddress();
		if (name && addr) {
		    llvm::dbgs() << "  " << *name << " @ " << (void *)*addr << "\n";
		}
	    }

	    if (dumpBits && foundTextSect) {
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
		    if (reloc.getSymbol() != obj->symbols().end()) {
			auto symb = *(reloc.getSymbol());
			auto name = symb.getName();
			if (! name.takeError()) {
			    llvm::dbgs () << "  " << *name
				<< ": addr = " << llvm::format_hex(exitOnErr(symb.getAddress()), 10)
				<< "; value = "  << llvm::format_hex(symb.getValue(), 10)
				<< "; offset = " << llvm::format_hex(offset, 10)
				<< "; type = " << reloc.getType() << "\n";
			} else {
			    llvm::dbgs () << "  <unknown>: offset = "
				<< llvm::format_hex(offset, 10)
				<< "; type = " << reloc.getType() << "\n";
			}
		    }
		}
	    }

	} break;
    }

    CodeBuf->endModule();

}
