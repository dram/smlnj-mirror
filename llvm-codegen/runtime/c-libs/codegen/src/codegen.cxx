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
#include "codegen.h"
#include <iostream>
#include <cstring>

#include "llvm/Support/TargetSelect.h"
#include "llvm/Object/ObjectFile.h"

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

/* patch the code object by resolving the relocation records
 * for the text segment.
 */
static void resolve_relocs (llvm::object::SectionRef &sect, unsigned char *code, size_t szb)
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
/* FIXME: we are assuming 32-bit patches and a little-endian target here */
	    for (int i = 0;  i < 4;  i++) {
		code[offset++] = value & 0xff;
		value >>= 8;
	    }
	}
    }

}

// should the contents of a section be included in the output object?
//
static bool includeSection (llvm::object::SectionRef sect)
{
    if (sect.isText()) return true;
    if (sect.isData()) {
	auto name = sect.getName();
/* FIXME: the following is both architecture and object-file-format dependent */
      // the "__literal16" section has literals referenced by the code for
      // floating-point negation and absolute value
	if (name && name->equals("__literal16")) return true;
    }
    return false;
}

ml_val_t llvm_codegen (ml_state_t *msp, const char *src, const char *pkl, size_t pklSzb)
{
    Timer totalTimer = Timer::start();

    Timer initTimer = Timer::start();
    if (CodeBuf == nullptr) {
#ifdef ALL_TARGETS
	llvm::InitializeAllTargetInfos ();
	llvm::InitializeAllTargets ();
	llvm::InitializeAllTargetMCs ();
	llvm::InitializeAllAsmParsers ();
	llvm::InitializeAllAsmPrinters ();
#else
	llvm::InitializeNativeTarget ();
	llvm::InitializeNativeTargetAsmParser ();
	llvm::InitializeNativeTargetAsmPrinter ();
#endif // ALL_TARGETS

	CodeBuf = code_buffer::create ("amd64");
    }
    double initT = initTimer.msec();

  // unpickle the CFG
    Timer unpklTimer = Timer::start();
/* FIXME: using a std::string here probably results in extra data copying */
    asdl::memory_instream inS (std::string (pkl, pklSzb));
    CFG::comp_unit *cu = CFG::comp_unit::read (inS);
    double unpklT = unpklTimer.msec();

  // generate LLVM
    Timer genTimer = Timer::start();
    cu->codegen (CodeBuf);
    double genT = genTimer.msec();

#ifdef VERIFY_LLVM
    Timer verifyTimer = Timer::start();
    if (CodeBuf->verify ()) {
	llvm::report_fatal_error ("LLVM verification error", true);
    }
    double verifyT = verifyTimer.msec();
#else
    double verifyT = 0.0;
#endif

  // optimize the LLVM code
    Timer optTimer = Timer::start();
    CodeBuf->optimize ();
    double optT = optTimer.msec();

#ifdef VERIFY_LLVM
    verifyTimer.restart();
    if (CodeBuf->verify ()) {
	llvm::report_fatal_error ("LLVM verification error after optimization", true);
    }
    verifyT += optTimer.msec();
#endif

  // generate the in-memory object file
    Timer objGenTimer = Timer::start();
    auto obj = exitOnErr (CodeBuf->compile ());
    double objGenT = objGenTimer.msec();

  // collect the sections to include
    Timer relocTimer = Timer::start();
    std::vector<llvm::object::SectionRef> sects;
    for (auto sect : obj->sections()) {
	if (includeSection (sect)) {
	    sects.push_back (sect);
	}
    }

  // check that we actual got something
    if (sects.size() == 0) {
/* TOD: we should map this back an SML exception */
	llvm::report_fatal_error ("unable to get code object", true);
    }

  // calculate the size of the combined code object
    uint64_t codeSzb = 0;
    for (auto sect : sects) {
	uint64_t addr = sect.getAddress();
	uint64_t szb = sect.getSize();
	assert (codeSzb <= addr && "overlapping sections");
	codeSzb = addr + szb;
    }

  // copy the sections to a heap-allocated code object
    auto codeObj = ML_AllocCode (msp, codeSzb);
    for (auto sect : sects) {
	auto contents = exitOnErr (sect.getContents());
	auto szb = contents.size();
	assert (sect.getSize() == contents.size() && "inconsistent sizes");
      /* copy the code into the object */
	Byte_t *base = PTR_MLtoC(unsigned char, codeObj) + sect.getAddress();
	memcpy (base, contents.data(), szb);
      /* if the section is a text section, then resolve relocations */
	resolve_relocs (sect, base, szb);
    }

#ifdef NDEBUG
  /* report stats */
    double relocT = relocTimer.msec();
llvm::dbgs() << "\"" << src << "\"," << pklSzb << "," << codeSzb << ","
    << initT << "," << unpklT << "," << genT << "," << optT << "," << verifyT << "," << relocT
    << "," << totalTimer.msec() << "\n";
#endif

  /* create a pair of the code object and entry-point offset */
    ml_val_t arr, res;
/* FIXME: it appears (from experimentation) that the entry-point offset is always
 * zero, but we should probably be a bit more careful in the final version.
 */
    SEQHDR_ALLOC(msp, arr, DESC_word8arr, codeObj, codeSzb);
    REC_ALLOC2(msp, res, arr, 0);
    return res;

}
