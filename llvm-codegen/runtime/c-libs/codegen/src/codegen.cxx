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

ml_val_t llvm_codegen (ml_state_t *msp, const char *src, const char *pkl, size_t szb)
{

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

/* FIXME: using a std::string here probably results in extra data copying */
    asdl::memory_instream inS (std::string (pkl, szb));

    std::cout << "read pickle ..." << std::flush;
    Timer pklTimer = Timer::start();
    CFG::comp_unit *cu = CFG::comp_unit::read (inS);
    std::cout << " " << pklTimer.msec() << "ms\n" << std::flush;

    std::cout << " generate llvm ..." << std::flush;;
    Timer genTimer = Timer::start();
    cu->codegen (CodeBuf);
    std::cout << " " << genTimer.msec() << "ms\n" << std::flush;

    if (! CodeBuf->verify ()) {
	std::cerr << "Module verified\n";
    }

    std::cout << " optimize ..." << std::flush;;
    Timer optTimer = Timer::start();
    CodeBuf->optimize ();
    std::cout << " " << optTimer.msec() << "ms\n" << std::flush;

    if (! CodeBuf->verify ()) {
	std::cerr << "Module verified after optimization\n";
    }

  // generate the in-memory object file
    auto obj = exitOnErr (CodeBuf->compile ());

  // identify the text section
    for (auto sect : obj->sections()) {
	if (sect.isText()) {
	    auto contents = exitOnErr (sect.getContents());
	    auto szb = contents.size();
	    if (szb > 0) {
		auto code = contents.data();
	      /* allocate the code object in the heap */
		auto codeObj = ML_AllocCode (msp, szb);
	      /* copy the code into the object */
		memcpy (PTR_MLtoC(void, codeObj), code, szb);
	      /* create a pair of the code object and entry-point offset */
		ml_val_t arr, res;
/* FIXME: it appears (from experimentation) that the entry-point offset is always
* zero, but we should probably be a bit more careful in the final version.
*/
		SEQHDR_ALLOC(msp, arr, DESC_word8arr, codeObj, szb);
		REC_ALLOC2(msp, res, arr, 0);
		return res;
	    }
	}
    }

    llvm::report_fatal_error ("unable to get code object", true);

  /* if we get here, then something is wrong, since there was no text segment */
    return ML_unit;

}
