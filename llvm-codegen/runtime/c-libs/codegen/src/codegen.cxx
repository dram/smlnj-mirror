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
#include "target-info.hxx"
#include <iostream>

#include "llvm/Support/TargetSelect.h"

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

bool_t setTarget (const char *target)
{
    if (CodeBuf != nullptr) {
	if (CodeBuf->targetInfo()->name == target) {
	    return false;
	}
	delete CodeBuf;
    } else {
      // initialize LLVM
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
    }

    CodeBuf = code_buffer::create (target);

    return (CodeBuf == nullptr);

}

ml_val_t llvm_codegen (ml_state_t *msp, const char *src, const char *pkl, size_t pklSzb)
{
    Timer totalTimer = Timer::start();

    Timer initTimer = Timer::start();
    if (CodeBuf == nullptr) {
/* FIXME: should be the host architecture */
	if (setTarget ("x86_64")) {
	    llvm::report_fatal_error ("initialization failure", true);
	}
    }
    double initT = initTimer.msec();

  // unpickle the CFG
    Timer unpklTimer = Timer::start();
/* FIXME: using a std::string here probably results in extra data copying */
    asdl::memory_instream inS (std::string (pkl, pklSzb));
    CFG::comp_unit *cu = CFG::comp_unit::read (inS);
    if (cu == nullptr) {
	llvm::report_fatal_error ("unable to unpickle code", true);
    }
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
    auto obj = CodeBuf->compile ();
    double objGenT = objGenTimer.msec();

    if (obj != nullptr) {
      // copy the sections to a heap-allocated code object.  At the very end, we add the
      // name of ths source file.  The name string is word-aligned, nul-terminated,
      // and padded to a multiple of the word size.  It is followed by a byte
      // specifying its length in words (if the name is longer than 255*WORD_SZB,
      // then we omit the source file name).  This layout must be consistent with
      // the function BO_GetCodeObjTag in runtime/gc/big-objects.c.
// FIXME: it does not make any sense to restrict the length field to a byte, since
// the code object is going to occupy a multiple of big-object pages in memory!!!

	Timer relocTimer = Timer::start();

	size_t codeSzb = obj->size();
        // first we round the code size up to a multiple of the word size
        size_t alignedCodeSzb = CodeBuf->roundToWordSzInBytes (codeSzb);
        // compute the padded size of the source-file name
        size_t srcFileLen = strlen(src);
        size_t paddedSrcFileLen = CodeBuf->roundToWordSzInBytes (srcFileLen + 1);
        if (paddedSrcFileLen > 255 * CodeBuf->wordSzInBytes()) {
            // if the file name is too long, which is unexpected, omit it
            paddedSrcFileLen = 0;
        }
        // size of code-object with extras
        size_t codeObjSzb = alignedCodeSzb      // code + alignment padding
            + paddedSrcFileLen                  // src name
            + 1;                                // src name length
	auto codeObj = ML_AllocCode (msp, codeObjSzb);
	obj->getCode (PTR_MLtoC(unsigned char, codeObj));
        // now add the source-file name to the end of the code object
        char *srcNameLoc = PTR_MLtoC(char, codeObj) + alignedCodeSzb;
        // copy the source-file name; note that `strncpy` pads with zeros
        strncpy (srcNameLoc + alignedCodeSzb, src, paddedSrcFileLen);
        // add the length in words at the end
        *reinterpret_cast<unsigned char *>(srcNameLoc + paddedSrcFileLen) =
            (paddedSrcFileLen / CodeBuf->wordSzInBytes());

	double relocT = relocTimer.msec();

#ifdef NDEBUG
  /* report stats */
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
    else {
	llvm::report_fatal_error ("unable to get code object", true);
    }

}
