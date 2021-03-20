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
#include "target-info.hxx"
#include <iostream>

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

bool setTarget (std::string const &target)
{
    if (CodeBuf != nullptr) {
	if (CodeBuf->targetInfo()->name == target) {
	    return false;
	}
	delete CodeBuf;
    }

    CodeBuf = code_buffer::create (target);

    return (CodeBuf == nullptr);

}

void codegen (std::string const & src, bool emitLLVM, bool dumpBits, output out)
{
    asdl::file_instream inS(src);

    assert (CodeBuf != nullptr && "call setTarget before calling codegen");

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
	    auto obj = CodeBuf->compile ();
	    if (obj) {
		obj->dump(dumpBits);
	    }
	} break;
    }

    CodeBuf->endModule();

}
