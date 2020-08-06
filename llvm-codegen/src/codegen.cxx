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

static code_buffer *CodeBuf = nullptr;

void codegen (std::string const & src, output out)
{
    asdl::file_instream inS(src);

    std::cout << "read pickle ..." << std::flush;;
    CFG::comp_unit *cu = CFG::comp_unit::read (inS);

    std::cout << " setup code buffer ..." << std::flush;;
    if (CodeBuf == nullptr) {
	CodeBuf = code_buffer::create ("amd64");
    }

    std::cout << " generate llvm ..." << std::flush;;
    cu->codegen (CodeBuf);

    std::cout << " optimize ..." << std::flush;;
    CodeBuf->optimize ();

    std::cout << " done\n" << std::flush;;

    CodeBuf->dump ();

    if (! CodeBuf->verify ()) {
	std::cerr << "\nModule verified\n";
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
      case output::Memory:
	CodeBuf->compile ();
	break;
    }

    CodeBuf->endModule();

}
