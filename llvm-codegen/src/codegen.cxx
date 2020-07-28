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
#include <iostream>

static code_buffer *CodeBuf = nullptr;

void codegen (asdl::instream &inS)
{
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

    CodeBuf->dumpAsm ("out.s");

    CodeBuf->endModule();

}
