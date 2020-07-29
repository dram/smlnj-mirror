/// \file codegen.cxx
///
/// \copyright 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
/// All rights reserved.
///
/// \brief Main test driver for the code generator
///
/// \author John Reppy
///

#include "asdl/asdl.hxx"
#include <iostream>
#include <cstdlib>

#include "llvm/Support/TargetSelect.h"

void codegen (asdl::instream &inS);

int main (int argc, char **argv)
{
    if (argc != 2) {
	std::cerr << "usage: codegen <pkl-file>\n";
	exit (1);
    }

    llvm::InitializeAllTargetInfos();
    llvm::InitializeAllTargets();
    llvm::InitializeAllTargetMCs();
    llvm::InitializeAllAsmParsers();
    llvm::InitializeAllAsmPrinters();

    asdl::file_instream inS(argv[1]);
    codegen (inS);

    return 0;

}
