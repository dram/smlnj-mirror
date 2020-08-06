/// \file main.cxx
///
/// \copyright 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
/// All rights reserved.
///
/// \brief Main test driver for the code generator
///
/// \author John Reppy
///

#include <string>
#include <iostream>
#include <cstdlib>

#include "llvm/Support/TargetSelect.h"

#include "codegen.hxx"

void usage ()
{
    std::cerr << "usage: codegen [ -o | -S | -c ] <pkl-file>\n";
    exit (1);
}

int main (int argc, char **argv)
{
    output out = output::PrintAsm;
    std::string src;

    if (argc < 2) {
	usage();
    }

    if (argv[1][0] == '-') {
	if (argc != 3) {
	    usage();
	}
	std::string flag(argv[1]);
	if (flag == "-o") {
	    out = output::ObjFile;
	} else if (flag == "-S") {
	    out = output::AsmFile;
	} else if (flag == "-c") {
	    out = output::Memory;
	}
	else {
	    usage();
	}
	src = argv[2];
    }
    else if (argc == 2) {
	src = argv[1];
    }
    else {
	std::cerr << "usage: codegen [ -o | -S ] <pkl-file>\n";
	exit (1);
    }

    llvm::InitializeAllTargetInfos();
    llvm::InitializeAllTargets();
    llvm::InitializeAllTargetMCs();
    llvm::InitializeAllAsmParsers();
    llvm::InitializeAllAsmPrinters();

    codegen (src, out);

    return 0;

}
