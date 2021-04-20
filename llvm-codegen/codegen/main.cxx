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
#include "llvm/IR/Value.h"

#include "codegen.hxx"

void usage ()
{
    std::cerr << "usage: codegen [ -o | -S | -c ] [ --emit-llvm ] [ --target <target> ] <pkl-file>\n";
    exit (1);
}

int main (int argc, char **argv)
{
    output out = output::PrintAsm;
    bool emitLLVM = false;
    bool dumpBits = false;
    std::string src = "";
    std::string targetArch = HOST_ARCH;

    if (argc < 2) {
	usage();
    }

    for (int i = 1;  i < argc;  i++) {
	if (argv[i][0] == '-') {
	    std::string flag(argv[i]);
	    if (flag == "-o") {
		out = output::ObjFile;
	    } else if (flag == "-S") {
		out = output::AsmFile;
	    } else if (flag == "-c") {
		out = output::Memory;
	    } else if (flag == "--emit-llvm") {
		emitLLVM = true;
	    } else if (flag == "--bits") {
		dumpBits = true;
	    } else if (flag == "--target") {
		i++;
		if (i < argc) {
		    targetArch = argv[i];
		} else {
		    usage();
		}
	    } else {
		usage();
	    }
	}
	else if ((i < argc-1) || (src != "")) {
	    std::cerr << "usage: codegen [ -o | -S | -c ] [ --emit-llvm ] [ --bits ] <pkl-file>\n";
	    exit (1);
	}
	else {
	    src = argv[i];
	}
    }

std::cout << "FunctionVal = " << llvm::Value::FunctionVal << "\n";
std::cout << "GlobalAliasVal = " << llvm::Value::GlobalAliasVal << "\n";
std::cout << "GlobalIFuncVal = " << llvm::Value::GlobalIFuncVal << "\n";
std::cout << "GlobalVariableVal = " << llvm::Value::GlobalVariableVal << "\n";
std::cout << "BlockAddressVal = " << llvm::Value::BlockAddressVal << "\n";
std::cout << "ConstantExprVal = " << llvm::Value::ConstantExprVal << "\n";
std::cout << "ConstantArrayVal = " << llvm::Value::ConstantArrayVal << "\n";
std::cout << "ConstantStructVal = " << llvm::Value::ConstantStructVal << "\n";
std::cout << "ConstantVectorVal = " << llvm::Value::ConstantVectorVal << "\n";
std::cout << "UndefValueVal = " << llvm::Value::UndefValueVal << "\n";
std::cout << "ConstantAggregateZeroVal = " << llvm::Value::ConstantAggregateZeroVal << "\n";
std::cout << "ConstantDataArrayVal = " << llvm::Value::ConstantDataArrayVal << "\n";
std::cout << "ConstantDataVectorVal = " << llvm::Value::ConstantDataVectorVal << "\n";
std::cout << "ConstantIntVal = " << llvm::Value::ConstantIntVal << "\n";
std::cout << "ConstantFPVal = " << llvm::Value::ConstantFPVal << "\n";
std::cout << "ConstantPointerNullVal = " << llvm::Value::ConstantPointerNullVal << "\n";
std::cout << "ConstantTokenNoneVal = " << llvm::Value::ConstantTokenNoneVal << "\n";
std::cout << "ArgumentVal = " << llvm::Value::ArgumentVal << "\n";
std::cout << "BasicBlockVal = " << llvm::Value::BasicBlockVal << "\n";
std::cout << "MetadataAsValueVal = " << llvm::Value::MetadataAsValueVal << "\n";
std::cout << "InlineAsmVal = " << llvm::Value::InlineAsmVal << "\n";
std::cout << "MemoryUseVal = " << llvm::Value::MemoryUseVal << "\n";
std::cout << "MemoryDefVal = " << llvm::Value::MemoryDefVal << "\n";
std::cout << "MemoryPhiVal = " << llvm::Value::MemoryPhiVal << "\n";
std::cout << "InstructionVal = " << llvm::Value::InstructionVal << "\n";

    llvm::InitializeAllTargetInfos();
    llvm::InitializeAllTargets();
    llvm::InitializeAllTargetMCs();
    llvm::InitializeAllAsmParsers();
    llvm::InitializeAllAsmPrinters();

    if (setTarget (targetArch)) {
	std::cerr << "codegen: unable to set target to \"" << targetArch << "\"\n";
	return 1;
    }

    codegen (src, emitLLVM, dumpBits, out);

    return 0;

}
