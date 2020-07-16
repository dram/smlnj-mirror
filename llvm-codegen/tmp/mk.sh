#!/bin/sh
#

LLVM_LIBS="-lLLVMCore -lLLVMRemarks -lLLVMBitstreamReader -lLLVMBinaryFormat -lLLVMSupport -lLLVMDemangle"

clang++ -std=c++17 -o test-metadata -I../llvm/include -L../llvm/lib $LLVM_LIBS -lcurses test-metadata.cxx

