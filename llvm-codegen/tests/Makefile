# Makefile for compiling the C++ test driver
#

SHELL =		/bin/sh

SML_ROOT =	../..
LLVM_DIR =	../llvm-debug

CXX =		clang++ --std=c++14
CPP_FLAGS =	-I../include -I$(SML_ROOT)/include -I$(LLVM_DIR)/include/ -I/opt/local/include
CXX_FLAGS =	-g
LD_FLAGS =	-L../../lib
LIBS =		-lasdl

LLVM_CONFIG =	$(LLVM_DIR)/bin/llvm-config

LLVM_LD_FLAGS =	$(shell $(LLVM_CONFIG) --ldflags)
LLVM_LIBS =	$(shell $(LLVM_CONFIG) --libs x86 aarch64 orcjit) \
		$(shell $(LLVM_CONFIG) --system-libs)

CODEGEN_OBJS =	$(wildcard ../build/*.o)
OBJS =		main.o $(CODEGEN_OBJS)

codegen:	$(OBJS)
	clang++ -o codegen $(LLVM_LD_FLAGS) $(LD_FLAGS) $(OBJS) $(LLVM_LD_FLAGS) $(LIBS) $(LLVM_LIBS)

%.o: %.cxx
	$(CXX) -c $(CPP_FLAGS) $(CXX_FLAGS) $<
