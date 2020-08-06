# Makefile for compiling the C++ test driver
#
# Note: we assume that you have gmp installed using macports (i.e., in /opt/local).

SHELL =		/bin/sh

SML_ROOT =	../..

CXX =		clang++ --std=c++14
CPP_FLAGS =	-I../include -I$(SML_ROOT)/include -I../llvm/include/ -I/opt/local/include
CXX_FLAGS =	-g
LD_FLAGS =	-L../../lib -L/opt/local/include
LIBS =		-lasdl

LLVM_LD_FLAGS =	$(shell ../llvm/bin/llvm-config --ldflags)
LLVM_LIBS =	$(shell ../llvm/bin/llvm-config --libs x86 aarch64 orcjit) \
		$(shell ../llvm/bin/llvm-config --system-libs)

CODEGEN_OBJS =	$(wildcard ../build/*.o)
OBJS =		main.o $(CODEGEN_OBJS)

codegen:	$(OBJS)
	clang++ -o codegen $(LLVM_LD_FLAGS) $(LD_FLAGS) $(OBJS) $(LLVM_LD_FLAGS) $(LIBS) $(LLVM_LIBS)

%.o: %.cxx
	$(CXX) -c $(CPP_FLAGS) $(CXX_FLAGS) $<
