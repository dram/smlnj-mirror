# Makefile for compiling the llvm code generator
#
# Note: we assume that you have gmp installed using macports (i.e., in /opt/local).

SHELL =		/bin/sh

SML_ROOT =	../..

CXX =		clang++ --std=c++14
CPP_FLAGS =	-I../include -I$(SML_ROOT)/include -I../llvm/include/ -I/opt/local/include
CXX_FLAGS =	-g
LD_FLAGS =	-L../llvm/lib -L/opt/local/include

VPATH =		../src

INCLS =		$(wildcard ../include/*hxx) $(wildcard ../../include/asdl/*hxx)

SRCS =		cfg.cxx lambda-var.cxx \
		codegen.cxx code-buffer.cxx sml-registers.cxx target-info.cxx \
		cfg-init.cxx cfg-codegen.cxx cfg-prim-codegen.cxx

OBJS =		$(SRCS:%.cxx=%.o)

llvm-codegen:	$(OBJS)

%.o: %.cxx $(INCLS)
	$(CXX) -c $(CPP_FLAGS) $(CXX_FLAGS) $<

.PHONY:		clean
clean:
		rm -rf $(OBJS)

