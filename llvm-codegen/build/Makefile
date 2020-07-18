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

SRCS =		cfg.cxx

OBJS =		$(SRCS:%.cxx=%.o)

llvm-codegen:	$(OBJS)

%.o: %.cxx
	$(CXX) -c $(CPP_FLAGS) $(CXX_FLAGS) $<
