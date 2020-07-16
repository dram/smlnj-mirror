# Makefile for compiling the llvm code generator
#

SHELL =		/bin/sh

SML_ROOT =	../..

CXX =		clang++ --std=c++14
CPP_FLAGS =	-I../include -I$(SML_ROOT)/include -I../llvm/include/
CXX_FLAGS =	-g
LD_FLAGS =

VPATH =		../src

SRCS =		cfg.cxx

OBJS =		$(SRCS:%.cxx=%.o)

llvm-codegen:	$(OBJS)

%.o: %.cxx
	$(CXX) -c $(CPP_FLAGS) $(CXX_FLAGS) $<
