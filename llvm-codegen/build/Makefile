# Makefile for compiling the llvm code generator
#
# Note: we assume that you have gmp installed using macports (i.e., in /opt/local).

SHELL =		/bin/sh

SML_ROOT =	../..
LLVM_DIR =	../llvm-debug

LLVM_CONFIG =	$(LLVM_DIR)/bin/llvm-config
LLVM_CXX_FLAGS = $(shell $(LLVM_CONFIG) --cxxflags)

CXX =		clang++ --std=c++14
CPP_FLAGS =	-I../include -I$(LLVM_DIR)/include/
CXX_FLAGS =	-g $(LLVM_CXX_FLAGS)

VPATH =		../src

INCLS =		$(wildcard *hxx) $(wildcard ../include/*hxx) $(wildcard ../../include/asdl/*hxx)

SRCS =		$(wildcard ../src/*.cxx)

OBJS =		$(notdir $(SRCS:%.cxx=%.o))

llvm-codegen:	$(OBJS)

%.o: %.cxx $(INCLS)
	$(CXX) -c $(CPP_FLAGS) $(CXX_FLAGS) $<

.PHONY:		clean
clean:
		rm -rf $(OBJS)

