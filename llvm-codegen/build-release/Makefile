# Makefile for compiling the llvm code generator
#

SHELL =		/bin/sh

SML_ROOT =	../..
LLVM_DIR =	../llvm-release
LLVM_CONFIG =	$(LLVM_DIR)/bin/llvm-config
LLVM_TARGETS = $(shell $(LLVM_CONFIG) --targets-built)
LLVM_CXX_FLAGS = $(shell $(LLVM_CONFIG) --cxxflags)

CXX =		clang++ --std=c++14
CPP_FLAGS =	-DNDEBUG -DRELEASE_BUILD -I../include -I$(SML_ROOT)/include
CXX_FLAGS =	-g -O2 $(LLVM_CXX_FLAGS)

LLVM_CPP_FLAGS = $(shell $(LLVM_CONFIG) --cppflags)

VPATH =		../src

# determine the operating system
#
HOST_OS := $(shell uname -s)
ifeq ($(HOST_OS),Darwin)
TARGET_FLAGS += -DOPSYS_DARWIN
else ifeq ($(HOST_OS),Linux)
TARGET_FLAGS += -DOPSYS_LINUX
endif

# determine the supported targets
#
ifneq (,$(findstring AArch64,$(LLVM_TARGETS)))
TARGET_FLAGS +=	-DENABLE_AARCH64
endif
ifneq (,$(findstring X86,$(LLVM_TARGETS)))
TARGET_FLAGS +=	-DENABLE_X86
endif

INCLS =		$(wildcard ../include/*hxx) $(wildcard ../../include/asdl/*hxx)

SRCS =		$(wildcard ../src/*.cxx)

OBJS =		$(SRCS:%.cxx=%.o)

llvm-codegen:	$(OBJS)

%.o: %.cxx $(INCLS)
	$(CXX) -c $(CPP_FLAGS) $(TARGET_FLAGS) $(CXX_FLAGS) $<

.PHONY:		clean
clean:
		rm -rf $(OBJS)
