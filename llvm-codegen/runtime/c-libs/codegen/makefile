#
# the makefile for the LLVM code generator library
#

SHELL =		/bin/sh

SML_ROOT =      /Users/jhr/Work/smlnj/sml-llvm
LLVM_DIR =      $(SML_ROOT)/llvm-codegen/llvm-release
RUNTIME_ROOT =	$(SML_ROOT)/llvm-codegen/runtime
CLIB_DIR =	$(RUNTIME_ROOT)/c-libs
INC_DIR =	$(RUNTIME_ROOT)/include
OBJS_DIR =	$(RUNTIME_ROOT)/objs

INCLUDES =	-I$(INC_DIR) -I$(CLIB_DIR) -I$(OBJS_DIR)

MAKE =		make
AR =		ar
ARFLAGS =	rcv
RANLIB =	ranlib

#
# LLVM configuration (LLVM_DIR should be passed in)
#
LLVM_CONFIG =	$(LLVM_DIR)/bin/llvm-config

LLVM_CXX_FLAGS = $(shell $(LLVM_CONFIG) --cxxflags)
LLVM_CPP_FLAGS = -DNDEBUG -DNO_NAMES -Iinclude $(INCLUDES)

CXX =           clang++ $(LLVM_CXX_FLAGS)

LIBRARY =	libcodegen.a

VERSION =	v-dummy

C_SRCS =	codegen-lib.c \
		generate.c

CXX_SRCS =	$(wildcard src/*.cxx)

OBJS =		$(C_SRCS:%.c=%.o) $(CXX_SRCS:src/%.cxx=%.o)

$(LIBRARY)	: $(VERSION) $(OBJS)
	rm -rf $(LIBRARY)
	$(AR) $(ARFLAGS) $(LIBRARY) $(OBJS)
	$(RANLIB) $(LIBRARY)

$(VERSION) :
	($(MAKE) MAKE="$(MAKE)" clean)
	echo "$(VERSION)" > $(VERSION)

.c.o:	$(INC_DIR)/ml-unixdep.h $(INC_DIR)/ml-base.h $(INC_DIR)/ml-values.h \
	$(INC_DIR)/ml-objects.h \
	$(CLIB_DIR)/ml-c.h cfun-proto-list.h cfun-list.h
	$(CC) $(CFLAGS) $(DEFS) $(INCLUDES) -Iinclude -c $<

%.o: src/%.cxx $(INCLS)
	$(CXX) -c $(LLVM_CPP_FLAGS) $(DEFS) $<

clean :
	rm -f v-* *.o $(LIBRARY)
