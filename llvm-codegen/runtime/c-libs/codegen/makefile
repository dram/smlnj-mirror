#
# the makefile for the LLVM code generator library
#

SHELL =		/bin/sh

SML_ROOT =      /Users/jhr/Work/smlnj/sml-llvm
LLVM_DIR =      $(SML_ROOT)/llvm-codegen/llvm-debug
RUNTIME_ROOT =	$(SML_ROOT)/llvm-codegen/runtime
CLIB_DIR =	$(RUNTIME_ROOT)/c-libs
INC_DIR =	$(RUNTIME_ROOT)/include
OBJS_DIR =	$(RUNTIME_ROOT)/objs

INCLUDES =	-I$(INC_DIR) -I$(CLIB_DIR) -I$(OBJS_DIR)

MAKE =		make
AR =		ar
ARFLAGS =	rcv
RANLIB =	ranlib

CXX =           clang++ --std=c++14
CPP_FLAGS =     $(INCLUDES) -Iinclude -I$(SML_ROOT)/include -I$(LLVM_DIR)/include/
CXX_FLAGS =     -g

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
	$(CXX) -c $(CPP_FLAGS) $(LLVM_CPP_FLAGS) $(DEFS) $(CXX_FLAGS) $<

clean :
	rm -f v-* *.o $(LIBRARY)
