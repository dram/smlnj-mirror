# Makefile for compiling the C++ test driver
#

SHELL =		/bin/sh

# determine the host architecture; we use the LLVM triple
# names, which are different from the names that llvm-config
# reports.
#
UNAME_ARCH :=	$(shell uname -m)
ifeq ($(UNAME_ARCH),x86_64)
HOST_ARCH="x86_64"
else ifeq ($(UNAME_ARCH),arm64)
HOST_ARCH="aarch64"
else
HOST_ARCH=unknown
endif

BUILD =		debug
#BUILD =		release

SML_ROOT =	../..
LLVM_DIR =	../llvm-$(BUILD)
LLVM_CONFIG =	$(LLVM_DIR)/bin/llvm-config
BUILD_DIR =	../build-$(BUILD)

CXX =		clang++ --std=c++14
CPP_FLAGS =	-I../include -I$(SML_ROOT)/include -I/opt/local/include
CXX_FLAGS =	-g
LD_FLAGS =

LLVM_TARGETS = $(shell $(LLVM_CONFIG) --targets-built)
LLVM_CPP_FLAGS := $(shell $(LLVM_CONFIG) --cppflags)
LLVM_LD_FLAGS := $(shell $(LLVM_CONFIG) --ldflags)
LLVM_LIBS :=	$(shell $(LLVM_CONFIG) --libs $(LLVM_TARGETS)) \
		$(shell $(LLVM_CONFIG) --system-libs)

INCLUDES =	$(wildcard ../include/*hxx) \
		$(wildcard ../include/asdl/*hxx) \
		$(wildcard $(BUILD_DIR)/*hxx)
CODEGEN_SRCS =	$(wildcard ../src/*.cxx)
CODEGEN_OBJS =	$(addprefix $(BUILD_DIR)/,$(notdir $(CODEGEN_SRCS:%.cxx=%.o)))
OBJS =		main.o $(CODEGEN_OBJS)

codegen:	main.o $(CODEGEN_SRCS) $(INCLUDES)
	(cd $(BUILD_DIR); $(MAKE) LLVM_DIR=$(LLVM_DIR))
	clang++ -o codegen $(LLVM_LD_FLAGS) $(LD_FLAGS) $(OBJS) $(LLVM_LD_FLAGS) $(LIBS) $(LLVM_LIBS)

%.o: %.cxx
	$(CXX) -c -DHOST_ARCH="\"$(HOST_ARCH)\"" $(LLVM_CPP_FLAGS) $(CPP_FLAGS) $(CXX_FLAGS) $<

.PHONY:		clean
clean:
		rm -rf $(OBJS) codegen
		(cd $(BUILD_DIR); make clean)
