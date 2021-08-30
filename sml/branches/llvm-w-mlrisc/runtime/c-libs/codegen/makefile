#
# the makefile for the stub LLVM code generator library
#

SHELL =		/bin/sh

INCLUDES =	-I$(INC_DIR) -I$(CLIB_DIR) -I$(OBJS_DIR)

MAKE =		make
AR =		ar
ARFLAGS =	rcv
RANLIB =	ranlib

LIBRARY =	libcodegen.a

VERSION =	v-dummy

C_SRCS =	codegen-lib.c \
		generate.c

OBJS =		$(C_SRCS:%.c=%.o)

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

clean :
	rm -f v-* *.o $(LIBRARY)
