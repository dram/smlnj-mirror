#SUN3
#MACHINE = M68
#DEFS = -DBSD -DSUN3
#CFL = -n -Bstatic -f68881
#AS = as

#SONY
#MACHINE = M68
#DEFS =  -DBSD
#CFL =
#AS = /mnt/brown/toe/gas/a68  # the GNU assembler, that is

#SUN4
#MACHINE = SPARC
#DEFS = -DBSD
#CFL = -n -Bstatic
#AS = as

#ENCORE
#MACHINE = NS32
#DEFS =  -DBSD
#CFL =
#AS = as -j

#NEXT
#MACHINE = M68
#DEFS = -DBSD -DMACH
#CFL =
#AS = as

#VAXBSD
#MACHINE = VAX
#DEFS = -DBSD
#CFL = 
#AS = as

#VAXV9
#MACHINE = VAX
#DEFS = -DV9
#CFL = 
#AS = as

#
# this is needed by HPUX
#
SHELL=/bin/sh

DEFINES = -D$(MACHINE) $(DEFS)
#CFLAGS = -g $(CFL) $(DEFINES)
CFLAGS = -O $(CFL) $(DEFINES)

# Other possible #define's:
# GCDEBUG:  print extra information during garbage collection,
#	    examine the to_space after a collection for stray pointers,
#	    and check create_b and create_s for zero allocation.
# GCPROFILE: for a run which prints out cumulative garbage collector
#	     information as it exits.

MDEP	= $(MACHINE).dep

OBJS	= run.o run_ml.o callgc.o gc.o $(MDEP).o prim.o export.o timers.o \
	  ml_objects.o cfuns.o cstruct.o signal.o exncode.o malloc.o \
	  allmo.o

run:	$(OBJS)
	$(CC) $(CFLAGS) -o run $(OBJS)

#
# include file dependencies
#

run_ml.o callgc.o cfuns.o : cause.h

callgc.o cfuns.o signal.o $(MACHINE).dep.o : ml_os.h

run.o run_ml.o callgc.o cfuns.o ml_objects.o $(MACHINE).dep.o : ml_state.h

run.o run_ml.o allmo.o callgc.o gc.o ml_objects.o cstruct.o signal.o exncode.o \
    : ml_types.h tags.h

run_ml.o callgc.o signal.o $(MACHINE).dep.o prim.s : request.h

prim.s : tags.h

prim.o : $(MACHINE).prim.s tags.h
	 $(CPP) -DASM $(DEFINES) $(MACHINE).prim.s > prim.s
	 $(AS) -o prim.o prim.s

allmo.o : allmo.c

linkdata: linkdata.c tags.h
	$(CC) $(CFLAGS) -o linkdata linkdata.c

clean:
	rm -f *.o lint.out prim.s linkdata allmo.s run
