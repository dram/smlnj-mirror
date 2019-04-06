#SUN3
#MACHINE = M68
#DEFS =  -DBSD -DSUN3
#CFL = -n -Bstatic -f68881

#NEXT
#MACHINE = M68
#DEFS =  -DBSD -DMACH
#CFL =

#VAXBSD
#MACHINE = VAX
#DEFS = -DBSD
#CFL = 

#VAXV9
#MACHINE = VAX
#DEFS = -DV9
#CFL = 

DEFINES = -D$(MACHINE) $(DEFS)
#CFLAGS = -g $(CFL) $(DEFINES)
CFLAGS = -O $(CFL) $(DEFINES)

# Other possible #define's:
# PROFILE:  for a run which counts allocs.
# GCDEBUG:  print extra information during garbage collection,
#	    examine the to_space after a collection for stray pointers,
#	    and check create_b and create_s for zero allocation.
# GCPROFILE: for a run which prints out cumulative garbage collector
#	     information as it exits.

run: run.o gc.o callgc.o prim.o prof.o export.o objects.o cstruct.o trace.o allmo.o
	cc $(CFLAGS) -o run run.o gc.o callgc.o prim.o prof.o export.o objects.o cstruct.o trace.o allmo.o

allmo.o: linkdata
	linkdata >allmo.o

linkdata: linkdata.c
	cc -O -o linkdata linkdata.c

prim.s: $(MACHINE).prim.s tags.h prof.h ml.h prim.h
	/lib/cpp $(DEFINES) $(MACHINE).prim.s > prim.s

callgc.o objects.o gc.o run.o trace.o: descriptor.h tags.h

callgc.o run.o export.o objects.o: ml.h

prof.o: prof.h

clean:
	rm -f *.o lint.out prim.s linkdata

lint:
	lint $(DEFINES) run.c gc.c callgc.c prof.c export.c objects.c cstruct.c trace.c | tee lint.out
