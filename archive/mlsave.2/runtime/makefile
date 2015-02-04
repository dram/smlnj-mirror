M68FLAGS = -f68881
VAXFLAGS =
# $(VAXFLAGS) or $(M68FLAGS)
#FLAGS = $(VAXFLAGS)
FLAGS = $(M68FLAGS)

# VAX or M68
#MACHINE = VAX
MACHINE = M68

# BSD or V9
OPSYS = BSD

CFLAGS = -g -D$(OPSYS) -D$(MACHINE) $(FLAGS)

nml: run.o gc.o callgc.o prim.o prof.o export.o ieee.o
	cc $(CFLAGS) -o nml run.o gc.o callgc.o prim.o prof.o export.o ieee.o

ieee.o: ieee.s.$(MACHINE)
	as -o ieee.o ieee.s.$(MACHINE)

prim.o: prim.s.$(MACHINE)
	as -o prim.o prim.s.$(MACHINE)

clean:
	rm -f *.o
