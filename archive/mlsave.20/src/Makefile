#!/bin/csh
HALFSIZE=5000
IMAGE=sml
STARTUP='(exportML "$(IMAGE)"; output std_out System.version; output std_out "\n");'
instructions:
	@echo Standard ML of New Jersey
	@echo For installation instructions, see doc/INSTALL.

sun:
	(cd runtime; rm -f makefile; ln makefile.m68 makefile; make; make clean; rm -f makefile)
	rm -f mo
	ln -s ../mo.m68 mo
	echo $(STARTUP) | runtime/run -h $(HALFSIZE) IntM68
	rm -f mo
	rm runtime/run

vaxultrix:
	(cd runtime; rm -f makefile; ln makefile.ult makefile; make; make clean; rm -f makefile)
	rm -f mo
	ln -s ../mo.vax mo
	echo $(STARTUP) | runtime/run -h $(HALFSIZE) IntVax
	rm -f mo
	rm runtime/run

vaxbsd:
	(cd runtime; rm -f makefile; ln makefile.bsd makefile; make; make clean; rm -f makefile)
	rm -f mo
	ln -s ../mo.vax mo
	echo $(STARTUP) | runtime/run -h $(HALFSIZE) IntVax
	rm -f mo
	rm runtime/run

vaxv9 install:
	(cd runtime; rm -f makefile; ln makefile.v9 makefile; make; make clean; rm -f makefile)
	rm -f mo
	ln -s ../mo.vax mo
	echo $(STARTUP) | runtime/run -h $(HALFSIZE) IntVax
	rm -f mo
	rm runtime/run
