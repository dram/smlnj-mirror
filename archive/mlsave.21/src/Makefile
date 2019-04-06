#!/bin/csh
IMAGE=sml
STARTUP='(exportML "$(IMAGE)"; output std_out System.version; output std_out "\n");'
instructions:
	@echo Standard ML of New Jersey
	@echo For installation instructions, see doc/INSTALL.

sun:
	(cd runtime; rm -f makefile; ln makefile.m68 makefile; make clean)
	rm -f mo
	ln -s ../mo.m68 mo
#	(cd runtime; make linkdata)
#	runtime/linkdata mo/* >runtime/allmo.o
	(cd runtime; make)
	echo $(STARTUP) | runtime/run IntM68
	rm -f mo
	rm runtime/run

vaxultrix:
	(cd runtime; rm -f makefile; ln makefile.ult makefile; make clean)
	rm -f mo
	ln -s ../mo.vax mo
#	(cd runtime; make linkdata)
#	runtime/linkdata mo/* >runtime/allmo.o
	(cd runtime; make)
	echo $(STARTUP) | runtime/run IntVax
	rm -f mo
	rm runtime/run

vaxbsd:
	(cd runtime; rm -f makefile; ln makefile.bsd makefile; make clean)
	rm -f mo
	ln -s ../mo.vax mo
#	(cd runtime; make linkdata)
#	runtime/linkdata mo/* >runtime/allmo.o
	(cd runtime; make)
	echo $(STARTUP) | runtime/run IntVax
	rm -f mo
	rm runtime/run

vaxv9 install:
	(cd runtime; rm -f makefile; ln makefile.v9 makefile; make clean)
	rm -f mo
	ln -s ../mo.vax mo
#	(cd runtime; make linkdata)
#	runtime/linkdata mo/* >runtime/allmo.o
	(cd runtime; make)
	echo $(STARTUP) | runtime/run IntVax
	rm -f mo
	rm runtime/run
