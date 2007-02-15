#!/bin/sh

if [ $# -ge 1 ] ; then
    cd $1
fi

gf=svn://smlnj-gforge.cs.uchicago.edu/smlnj

here=`pwd`

for tree in config base smlnj-lib MLRISC ml-yacc trace-debug-profile pgraph \
            ckit cml eXene smlnj-c ml-burg ml-lex heap2asm nlffi \
	    ml-lpt
do
  if [ -d $tree -a -d $tree/.svn ] ; then
      echo Checking ${tree}...
      cd $tree
      svn status
      cd $here
  fi
done
