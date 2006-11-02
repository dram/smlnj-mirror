#!/bin/sh

if [ $# -ge 1 ] ; then
    cd $1
fi

gf=svn://smlnj-gforge.cs.uchicago.edu/smlnj

here=`pwd`

for tree in config base smlnj-lib MLRISC ml-yacc \
            ckit cml eXene smlnj-c lexgen ml-burg ml-lex heap2asm nlffi \
            ml-antlr ml-ulex
do
  if [ -d $tree -a -d $tree/.svn ] ; then
      echo Refreshing ${tree}...
      cd $tree
      svn update
      cd $here
  fi
done
