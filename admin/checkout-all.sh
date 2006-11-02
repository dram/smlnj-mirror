#!/bin/sh

if [ $# -ge 1 ] ; then
    mkdir $1
    cd $1
fi

gf=svn://smlnj-gforge.cs.uchicago.edu/smlnj

svn checkout $gf/config/trunk config
svn checkout $gf/sml/trunk base
svn checkout $gf/smlnj-lib/trunk smlnj-lib
svn checkout $gf/MLRISC/trunk MLRISC
svn checkout $gf/ml-yacc/trunk ml-yacc

svn checkout $gf/ckit/trunk ckit
svn checkout $gf/cml/trunk cml
svn checkout $gf/eXene/trunk eXene
svn checkout $gf/smlnj-c/trunk smlnj-c
svn checkout $gf/lexgen/trunk lexgen
svn checkout $gf/ml-burg/trunk ml-burg
svn checkout $gf/ml-lex/trunk ml-lex
svn checkout $gf/heap2asm/trunk heap2asm
svn checkout $gf/nlffi/trunk nlffi
