#!/bin/csh
#
# make an interactive ML
# current directory should be $MLCOMP (the main ml directory)
# $1 = object file directory (e.g. mo.m68 or mo.vax)
# $2 = main object file (IntM68 or IntVax)
# $3 = name of interactive ml (e.g sml)
#
rm -f mo
ln -s $1 mo
runtime/nml -g -h 5000 $2 >& diagnostics << input 
exportML "$3";
input
