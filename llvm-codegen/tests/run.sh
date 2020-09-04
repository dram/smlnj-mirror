#!/bin/sh
#
# usage: run.sh [ -emit-llvm ] [ -o | -S | -c ] [ files ... ]
#

ROOT=/Users/jhr/Work/smlnj/sml-llvm/

SML=$ROOT/bin/sml
CODEGEN=$ROOT/llvm-codegen/codegen/codegen

if [ ! -x $CODEGEN ] ; then
  echo "$0: codegen executable is missing"
  exit 1
fi


#
# Process command line arguments
#
FLAGS=""
FILES=""
while [ "$#" != "0" ]; do
  arg=$1; shift
  case "$arg" in
    -o) FLAGS="$FLAGS $arg" ;;
    -S) FLAGS="$FLAGS $arg" ;;
    -c) FLAGS="$FLAGS $arg" ;;
    -emit-llvm) FLAGS="$FLAGS $arg" ;;
    -*) echo "$0: unrecognized flag $arg"
        exit 1 ;;
    *) FILES="$FILES $arg" ;;
  esac
done

if [ x"$FILES" = x ] ; then
  FILES="tst-*sml"
  echo "files: $FILES"
fi

for t in $FILES ; do
  base=$(basename $t .sml)
  echo "***** $base"
  echo "use \"$t\";" | $SML -Ccg.dump-cfg=true
  rm -f stdIn.pkl
  if [ -r $base.pkl ] ; then
    $CODEGEN $FLAGS $base.pkl
  else
    echo "$0: $base.pkl is missing!!!"
    exit 1
  fi
done

exit 0
