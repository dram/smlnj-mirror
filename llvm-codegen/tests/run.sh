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
TARGET=""
FLAGS=""
FILES=""
while [ "$#" != "0" ]; do
  arg=$1; shift
  case "$arg" in
    -o) TARGET="$arg" ;;
    -S) TARGET="$arg" ;;
    -c) TARGET="$arg" ;;
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

# remove existing files
#
for t in $FILES ; do
  stem=$(basename $t .sml)
  rm -rf $stem.pkl $stem.s $stem.o
done

for t in $FILES ; do
  base=$(basename $t .sml)
  echo "***** $base"
  echo "use \"$t\";" | $SML -Ccg.dump-cfg=true
  rm -f stdIn.pkl
  if [ -r $base.pkl ] ; then
    $CODEGEN $TARGET $FLAGS $base.pkl
  else
    echo "$0: $base.pkl is missing!!!"
    exit 1
  fi
done

if [ x"$TARGET" = x-o ] ; then
  SUFFIX="o"
elif [ x"$TARGET" = x-S ] ; then
  SUFFIX="S"
else
  exit 0
fi

#
# check results
#
echo "# summary:"
for t in $FILES ; do
  stem=$(basename $t .sml)
  if [ -r $stem.s ] ; then
    STATUS="success"
  elif [ -r $stem.pkl ] ; then
    STATUS="codegen failed!!"
  else
    STATUS="sml error!!!!"
  fi
  echo "## $stem: $STATUS"
done

exit 0
