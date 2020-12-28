#!/bin/sh
#
# Script for running code generator tests
#
# usage: run.sh [ -emit-llvm ] [ -o | -S | -c ] [ files ... ]
#
#	--emit-llvm	-- dump the LLVM IR
#
# Target options (default is to print the assembler):
#
#	-o		-- generate ".o" files for targets
#	-S		-- generate ".s" files for targets
#	-c		-- compile to in-memory machine code and then print
#			   information about the code
#

ROOT=/Users/jhr/Work/smlnj/sml-llvm/

SML=$ROOT/bin/sml
CODEGEN=$ROOT/llvm-codegen/codegen/codegen

# check that the codegen executable exists
#
if [ ! -x $CODEGEN ] ; then
  echo "$0: codegen executable is missing"
  exit 1
fi

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

# if individual tests have not been specified, then run all the tests
#
if [ x"$FILES" = x ] ; then
  FILES="tst-*sml"
  RUN_ALL=yes
else
  RUN_ALL=no
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

#
# if we ran all of the tests and were generating .s or .o files, then
# we check and summarize the results
#

if [ $RUN_ALL = no ] ; then
  exit 0
fi

if [ x"$TARGET" = x-o ] ; then
  SUFFIX="o"
elif [ x"$TARGET" = x-S ] ; then
  SUFFIX="s"
else
  exit 0
fi

echo "# summary:"
for t in $FILES ; do
  stem=$(basename $t .sml)
  if [ -r $stem.$SUFFIX ] ; then
    STATUS="success"
  elif [ -r $stem.pkl ] ; then
    STATUS="codegen failed!!"
  else
    STATUS="sml error!!!!"
  fi
  echo "## $stem: $STATUS"
done

exit 0
