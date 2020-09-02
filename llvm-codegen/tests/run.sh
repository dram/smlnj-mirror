#!/bin/sh
#
# usage: run.sh [ -o | -S | -c ]
#

ROOT=/Users/jhr/Work/smlnj/sml-llvm/

SML=$ROOT/bin/sml
CODEGEN=$ROOT/llvm-codegen/codegen/codegen

if [ ! -x $CODEGEN ] ; then
  echo "$0: codegen executable is missing"
  exit 1
fi

case x"$1" in
  x-o) FLAG=$1 ;;
  x-S) FLAG=$1 ;;
  x-c) FLAG=$1 ;;
  *) FLAG="" ;;
esac

for t in tst-*sml ; do
  base=$(basename $t .sml)
  echo "***** $base"
  echo "use \"$t\";" | $SML -Ccg.dump-cfg=true
  rm -f stdIn.pkl
  if [ -r $base.pkl ] ; then
    $CODEGEN $FLAG $base.pkl
  else
    echo "$0: $base.pkl is missing!!!"
    exit 1
  fi
done

exit 0
