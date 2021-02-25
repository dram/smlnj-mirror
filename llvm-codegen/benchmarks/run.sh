#!/bin/ksh
#
# usage:
#	run.sh [ --llvm ] prog
#

unalias echo

NCOMPS=5
NRUNS=10

if [ x"$1" = "x--llvm" ] ; then
  LLVM=yes
  SML="../../bin/sml -Ccg.llvm=true"
  OUT_SUFFIX="-llvm"
  shift
else
  LLVM=no
  SML="/usr/local/smlnj/bin/sml"
  OUT_SUFFIX="-mlrisc"
fi

if [ x"$1" = x ] ; then
  echo "usage: run.sh [ --llvm ] prog"
  exit 1
fi

prog=$1

OUT_FILE="$prog$OUT_SUFFIX"

echo "results in $OUT_FILE: "

echo "{bmark=\"$prog\", llvm=\"$LLVM\", " > $OUT_FILE

# first we time the compile time
#
echo "    compiling ..."
echo -n " compiles=[ " >> $OUT_FILE
$SML <<EOF 1>/dev/null 2>&1
  use "timeit.sml";
  val outS = TextIO.openAppend("$OUT_FILE");
  fun loop 0 = ()
    | loop i = (
        Timing.timeUse (outS, "$prog.sml");
        if i > 1 then TextIO.output(outS, ",") else ();
        loop (i-1));
  loop $NCOMPS;
  TextIO.flushOut outS;
  TextIO.closeOut outS;
EOF
echo " ]," >> $OUT_FILE

# then measure runtimes
#
echo "    running ..."
echo -n " runs=[" >> $OUT_FILE
$SML <<EOF 1>/dev/null 2>&1
  use "timeit.sml";
  use "$prog.sml";
  val outS = TextIO.openAppend("$OUT_FILE");
  Timing.time($NRUNS, outS, Main.doit);
  TextIO.flushOut outS;
  TextIO.closeOut outS;
EOF
echo " ]}" >> $OUT_FILE
