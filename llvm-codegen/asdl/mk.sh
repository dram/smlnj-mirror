#!/bin/sh
#
# script to make the C++ headers/unpickler
#

STEM=cfg

ASDLGEN=/Users/jhr/Work/smlnj/sml/bin/asdlgen
#ASDLGEN=../../bin/asdlgen

if [ ! -x $ASDLGEN ] ; then
  echo "mk.sh: unable to find '$ASDLGEN'"
  exit 1
fi

echo "generating C++ code"
$ASDLGEN cxx $STEM.asdl || exit 1

if [ -r $STEM.hxx ] ; then
  mv $STEM.hxx ../include
else
  echo "mk.sh: unable to find '$STEM.hxx'"
  exit 1
fi

if [ -r $STEM.cxx ] ; then
  mv $STEM.cxx ../src
else
  echo "mk.sh: unable to find '$STEM.cxx'"
  exit 1
fi

echo "generating SML code"
$ASDLGEN sml $STEM.asdl || exit 1

mv *sig *sml ../tests

exit 0
