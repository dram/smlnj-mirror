#!/bin/sh
#
# create the source tar files for a distribution
#

set +x

here=`pwd`
path_to_me=`dirname $0`


# export all source files
#
$path_to_me/checkout-all.sh --export

dirs="\
    MLRISC \
    ckit \
    cml \
    config \
    eXene \
    heap2asm \
    ml-burg \
    ml-lex \
    ml-lpt \
    ml-yacc \
    nlffi \
    pgraph \
    smlnj-c \
    smlnj-lib \
    trace-debug-profile \
  "

for d in $dirs ; do
  tar cfz $d.tgz $d
done

base_dirs="\
    cm \
    compiler \
    runtime \
    system \
  "

for d in $base_dirs ; do
  tar cfz $d.tgz base/$d
done
