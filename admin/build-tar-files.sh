#!/bin/sh
#
# create the source tar files for a distribution
#
# usage:
#	admin/build-tar-files.sh
#

set +x

here=`pwd`
path_to_me=`dirname $0`

# export all source files
#
$path_to_me/checkout-all.sh --export

# cleanup stuff that shouldn't be in the release
#
rm -rf smlnj-lib/Dev

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
  tar -czf $here/$d.tgz $d
done

base_dirs="\
    cm \
    compiler \
    runtime \
    system \
  "

cd $here/base
for d in $base_dirs ; do
  tar -czf $here/$d.tgz $d
done

# building the documentation requires configuring it and then generating the manual pages
#
cd $here
cd doc
autoconf -Iconfig || exit 1
./configure
#
# generate the man pages into $here/doc/doc/man
make man || exit 1
#
# cleanup
make distclean
# build tar files
cd $here/doc
tar -czf $here/doc.tar.gz doc
