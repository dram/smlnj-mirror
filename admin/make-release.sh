#!/bin/sh
#
# "tag" a release snapshot by copying all of the trees into a fresh release tree.
#

if [ $# -lt 1 ] ; then
    echo Usage: $0 '<release-number>'
    exit 1
fi

relno=$1

gf=https://smlnj-gforge.cs.uchicago.edu/svn/smlnj

here=`pwd`

for tree in config sml smlnj-lib MLRISC asdl ml-yacc trace-debug-profile pgraph \
            ckit cml eXene smlnj-c ml-burg ml-lex heap2asm nlffi doc
do
  echo "tagging $tree..."
  svn cp $gf/$tree/trunk $gf/$tree/releases/release-$relno -m "Release $relno"
done
