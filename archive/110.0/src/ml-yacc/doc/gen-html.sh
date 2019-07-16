#!/bin/ksh
#
# Generate HTML using the HeVeA tool
#

case $(uname -s) in
  SunOS) ;;
  *) echo "can only run this script on a Sun"
    exit 1;;
esac

BINDIR=/home/sml/bin/sparc-solaris

export HEVEADIR=/home/sml/lib/hevea

HEVEA=$BINDIR/hevea
HACHA=$BINDIR/hacha
IMAGEN=$BINDIR/imagen

HVA_FILES="$HEVEADIR/html/symb.hva $HEVEADIR/html/hevea.hva"

print "<=== start"
$HEVEA -v -noiso $HVA_FILES mlyacc.tex
print "<=== hevea done"
$IMAGEN mlyacc
print "<=== imagen done"
$HACHA mlyacc.html
print "<=== hacha done"

mv *.gif *.html HTML/


