#!/bin/sh
#
# Copyright (c) 2019 The Fellowship of SML/NJ
#
# Pre-installation script for SML/NJ.  The purpose of this script
# is to download and unpackage files in preparation of building on
# Windows.
#

complain() {
    echo "$@"
    exit 1
}

this=$0

ROOT=`pwd`

#
# set the various directory and file pathname variables
#
CONFIGDIR=$ROOT/config
BASEDIR=$ROOT/base		# where the base source tree is rooted
LIBDIR=$INSTALLDIR/lib		# where libraries live
BOOT_ARCHIVE=boot.x86-win32

#
# create the various sub directories
#
for dir in "$LIBDIR" "$BASEDIR" ; do
    makedir "$dir"
done

"$CONFIGDIR"/unpack "$ROOT" runtime
"$CONFIGDIR"/unpack "$ROOT" "$BOOT_ARCHIVE"
