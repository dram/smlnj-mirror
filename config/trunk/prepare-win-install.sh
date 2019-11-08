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
# Function to make a directory including its ancestors.
#
makedir() {
    if [ x"$1" = x ] ; then
	:
    elif [ -d "$1" ] ; then
	:
    else
	makedirtmp=`dirname "$1"`
	makedir "$makedirtmp"
	if [ x${INSTALL_VERBOSE} = xtrue ] ; then
	    vsay "$this: Making directory $1"
	fi
	if mkdir "$1" ; then
	    :
	else
	    complain "$this: !!! Unable to make directory $1!"
	fi
    fi
}

#
# create the various sub directories
#
for dir in "$LIBDIR" "$BASEDIR" ; do
    makedir "$dir"
done

"$CONFIGDIR"/unpack "$ROOT" runtime
"$CONFIGDIR"/unpack "$ROOT" "$BOOT_ARCHIVE"
