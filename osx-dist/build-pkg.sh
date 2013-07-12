#!/bin/sh
#
# Script to build the installer package for x86 on Mac OS X (10.7+)
#

# get the version number
#
if [ $# != 1 ] ; then
  echo "usage: build-pkg.sh version"
  exit 1
fi
VERSION=$1

CONFIGURL=http://smlnj.cs.uchicago.edu/dist/working/$VERSION/config.tgz
DISTROOT=smlnj.dst
ID=org.smlnj.x86.pkg
ROOT=$(pwd)

# you need a developer ID to sign the final package
#
SIGN="Developer ID Installer: John Reppy"

if [ -d $DISTROOT ] ; then
  echo "please remove $DISTROOT first"
  exit 1
fi
mkdir $DISTROOT
cd $DISTROOT

# first we need to download and unbundle the config directory for the release
#
curl -O $CONFIGURL
tar -xzf config.tgz

# check that the version numbers match
#
CONFIG_VERSION=$(cat config/version)
if [ x"$VERSION" != x"$CONFIG_VERSION" ] ; then
  echo "version in config/version is $CONFIG_VERSION"
  exit 1
fi

# build the distribution (note that this assumes that config/targets is what we want!)
#
config/install.sh

# get the other files to include in the distribution
#
cp -p $ROOT/components/license.html .
svn export https://smlnj-gforge.cs.uchicago.edu/svn/smlnj/sml/trunk/READMES/$VERSION-README.html

# cleanup
#
rm *tgz

# back up to the root
#
cd $ROOT

# patch distribution file for version
#
sed -e "s/VERSION/$VERSION/g" components/distribution_xml.in > distribution.xml

# build package
#
pkgbuild --identifier $ID --scripts components/scripts/ --install-location /usr/local/Xsmlnj --root $DISTROOT smlnj.pkg

# build distribution package
#
productbuild --sign "$SIGN" --distribution ./distribution.xml --package-path components ./smlnj-x86-$VERSION.pkg

# cleanup
#
#rm -rf distribution.xml $DISTROOT smlnj.pkg
