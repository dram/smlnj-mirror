#!/bin/sh
#
# Script to build the installer package for x86 on Mac OS X (10.7+)
#

CMD="build-pkg.sh"

# get the version number
#
if [ $# != 1 ] ; then
  echo "usage: $CMD version"
  exit 1
fi
VERSION=$1

CONFIGURL=http://smlnj.cs.uchicago.edu/dist/working/$VERSION/config.tgz
DISTROOT=smlnj.dst
ID=org.smlnj.x86.pkg
ROOT=$(pwd)
RSRC=Resources

# you need a developer ID to sign the final package; 
#
case x"$USER" in
  xjhr) SIGN="Developer ID Installer: John Reppy" ;;
  *)
    echo "$CMD [Warning]: unknown user, so package will not be signed!"
    SIGN=none
  ;;
esac

if [ -d $DISTROOT ] ; then
  echo "$CMD [Error]: please remove $DISTROOT first"
  exit 1
fi
mkdir $DISTROOT
cd $DISTROOT

# first we need to download and unbundle the config directory for the release
#
curl -s -S -O $CONFIGURL
tar -xzf config.tgz
if [ "$?" != 0 ] ; then
  # note that if config.tgz does not exist, curl will still work (it will get a
  # 404 page from the server)
  echo "$CMD [Error]: unable to download/unpack config.tgz"
  cd $ROOT
  rm -rf $DISTROOT
  exit 1
fi

# check that the version numbers match
#
if [ ! -r config/version ] ; then
  echo "$CMD [Error]: config/version is missing"
  exit 1
fi
CONFIG_VERSION=$(cat config/version)
if [ x"$VERSION" != x"$CONFIG_VERSION" ] ; then
  echo "$CMD [Error]: version in config/version is $CONFIG_VERSION"
  cd $ROOT
  rm -rf $DISTROOT
  exit 1
fi

# build the distribution (note that this assumes that config/targets is what we want!)
#
config/install.sh
if [ "$?" != 0 ] ; then
  echo "$CMD [Error]: problem building SML/NJ"
  exit 1
fi

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

# create the resources directory and fill it
#
if [ -d $RSRC ] ; then
  rm -rf $RSRC
fi
mkdir $RSRC
sed -e "s/VERSION/$VERSION/g" components/distribution_xml.in > $RSRC/distribution.xml
cp -p components/smlnj-background.jpg $RSRC/background.jpg
#cp -p components/welcome.html $RSC/welcome.html
cp -p $DISTROOT/$VERSION-README.html $RSRC/readme.html
cp -p components/license.html $RSRC/license.html
cp -p components/conclusion.html $RSRC/conclusion.html

# build package
#
pkgbuild --identifier $ID --scripts components/scripts/ --install-location /usr/local/Xsmlnj --root $DISTROOT smlnj.pkg

# build distribution package
#
if [ $SIGN = none ] ; then
  echo "$CMD: building unsigned package smlnj-x86-$VERSION.pkg"
  productbuild --package-path components --resources $RSRC \
      --distribution $RSRC/distribution.xml ./smlnj-x86-$VERSION.pkg
else
  echo "$CMD: building signed package smlnj-x86-$VERSION.pkg"
  productbuild --sign "$SIGN" --package-path components --resources $RSRC \
      --distribution $RSRC/distribution.xml ./smlnj-x86-$VERSION.pkg
fi

# cleanup
#
rm -rf $RSRC $DISTROOT smlnj.pkg
