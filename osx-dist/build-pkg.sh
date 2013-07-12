#!/bin/sh
#
# Script to build the installer package for x86 on Mac OS X (10.7+)
#

# FIXME: check for VERSION argument!!!
VERSION=$1

# patch distribution file for version
#
sed -e "s/VERSION/$VERSION/g" distribution_xml.in > distribution.xml

# build package
#
pkgbuild --scripts smlnj.dst/config/MacResources/ --install-location /usr/local/smlnj --root smlnj.dst/ smlnj.pkg

# build distribution package
#
productbuild --sign "Mac Developer: Rich Manalang (3U78U4KMEF)" --distribution ./distribution.xml --package-path . ./smlnj-x86-$VERSION.pkg
