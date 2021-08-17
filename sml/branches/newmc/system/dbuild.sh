#!/bin/sh

# start in base/system
rm -rf sml.b*
./cmb-make ~/sml/Dev/110.98.1
./makeml
./installml -clean
cd ../..
config/install.sh
cd base/system
rm -rf sml.b*
./cmb-make ../../bin/sml
./makeml
./installml -clean
cd ../..
config/install.sh
