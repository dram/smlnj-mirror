#!/bin/sh
#
# COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
# All rights reserved.
#
# Script to configure and build the LLVM sources.
#
# usage: build-llvm.sh [--force] [-np <n>] [--llvm-src <path>] [--release]
#
#       --help                  Generate help message
#
#       --force                 Remove any existing build directory before
#                               configuration
#
#       --np <n>                Specify the number of cores to use in the build
#
#       --llvm-src <path>       Specify path to LLVM source tree relative to the
#                               llvm-codegen directory
#
#	--release		Build a "release" build in llvm-build-release
#				[default: "debug" build in llvm-build-debug]
#
#	--targets <targets>	Specify the target architectures to support.
#				An argument of "all" means all supported
#				architectures.
#

FORCE=no
LLVM_SRC=llvm-10.0.1.src
BUILD_TYPE=Debug
NPROCS=4
USE_GOLD_LD=no

# system specific defaults
#
case `uname -s` in
  Darwin)
    NPROCS=$(sysctl -n hw.physicalcpu)
  ;;
  Linux)
    USE_GOLD_LD=yes
  ;;
esac

ALL_TARGETS="AArch64;X86"
case $(uname -m) in
  x86_64) TARGETS="X86" ;;
  arm64) TARGETS="AArch64" ;;
  *) echo "unknown hardware platform"
    exit 1
    ;;
esac

usage() {
  echo "usage: build-llvm.sh [--force] [--np <n>] [--llvm-src <path>] [--release_build]"
  echo ""
  echo "    --help                Generate help message"
  echo ""
  echo "    --force               Remove any existing build directory before"
  echo "                          configuration"
  echo ""
  echo "    --np <n>              Specify the number of cores to use in the build"
  echo "                          [default: $NPROCS]"
  echo ""
  echo "    --llvm-src <path>     Specify path to LLVM source tree relative to the"
  echo "                          llvm-codegen directory [default: $LLVM_SRC]"
  echo ""
  echo "    --release             Build a "release" build to install in llvm-release"
  echo ""
  echo "    --targets <targets>   Specify target architectures (default: $TARGETS)"
  echo "                          The supported targets are X86 and AArch64"

  exit $1
}

# process command-line arguments
#
while [ "$#" != "0" ] ; do
  arg=$1; shift
  case $arg in
    -h) usage 0 ;;
    --help) usage 0 ;;
    --force) FORCE=yes ;;
    --np)
      if [ $# -ge 1 ] ; then
	NPROCS=$1
	shift
      else
	usage 1
      fi ;;
    --llvm-src)
      if [ $# -ge 1 ] ; then
	LLVM_SRC=$1
	shift
      else
	usage 1
      fi ;;
    --release)
      BUILD_TYPE=Release
      ;;
    --targets)
      if [ $# -ge 1 ] ; then
	if [ x"$1" = xall ] ; then
	  TARGETS=$ALL_TARGETS
	else
	  TARGETS=$1
	fi
	shift
      else
	usage 1
      fi ;;
    *) usage 1 ;;
  esac
done

if [ $BUILD_TYPE = "Debug" ] ; then
  LLVM_INSTALL=llvm-debug
else
  LLVM_INSTALL=llvm-release
fi
LLVM_BUILD=build-"$LLVM_INSTALL"
PRESET=smlnj-"$LLVM_INSTALL"

# most of the definitions are specified in the CMakePresets.json file
#
CMAKE_DEFS="\
  -DCMAKE_INSTALL_PREFIX=../$LLVM_INSTALL \
  -DLLVM_TARGETS_TO_BUILD=$TARGETS \
"

if [ x"$USE_GOLD_LD" = xyes ] ; then
  CMAKE_DEFS="$CMAKE_DEFS -DLLVM_USE_LINKER=gold"
fi

if [ -d "$LLVM_BUILD" ] ; then
  if [ $FORCE = yes ] ; then
    echo  "$0: removing old $LLVM_BUILD"
    rm -rf $LLVM_BUILD
  else
    echo "$0: $LLVM_BUILD already exists (use --force to override)"
    exit 1
  fi
fi

echo "$0: mkdir $LLVM_BUILD"
mkdir "$LLVM_BUILD"
cd $LLVM_BUILD

echo "$0: configuring build"
echo "  cmake --preset=$PRESET $CMAKE_DEFS ../$LLVM_SRC"
cmake --preset=$PRESET $CMAKE_DEFS ../$LLVM_SRC || exit 1

rm -rf $LLVM_INSTALL

echo "$0: building on $NPROCS cores"
echo "  make -j $NPROCS install"
time make -j $NPROCS install

echo ""
echo "LLVM for $TARGETS installed in $LLVM_INSTALL"
