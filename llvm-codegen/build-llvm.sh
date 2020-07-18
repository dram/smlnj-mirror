#!/bin/sh
#
# COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
# All rights reserved.
#
# Script to configure and build the LLVM sources.
#
# usage: build-llvm.sh [--force] [-np <n>] [--llvm-src <path>] [--build-dir <path>]
#
#       --help                  Generate help message
#
#       --force                 Remove any existing build directory before
#                               configuration
#
#       --np <n>                Specify the number of cores to use in the build
#
#       --llvm-src <path>       Specify path to LLVM source tree relative to the
#                               llvm-codegen directory [default: llvm-10.0.0.src]
#
#       --build-dir <path>      Specify name of build directory relative to the
#                               llvm-codegen directory [default: llvm-build]
#

FORCE=no
LLVM_SRC=llvm-10.0.0.src
LLVM_BUILD=llvm-build
NPROCS=4

# system specific defaults
#
case `uname -s` in
  Darwin)
    NPROCS=$(sysctl -n hw.physicalcpu)
  ;;
esac

usage() {
  echo "usage: build-llvm.sh [--force] [-np <n>] [--llvm-src <path>] [--build-dir <path>]"
  echo ""
  echo "    --help              Generate help message"
  echo ""
  echo "    --force             Remove any existing build directory before"
  echo "                        configuration"
  echo ""
  echo "    --np <n>            Specify the number of cores to use in the build"
  echo "                        [default: $NPROCS]"
  echo ""
  echo "    --llvm-src <path>   Specify path to LLVM source tree relative to the"
  echo "                        llvm-codegen directory [default: $LLVM_SRC]"
  echo ""
  echo "    --build-dir <path>  Specify name of build directory relative to the"
  echo "                        llvm-codegen directory [default: $LLVM_BUILD]"

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
    --build-dir)
      if [ $# -ge 1 ] ; then
	LLVM_BUILD=$1
	shift
      else
	usage 1
      fi ;;
    *) usage 1 ;;
  esac
done

CMAKE_DEFS="\
  -DCMAKE_BUILD_TYPE=Release \
  -DCMAKE_INSTALL_PREFIX=../llvm \
  -DLLVM_TARGETS_TO_BUILD=X86;AArch64 \
  -DLLVM_ENABLE_OCAMLDOC=OFF \
  -DLLVM_INCLUDE_BENCHMARKS=OFF \
  -DLLVM_INCLUDE_DOCS=OFF \
  -DLLVM_INCLUDE_GO_TESTS=OFF \
  -DLLVM_INCLUDE_TESTS=OFF \
  -DLLVM_TOOL_DSYMUTIL_BUILD=OFF \
  -DLLVM_TOOL_GOLD_BUILD=OFF \
  -DLLVM_TOOL_LLVM_AR_BUILD=OFF \
  -DLLVM_TOOL_LLVM_AS_BUILD=OFF \
  -DLLVM_TOOL_LLVM_AS_FUZZER_BUILD=OFF \
  -DLLVM_TOOL_LLVM_BCANALYZER_BUILD=OFF \
  -DLLVM_TOOL_LLVM_CAT_BUILD=OFF \
  -DLLVM_TOOL_LLVM_CFI_VERIFY_BUILD=OFF \
  -DLLVM_TOOL_LLVM_COV_BUILD=OFF \
  -DLLVM_TOOL_LLVM_CVTRES_BUILD=OFF \
  -DLLVM_TOOL_LLVM_CXXDUMP_BUILD=OFF \
  -DLLVM_TOOL_LLVM_CXXFILT_BUILD=OFF \
  -DLLVM_TOOL_LLVM_CXXMAP_BUILD=OFF \
  -DLLVM_TOOL_LLVM_C_TEST_BUILD=OFF \
  -DLLVM_TOOL_LLVM_DIFF_BUILD=OFF \
  -DLLVM_TOOL_LLVM_DIS_BUILD=OFF \
  -DLLVM_TOOL_LLVM_DWP_BUILD=OFF \
  -DLLVM_TOOL_LLVM_ELFABI_BUILD=OFF \
  -DLLVM_TOOL_LLVM_EXEGESIS_BUILD=OFF \
  -DLLVM_TOOL_LLVM_EXTRACT_BUILD=OFF \
  -DLLVM_TOOL_LLVM_GO_BUILD=OFF \
  -DLLVM_TOOL_LLVM_IFS_BUILD=OFF \
  -DLLVM_TOOL_LLVM_ISEL_FUZZER_BUILD=OFF \
  -DLLVM_TOOL_LLVM_ITANIUM_DEMANGLE_FUZZER_BUILD=OFF \
  -DLLVM_TOOL_LLVM_JITLINK_BUILD=OFF \
  -DLLVM_TOOL_LLVM_JITLISTENER_BUILD=OFF \
  -DLLVM_TOOL_LLVM_LINK_BUILD=OFF \
  -DLLVM_TOOL_LLVM_LIPO_BUILD=OFF \
  -DLLVM_TOOL_LLVM_LTO2_BUILD=OFF \
  -DLLVM_TOOL_LLVM_LTO_BUILD=OFF \
  -DLLVM_TOOL_LLVM_MCA_BUILD=OFF \
  -DLLVM_TOOL_LLVM_MC_ASSEMBLE_FUZZER_BUILD=OFF \
  -DLLVM_TOOL_LLVM_MC_BUILD=OFF \
  -DLLVM_TOOL_LLVM_MC_DISASSEMBLE_FUZZER_BUILD=OFF \
  -DLLVM_TOOL_LLVM_MICROSOFT_DEMANGLE_FUZZER_BUILD=OFF \
  -DLLVM_TOOL_LLVM_MODEXTRACT_BUILD=OFF \
  -DLLVM_TOOL_LLVM_MT_BUILD=OFF \
  -DLLVM_TOOL_LLVM_NM_BUILD=OFF \
  -DLLVM_TOOL_LLVM_OBJCOPY_BUILD=OFF \
  -DLLVM_TOOL_LLVM_OBJDUMP_BUILD=OFF \
  -DLLVM_TOOL_LLVM_OPT_FUZZER_BUILD=OFF \
  -DLLVM_TOOL_LLVM_OPT_REPORT_BUILD=OFF \
  -DLLVM_TOOL_LLVM_PDBUTIL_BUILD=OFF \
  -DLLVM_TOOL_LLVM_PROFDATA_BUILD=OFF \
  -DLLVM_TOOL_LLVM_RC_BUILD=OFF \
  -DLLVM_TOOL_LLVM_READOBJ_BUILD=OFF \
  -DLLVM_TOOL_LLVM_REDUCE_BUILD=OFF \
  -DLLVM_TOOL_LLVM_RTDYLD_BUILD=OFF \
  -DLLVM_TOOL_LLVM_SHLIB_BUILD=OFF \
  -DLLVM_TOOL_LLVM_SIZE_BUILD=OFF \
  -DLLVM_TOOL_LLVM_SPECIAL_CASE_LIST_FUZZER_BUILD=OFF \
  -DLLVM_TOOL_LLVM_SPLIT_BUILD=OFF \
  -DLLVM_TOOL_LLVM_STRESS_BUILD=OFF \
  -DLLVM_TOOL_LLVM_STRINGS_BUILD=OFF \
  -DLLVM_TOOL_LLVM_SYMBOLIZER_BUILD=OFF \
  -DLLVM_TOOL_LLVM_UNDNAME_BUILD=OFF \
  -DLLVM_TOOL_LLVM_XRAY_BUILD=OFF \
  -DLLVM_TOOL_LLVM_YAML_NUMERIC_PARSER_FUZZER_BUILD=OFF \
  -DLLVM_TOOL_LTO_BUILD=OFF \
  -DLLVM_TOOL_OBJ2YAML_BUILD=OFF \
  -DLLVM_TOOL_OPT_BUILD=OFF \
  -DLLVM_TOOL_OPT_VIEWER_BUILD=OFF \
  -DLLVM_TOOL_REMARKS_SHLIB_BUILD=OFF \
  -DLLVM_TOOL_SANCOV_BUILD=OFF \
  -DLLVM_TOOL_SANSTATS_BUILD=OFF \
  -DLLVM_TOOL_VERIFY_USELISTORDER_BUILD=OFF \
  -DLLVM_TOOL_VFABI_DEMANGLE_FUZZER_BUILD=OFF \
  -DLLVM_TOOL_XCODE_TOOLCHAIN_BUILD=OFF \
  -DLLVM_TOOL_YAML2OBJ_BUILD=OFF \
"

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
cd "$LLVM_BUILD"

echo "$0: configuring build"
cmake -G "Unix Makefiles" $CMAKE_DEFS "../$LLVM_SRC" || exit 1

echo "$0: building on $NPROCS cores"
time make -j $NPROCS install
