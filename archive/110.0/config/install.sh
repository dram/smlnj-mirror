#!/bin/sh
#
# Copyright (c) 1994 AT&T Bell Laboratories.
#
# installation script for SML/NJ and related tools; this is a temporary
# placeholder until the configuration tool is finished.
#

#set -x

#
# get the target list
#
if [ ! -r config/targets ]; then
  echo "!!! no target list"
  exit 1;
fi
. config/targets

#
# some OSs have make in strange places
#
MAKE=make

#
# command for building SML libraries; this should be either
# "CM.recompile()" or "CM.stabilize true".  The latter builds
# stable libraries, which may be somewhat faster to use.
#
#CM_MAKE_LIB="CM.recompile();"
CM_MAKE_LIB="CM.stabilize true;"

#
# set the SML root directory
#
REAL_PWD=`pwd`
ROOT=${PWD:-$REAL_PWD}
echo "SML root is $ROOT"

#
# set the various directory pathname variables
#
BINDIR=$ROOT/bin
CONFIGDIR=$ROOT/config
HEAPDIR=$BINDIR/.heap
RUNDIR=$BINDIR/.run
SRCDIR=$ROOT/src
LIBDIR=$ROOT/lib

#
# the paths to ml-yacc and ml-lex; needed to configure CM
#
YACCPATH=$BINDIR/ml-yacc
LEXPATH=$BINDIR/ml-lex
BURGPATH=$BINDIR/ml-burg

#
# the release version that we are installing
#
VERSION=`cat $CONFIGDIR/version`
echo "installing version $VERSION"

#
# create the various sub directories
#
for dir in $BINDIR $HEAPDIR $RUNDIR $LIBDIR $SRCDIR
do
  if [ -d $dir ]; then
    echo "$dir already exists"
  else
    echo "creating $dir"
    mkdir $dir
    if [ $? != "0" ]; then
      echo "unable to create $dir"
      exit 1
    fi
  fi
done


#
# install the script that tests the architecture, and make sure that it works
#
if [ -x $BINDIR/.arch-n-opsys ]; then
  echo "$BINDIR/.arch-n-opsys already exists"
else
  cat $CONFIGDIR/_arch-n-opsys > $BINDIR/.arch-n-opsys
  chmod 555 $BINDIR/.arch-n-opsys
  if [ ! -x $BINDIR/.arch-n-opsys ]; then
    echo "!!! installation of $BINDIR/.arch-n-opsys failed for some reason"
    exit 1
  fi
fi
ARCH_N_OPSYS=`$BINDIR/.arch-n-opsys`
if [ "$?" != "0" ]; then
  echo "!!! $BINDIR/.arch-n-opsys fails on this machine"
  echo "!!! you must patch this by hand and repeat the installation"
  exit 2
else
  echo "$BINDIR/.arch-n-opsys reports $ARCH_N_OPSYS"
fi
eval $ARCH_N_OPSYS

if [ -x $BINDIR/.run-sml ]; then
  echo "$BINDIR/.run-sml already exists"
else
  cat $CONFIGDIR/_run-sml | \
    sed -e "s,@BINDIR@,$BINDIR," -e "s,@VERSION@,$VERSION," \
    > $BINDIR/.run-sml
  chmod 555 $BINDIR/.run-sml
  if [ ! -x $BINDIR/.run-sml ]; then
    echo "!!! installation of $BINDIR/.run-sml failed for some reason"
    exit 1
  fi
fi

#
# set some architecture dependent run-time system flags
#
case $ARCH in
  mips*) ALLOC=1M ;;
  x86)
    ALLOC=256k
  ;;
  alpha32)
    ALLOC=512k
  ;;
  *)
    ALLOC=512k
  ;;
esac
case $OPSYS in
  solaris)
    MAKE=/usr/ccs/bin/make
  ;;
  linux)
    EXTRA_DEFS=`$CONFIGDIR/chk-global-names.sh`
    if [ "$?" != "0" ]; then
      echo "problems checking for underscores in global names"
      exit 1
    fi
    EXTRA_DEFS="XDEFS=$EXTRA_DEFS"
  ;;
esac

#
# the name of the bin files directory
#
BIN_FILES=bin.$ARCH-unix

#
# build the run-time system
#
$CONFIGDIR/unpack.sh "run-time" $SRCDIR runtime $ROOT/runtime.tar
if [ "$?" != "0" ]; then
  exit $?
fi
if [ ! -x $RUNDIR/run.$ARCH-$OPSYS ]; then
  cd $SRCDIR/runtime/objs
  echo "compiling the run-time system"
  $MAKE -f mk.$ARCH-$OPSYS $EXTRA_DEFS
  if [ -x run.$ARCH-$OPSYS ]; then
    mv run.$ARCH-$OPSYS $RUNDIR
    $MAKE MAKE=$MAKE clean
  else
    echo "!!! run-time system build failed for some reason"
    exit 1
  fi
fi
cd $SRCDIR

#
# boot the base SML system
#
if [ -r $HEAPDIR/sml.$HEAP_SUFFIX ]; then
  echo "$HEAPDIR/sml.$HEAP_SUFFIX already exists"
else
  if [ ! -d $ROOT/$BIN_FILES ]; then
    if [ -r $ROOT/$BIN_FILES.tar.Z ]; then
      cd $ROOT
      echo "unpacking bin files"
      zcat $BIN_FILES.tar.Z | tar -xf -
    elif [ -r $ROOT/$BIN_FILES.tar ]; then
      cd $ROOT
      echo "unpacking bin files"
      tar -xf $BIN_FILES.tar
    else
      echo "!!! the bin files are missing"
      exit 1
    fi
    if [ ! -d $ROOT/$BIN_FILES ]; then
      echo "!!! unable to unpack bin files"
      exit 1
    fi
  fi
  cd $ROOT
  $RUNDIR/run.$ARCH-$OPSYS @SMLboot=$ROOT/$BIN_FILES @SMLalloc=$ALLOC <<XXXX
    $SET_FLAGS
    val use = Compiler.Interact.useFile
    val _ = (SMLofNJ.exportML "sml";
             print Compiler.banner;
             print "\n");
XXXX
  if [ -r sml.$HEAP_SUFFIX ]; then
    mv sml.$HEAP_SUFFIX $HEAPDIR
    cd $BINDIR
    ln -s .run-sml sml
  else
    echo "!!! unable to build SML heap image (sml.$HEAP_SUFFIX)"
    exit 1
  fi
fi


#
# now build the individual targets
#
cd $SRCDIR
echo "install targets"
for i in $TARGETS
do
  if [ $i = "doc" ]; then
    TARGET=$i
  else
    TARGET=$i.$HEAP_SUFFIX
  fi
  if [ -r $HEAPDIR/$TARGET ]; then
    echo "$TARGET already exists"
  else
    echo "  building $TARGET"
    case $i in
      src-smlnj)
	$CONFIGDIR/unpack.sh src-smlnj $ROOT/src sml-nj $ROOT/sml-nj.tar
      ;;
      sml-full)
	if [ ! -d $ROOT/$BIN_FILES ]; then
	  echo "!!! bin files are missing; build of $TARGET failed"
	  exit 1
	else
	  cd $ROOT
	  $RUNDIR/run.$ARCH-$OPSYS @SMLfull @SMLboot=$ROOT/$BIN_FILES @SMLalloc=$ALLOC <<XXXX
	    $SET_FLAGS
	    val use = Compiler.Interact.useFile
	    val _ = (SMLofNJ.exportML "sml-full";
		     print Compiler.banner;
		     print " [full]\n");
XXXX
	  if [ -r sml-full.$HEAP_SUFFIX ]; then
	    mv sml-full.$HEAP_SUFFIX $HEAPDIR
	    cd $BINDIR
	    ln -s .run-sml sml-full
	  else
	    echo "!!! unable to build SML-FULL heap image"
	    exit 1
	  fi
	fi
      ;;
      sml-cm)
	$CONFIGDIR/unpack.sh CM $SRCDIR cm $ROOT/cm.tar
        if [ "$?" != "0" ]; then
	  exit $?
        fi
        cd $SRCDIR/cm
	./build -yacc $YACCPATH -lex $LEXPATH -burg $BURGPATH -L ".:$LIBDIR"
        if [ -r $TARGET ]; then
	  mv $TARGET $HEAPDIR
	  if [ ! -f $BINDIR/$i ]; then
	    cd $BINDIR
	    ln -s .run-sml $i
	  fi
        else
	  echo "!!! build of $TARGET failed"
	  exit 1
        fi
	if [ "$INSTALL_SML_AND_CM" != "TRUE" ]; then
	  cd $HEAPDIR
	  rm -f sml.$HEAP_SUFFIX
	  ln -s sml-cm.$HEAP_SUFFIX sml.$HEAP_SUFFIX
	fi
      ;;
      sml-full-cm)
	$CONFIGDIR/unpack.sh CM $SRCDIR cm $ROOT/cm.tar
        if [ "$?" != "0" ]; then
	  exit $?
        fi
        cd $SRCDIR/cm
	./build -yacc $YACCPATH -lex $LEXPATH -burg $BURGPATH -L ".:$LIBDIR" -sml $BINDIR/sml-full -o $TARGET
        if [ -r $TARGET ]; then
	  mv $TARGET $HEAPDIR
	  if [ ! -f $BINDIR/$i ]; then
	    cd $BINDIR
	    ln -s .run-sml $i
	  fi
        else
	  echo "!!! build of $TARGET failed"
	  exit 1
        fi
      ;;
      ml-yacc)
        $CONFIGDIR/unpack.sh ML-Yacc $SRCDIR ml-yacc $ROOT/ml-yacc.tar
        if [ "$?" != "0" ]; then
	  exit $?
        fi
        cd $SRCDIR/ml-yacc
        ./build
        if [ -r src/$TARGET ]; then
	  mv src/$TARGET $HEAPDIR
	  if [ ! -f $BINDIR/$i ]; then
	    cd $BINDIR
	    ln -s .run-sml $i
	  fi
        else
	  echo "!!! build of $TARGET failed"
	  exit 1
        fi
      ;;
      ml-lex)
        $CONFIGDIR/unpack.sh ML-Lex $SRCDIR ml-lex $ROOT/ml-lex.tar
        if [ "$?" != "0" ]; then
	  exit $?
        fi
        cd $SRCDIR/ml-lex
        ./build
        if [ -r $TARGET ]; then
	  mv $TARGET $HEAPDIR
	  if [ ! -f $BINDIR/$i ]; then
	    cd $BINDIR
	    ln -s .run-sml $i
	  fi
        else
	  echo "!!! build of $TARGET failed"
	  exit 1
        fi
      ;;
      ml-burg)
        $CONFIGDIR/unpack.sh ML-Burg $SRCDIR ml-burg $ROOT/ml-burg.tar
        if [ "$?" != "0" ]; then
	  exit $?
        fi
        cd $SRCDIR/ml-burg
        ./build
        if [ -r $TARGET ]; then
	  mv $TARGET $HEAPDIR
	  if [ ! -f $BINDIR/$i ]; then
	    cd $BINDIR
	    ln -s .run-sml $i
	  fi
        else
	  echo "!!! build of $TARGET failed"
	  exit 1
        fi
      ;;
      smlnj-lib)
        $CONFIGDIR/unpack.sh "SML/NJ Library" $SRCDIR smlnj-lib $ROOT/smlnj-lib.tar
        if [ "$?" != "0" ]; then
	  exit $?
        fi
      # make the Util library
        cd $SRCDIR/smlnj-lib/Util
	echo "CM.stabilize'(\"smlnj-lib.cm\", true);" | $BINDIR/sml-cm
	if [ ! -f $LIBDIR/smlnj-lib.cm ]; then
          cd $LIBDIR
          echo "Alias $SRCDIR/smlnj-lib/Util/smlnj-lib.cm" > smlnj-lib.cm
	fi
      # make the Unix library
        cd $SRCDIR/smlnj-lib/Unix
	echo "CM.stabilize'(\"unix-lib.cm\", true);" | $BINDIR/sml-cm
	if [ ! -f $LIBDIR/unix-lib.cm ]; then
          cd $LIBDIR
          echo "Alias $SRCDIR/smlnj-lib/Unix/unix-lib.cm" > unix-lib.cm
	fi
      # make the INet library
        cd $SRCDIR/smlnj-lib/INet
	echo "CM.stabilize'(\"inet-lib.cm\", true);" | $BINDIR/sml-cm
	if [ ! -f $LIBDIR/inet-lib.cm ]; then
          cd $LIBDIR
          echo "Alias $SRCDIR/smlnj-lib/INet/inet-lib.cm" > inet-lib.cm
	fi
      # make the HTML library
        cd $SRCDIR/smlnj-lib/HTML
	echo "CM.stabilize'(\"html-lib.cm\", true);" | $BINDIR/sml-cm
	if [ ! -f $LIBDIR/html-lib.cm ]; then
          cd $LIBDIR
          echo "Alias $SRCDIR/smlnj-lib/HTML/html-lib.cm" > html-lib.cm
	fi
      # make the PP library
        cd $SRCDIR/smlnj-lib/PP
	echo "CM.stabilize'(\"pp-lib.cm\", true);" | $BINDIR/sml-cm
	if [ ! -f $LIBDIR/pp-lib.cm ]; then
          cd $LIBDIR
          echo "Alias $SRCDIR/smlnj-lib/PP/pp-lib.cm" > pp-lib.cm
	fi
      # make the Reg-Exp library
        cd $SRCDIR/smlnj-lib/RegExp
	echo "CM.stabilize'(\"regexp-lib.cm\", true);" | $BINDIR/sml-cm
	if [ ! -f $LIBDIR/regexp-lib.cm ]; then
          cd $LIBDIR
          echo "Alias $SRCDIR/smlnj-lib/RegExp/regexp-lib.cm" > regexp-lib.cm
	fi
      # make the Reactive library
        cd $SRCDIR/smlnj-lib/Reactive
	echo "CM.stabilize'(\"reactive-lib.cm\", true);" | $BINDIR/sml-cm
	if [ ! -f $LIBDIR/reactive-lib.cm ]; then
          cd $LIBDIR
          echo "Alias $SRCDIR/smlnj-lib/Reactive/reactive-lib.cm" > reactive-lib.cm
	fi
      ;;
      ml-yacc-lib)
        $CONFIGDIR/unpack.sh ML-Yacc $SRCDIR ml-yacc $ROOT/ml-yacc.tar
        if [ "$?" != "0" ]; then
	  exit $?
        fi
        cd $SRCDIR/ml-yacc/lib
        echo "$CM_MAKE_LIB" | $BINDIR/sml-cm
	if [ ! -f $LIBDIR/ml-yacc-lib.cm ]; then
          cd $LIBDIR
          echo "Alias $SRCDIR/ml-yacc/lib/sources.cm" > ml-yacc-lib.cm
	fi
      ;;
      cml)
        $CONFIGDIR/unpack.sh CML $SRCDIR cml $ROOT/cml.tar
        if [ "$?" != "0" ]; then
	  exit $?
        fi
        cd $SRCDIR/cml/src
        echo "$CM_MAKE_LIB" | $BINDIR/sml-cm
	if [ ! -f $LIBDIR/cml.cm ]; then
          cd $LIBDIR
          echo "Alias $SRCDIR/cml/src/sources.cm" > cml.cm
	fi
      ;;
      cml-lib)
        $CONFIGDIR/unpack.sh CML $SRCDIR cml $ROOT/cml.tar
        if [ "$?" != "0" ]; then
	  exit $?
        fi
        cd $SRCDIR/cml/cml-lib
        echo "$CM_MAKE_LIB" | $BINDIR/sml-cm
	if [ ! -f $LIBDIR/cml-lib.cm ]; then
          cd $LIBDIR
          echo "Alias $SRCDIR/cml/cml-lib/sources.cm" > cml-lib.cm
	fi
      ;;
# NOTE: cml-cm doesn't really work right now because of CM features (bugs).
      cml-cm)
	CMD="CM.autoloading(SOME true); CM.clearAutoList();"
	CMD="$CMD CM.autoload'(\"$LIBDIR/cml.cm\");"
	if [ "$AUTOLOAD_CML_LIB" = "TRUE" ]; then
	  CMD="$CMD CM.autoload'(\"$LIBDIR/cml-lib.cm\");"
	fi
	if [ "$AUTOLOAD_EXENE" = "TRUE" ]; then
	  CMD="$CMD CM.autoload'(\"$LIBDIR/eXene.cm\");"
	fi
	cd $ROOT
	$BINDIR/sml-cm <<XXXX
	  $CMD
	  val _ = (SMLofNJ.exportML "$i";
		print CML.banner;
		print "[CML autoload]\n");
XXXX
	if [ -r $TARGET ]; then
	  mv $TARGET $HEAPDIR
	  if [ ! -f $BINDIR/$i ]; then
	    cd $BINDIR
	    ln -s .run-sml $i
	  fi
	else
	  echo "!!! unable to build autoloading CML"
	  exit 1
	fi
      ;;
      eXene)
        $CONFIGDIR/unpack.sh EXene $SRCDIR eXene $ROOT/eXene.tar
        if [ "$?" != "0" ]; then
	  exit $?
        fi
        cd $SRCDIR/eXene
        echo "$CM_MAKE_LIB" | $BINDIR/sml-cm
	if [ ! -f $LIBDIR/eXene.cm ]; then
          cd $LIBDIR
          echo "Alias $SRCDIR/eXene/sources.cm" > eXene.cm
	fi
      ;;
      doc)
	$CONFIGDIR/unpack.sh Doc $ROOT doc $ROOT/doc.tar
        if [ "$?" != "0" ]; then
	  exit $?
        fi
        cd $ROOT/doc
	build $ROOT
      ;;
      *)
        echo "!!! unknown target $i"
      ;;
    esac
  fi
done

if [ "$ENABLE_AUTOLOADING" = "TRUE" ]; then
  CMD="CM.autoloading(SOME true); CM.clearAutoList();"
  if [ "$AUTOLOAD_SMLNJ_LIB" = "TRUE" ]; then
    CMD="$CMD CM.autoload'(\"$LIBDIR/smlnj-lib.cm\");"
  fi
  if [ "$AUTOLOAD_SMLNJ_UNIX" = "TRUE" ]; then
    CMD="$CMD CM.autoload'(\"$LIBDIR/unix-lib.cm\");"
  fi
  if [ "$AUTOLOAD_SMLNJ_INET" = "TRUE" ]; then
    CMD="$CMD CM.autoload'(\"$LIBDIR/inet-lib.cm\");"
  fi
  if [ "$AUTOLOAD_SMLNJ_HTML" = "TRUE" ]; then
    CMD="$CMD CM.autoload'(\"$LIBDIR/html-lib.cm\");"
  fi
  if [ "$AUTOLOAD_SMLNJ_PP" = "TRUE" ]; then
    CMD="$CMD CM.autoload'(\"$LIBDIR/pp-lib.cm\");"
  fi
  if [ "$AUTOLOAD_SMLNJ_REGEXP" = "TRUE" ]; then
    CMD="$CMD CM.autoload'(\"$LIBDIR/regexp-lib.cm\");"
  fi
  if [ "$AUTOLOAD_SMLNJ_REACTIVE" = "TRUE" ]; then
    CMD="$CMD CM.autoload'(\"$LIBDIR/reactive-lib.cm\");"
  fi
  cd $ROOT
  $BINDIR/sml-cm <<XXXX
    $CMD
    val _ = (SMLofNJ.exportML "sml-cm";
             print Compiler.banner;
             print " [CM; autoload enabled]\n");
XXXX
  if [ -r sml-cm.$HEAP_SUFFIX ]; then
    mv sml-cm.$HEAP_SUFFIX $HEAPDIR
  else
    echo "!!! unable to build SML with autoloading"
    exit 1
  fi
fi

