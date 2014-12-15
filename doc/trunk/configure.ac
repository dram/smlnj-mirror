dnl Process this file with autoconf to produce a configure script.
dnl
dnl COPYRIGHT (c) 2014 The Fellowship of SML/NJ (http://www.smlnj.org)
dnl All rights reserved.
dnl
dnl This the configuration script for autoconf version 2.60+.

AC_INIT(smlnj-doc,110.77,sml@mailman.cs.uchicago.edu)

AC_PREREQ(2.60)
AC_COPYRIGHT([[COPYRIGHT (c) 2014 The Fellowship of SML/NJ (http://www.smlnj.org)]])
AC_CONFIG_SRCDIR(src/man/smlnj.7.txt)
AC_CONFIG_AUX_DIR(config)
AC_CONFIG_MACRO_DIR(config)
 
dnl
dnl get host information
dnl
AC_CANONICAL_HOST

dnl check for standard programs
dnl
AC_PROG_INSTALL
AC_PROG_MAKE_SET

dnl
dnl directory paths
dnl
case "$host_os" in
  mingw32)
    ROOT_DIR=`pwd -W`
  ;;
  *)
    ROOT_DIR=`pwd`
  ;;
esac
SRC_DIR=$ROOT_DIR/src
OUT_DIR=$ROOT_DIR/doc
MK_DIR=$ROOT_DIR/mk
SRC_CONFIG_DIR=$SRC_DIR/config
SRC_STYLES_DIR=$SRC_DIR/styles

AC_SUBST(ROOT_DIR)
AC_SUBST(OUT_DIR)
AC_SUBST(SRC_DIR)
AC_SUBST(MK_DIR)
AC_SUBST(SRC_CONFIG_DIR)
AC_SUBST(SRC_STYLES_DIR)

dnl
dnl ******************** check for documentation tools ********************
dnl
AC_PATH_PROGS([ASCIIDOC], [asciidoc], [none])
AC_PATH_PROGS([A2X], [a2x], [none])
AC_PATH_PROGS([HIGHLIGHTER], [pygmentize], [none])
AC_PATH_PROGS([DBLATEX], [dblatex], [none])

dnl
dnl error check for availability of tools
dnl
if test x"$ASCIIDOC" = xnone -o x"$A2X" = xnone ; then
  AC_MSG_ERROR([documentation generation requires asciidoc])
fi
if test x"$HIGHLIGHTER" = xnone ; then
  AC_MSG_ERROR([documentation generation requires a source highlighter tool])
fi

dnl
dnl get the path to the asciidoc cofiguration files
dnl
if test x"$ASCIIDOC" != xnone ; then
  AC_MSG_CHECKING([path to asciidoc configuration files])
  ASCIIDOC_CONF_DIR=`$ASCIIDOC -v /dev/null 2>&1 | head -n 1 | sed 's|.*reading: \(.*\)/asciidoc.conf|\1|'`
  AC_MSG_RESULT([$ASCIIDOC_CONF_DIR])
fi
AC_SUBST(ASCIIDOC)
AC_SUBST(A2X)
AC_SUBST(ASCIIDOC_CONF_DIR)
AC_SUBST(HIGHLIGHTER)
AC_SUBST(DBLATEX)

dnl
dnl ******************** write output ********************
dnl
AC_CONFIG_FILES(
dnl ***** Makefiles *****
  mk/doc-rules.gmk:config/doc-rules_gmk.in
  Makefile
  src/Makefile
  src/man/Makefile
)

AC_OUTPUT
