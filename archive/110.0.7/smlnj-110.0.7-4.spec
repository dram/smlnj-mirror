# smlnj.spec
#
# COPYRIGHT (c) 2000 Bell Labs, Lucent Technologies.
#
# RPM spec file for binary distribution of SML/NJ system.
#
Summary:        Standard ML of New Jersey
Name:           smlnj
Version:        110.0.7
Release:        4
Copyright:      MIT
Distribution:	SML/NJ
Group:          Development/Languages
Packager:	Dave MacQueen

%description
Standard ML of New Jersey.
  SML/NJ is an interactive compiler for the Standard ML Programming
  Language (1997 Revision).
  This is a summary package which includes binaries for the compiler
  and library sources.

%files
%dir /usr/share/smlnj
%dir /usr/share/smlnj/bin
%dir /usr/share/smlnj/bin/.heap
%dir /usr/share/smlnj/bin/.run
%dir /usr/share/smlnj/lib
%dir /usr/share/smlnj/src
%dir /usr/share/smlnj/src/cml
%dir /usr/share/smlnj/src/ml-yacc
%dir /usr/share/smlnj/src/smlnj-lib
/usr/bin/ml-burg
/usr/bin/ml-lex
/usr/bin/ml-yacc
/usr/bin/sml
/usr/bin/sml-cm
/usr/share/smlnj/110-PATCH-HISTORY
/usr/share/smlnj/110-INSTALL
/usr/share/smlnj/110-README
/usr/share/smlnj/110-README.html
/usr/share/smlnj/bin/.arch-n-opsys
/usr/share/smlnj/bin/.heap/ml-burg.x86-linux
/usr/share/smlnj/bin/.heap/ml-lex.x86-linux
/usr/share/smlnj/bin/.heap/ml-yacc.x86-linux
/usr/share/smlnj/bin/.heap/sml-cm.x86-linux
/usr/share/smlnj/bin/.heap/sml.x86-linux
/usr/share/smlnj/bin/.run-sml
/usr/share/smlnj/bin/.run/run.x86-linux
/usr/share/smlnj/bin/ml-burg
/usr/share/smlnj/bin/ml-lex
/usr/share/smlnj/bin/ml-yacc
/usr/share/smlnj/bin/sml
/usr/share/smlnj/bin/sml-cm
/usr/share/smlnj/lib/cml-lib.cm
/usr/share/smlnj/lib/cml.cm
/usr/share/smlnj/lib/eXene.cm
/usr/share/smlnj/lib/html-lib.cm
/usr/share/smlnj/lib/inet-lib.cm
/usr/share/smlnj/lib/ml-yacc-lib.cm
/usr/share/smlnj/lib/pp-lib.cm
/usr/share/smlnj/lib/reactive-lib.cm
/usr/share/smlnj/lib/regexp-lib.cm
/usr/share/smlnj/lib/smlnj-lib.cm
/usr/share/smlnj/lib/unix-lib.cm
/usr/share/smlnj/src/cml/cml-lib
/usr/share/smlnj/src/cml/src
/usr/share/smlnj/src/eXene
/usr/share/smlnj/src/ml-yacc/lib
/usr/share/smlnj/src/smlnj-lib/HTML
/usr/share/smlnj/src/smlnj-lib/INet
/usr/share/smlnj/src/smlnj-lib/PP
/usr/share/smlnj/src/smlnj-lib/README
/usr/share/smlnj/src/smlnj-lib/Reactive
/usr/share/smlnj/src/smlnj-lib/RegExp
/usr/share/smlnj/src/smlnj-lib/Unix
/usr/share/smlnj/src/smlnj-lib/Util
