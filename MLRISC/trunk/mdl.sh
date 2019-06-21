#!/bin/sh
#
# COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
# All rights reserved.
#
# A script for running the MDL tool on a specification file.
#
# usage: mdl.sh <file>
#

TARGET=$1

sml <<XXXX
val _ = List.app (fn f => #set(CM.Anchor.anchor f) (SOME "cm")) [
	  "Control.cm", "Lib.cm", "Graphs.cm", "MLRISC.cm", "MLTREE.cm"
	];
CM.make "Tools/MDL/sources.cm";
fun gen f = MDLGen.gen(f ^ "/" ^ f ^ ".mdl");
gen "$TARGET"
XXXX
