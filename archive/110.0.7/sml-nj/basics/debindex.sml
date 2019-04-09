(* Copyright 1996 by AT&T Bell Laboratories *)
(* debindex.sml *)

signature DEB_INDEX = 
sig
  eqtype depth
  eqtype index

  val top  : depth
  val next : depth -> depth
  val prev : depth -> depth
  val eq   : depth * depth -> bool
  val cmp  : depth * depth -> order

  val calc : depth * depth -> index

  val di_print : index -> string
  val di_key : index -> int

  val di_toint: index -> int
  val di_fromint: int -> index
 
  val innermost : index
  val innersnd : index

end (* signature DEB_INDEX *)


structure DebIndex : DEB_INDEX = 
struct

local structure EM = ErrorMsg
in

fun bug s = EM.impossible ("Depth: " ^ s)

type depth = int
type index = int

val top = 0

fun next i = i + 1

fun prev i = if (i > 0) then i-1 else bug "negative depth in back"

fun eq (i:int, j) = (i=j)
val cmp = Int.compare

fun calc (cur:int, def) = 
  if def > cur then bug "the definition is deeper than the use"
  else (cur - def)

fun di_key i = i

fun di_print i = Int.toString i

fun di_toint (i : index) = i
fun di_fromint (i : int) = i

val innermost = 1
val innersnd = 2

end (* local *)
end (* structure DebIndex *)


(*
 * $Log: debindex.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:36  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.2  1997/10/19 23:43:39  dbm
 *   Added cmp function to signature.  cmp is the same as Int.compare, but
 *   is included for the sake of completeness (and potential abstraction
 *   of the type depth).
 *
 * Revision 1.1.1.1  1997/01/14  01:38:09  george
 *   Version 109.24
 *
 *)
