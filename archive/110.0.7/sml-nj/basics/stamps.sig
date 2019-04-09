(* Copyright 1996 by AT&T Bell Laboratories *)
(* stamps.sig *)

signature STAMPS =
sig

  datatype stamp_scope
    = LOCAL
    | GLOBAL of PersStamps.persstamp
    | SPECIAL of string

  datatype stamp = STAMP of {scope : stamp_scope, count : int}

  val new : unit -> unit -> stamp  (* a generator of the stamp-generator *)
  val eq : stamp * stamp -> bool
  val cmp : stamp * stamp -> order
  val special : string -> stamp
  val stampToString : stamp -> string
  val stampToShortString : stamp -> string

  type 'a stampMap
  val newMap : exn -> '1a stampMap

  (* updateMap - add mapping to a stampMap *)
  val updateMap : '2a stampMap -> stamp * '2a -> unit

  (* applyMap - apply stampMap to a stamp *)
  val applyMap : 'a stampMap * stamp -> 'a

end (* signature STAMPS *)

(*
 * $Log: stamps.sig,v $
 * Revision 1.1.1.1  1999/12/03 19:59:36  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.1.1.1  1997/01/14 01:38:11  george
 *   Version 109.24
 *
 *)
