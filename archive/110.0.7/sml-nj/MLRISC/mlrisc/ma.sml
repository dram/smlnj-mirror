(* ma.sml --- graph coloring allocation of memory location
 * 
 * Copyright 1996 AT&T Bell Laboratories 
 *
 *)

signature MA_ARG = sig
  structure Liveness : LIVENESS
  structure InsnProps : INSN_PROPERTIES

  sharing Liveness.F.I = InsnProps.I


(*
 * $Log: ma.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:35  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.1.1.1  1997/04/19 18:14:20  george
 *   Version 109.27
 *
 *)
