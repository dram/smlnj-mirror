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
 * $Log$
 * Revision 1.1  2001/10/11 09:52:26  macqueen
 * Initial revision
 *
 * Revision 1.1.1.1  1998/04/08 18:39:02  george
 * Version 110.5
 *
 *)
