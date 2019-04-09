(* os2-process-sig.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

(* os2-process-sig.sml
 *
 * Signature for the OS/2 process interface.
 *
 * Peter Bertelsen, August 1995.
 *)

signature OS2_PROCESS =
  sig

    val system  : string -> int
    val exit    : int -> 'a
    val scanEnv : string -> string option

  end (* signature OS2_PROCESS *)


(*
 * $Log: os2-process-sig.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:41  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.1.1.1  1997/01/14 01:38:22  george
 *   Version 109.24
 *
 *)
