(* os2.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

(* os2.sml
 *
 * This is the SML/NJ interface to OS/2.
 *
 * Peter Bertelsen, August 1995.
 *)

structure OS2 : OS2 =
  struct

    structure FileSys = OS2_FileSys
    structure IO      = OS2_IO
    structure Process = OS2_Process

  end (* structure OS2 *)


(*
 * $Log: os2.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:41  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.1.1.1  1997/01/14 01:38:22  george
 *   Version 109.24
 *
 *)
