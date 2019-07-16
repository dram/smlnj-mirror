(* os2-sig.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

(* os2-sig.sml
 *
 * Signature for the SML/NJ interface to OS/2.
 *
 * Peter Bertelsen, August 1995.
 *)

signature OS2 =
  sig

    structure FileSys : OS2_FILESYS
    structure IO      : OS2_IO
    structure Process : OS2_PROCESS
    sharing type FileSys.offset = IO.offset = Offset.int

  end (* signature OS2 *)


(*
 * $Log: os2-sig.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:41  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.1.1.1  1997/01/14 01:38:22  george
 *   Version 109.24
 *
 *)
