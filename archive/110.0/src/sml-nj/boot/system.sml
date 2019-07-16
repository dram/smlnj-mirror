(* system.sml
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *
 * This structure will soon go away, once Tags has moved into the
 * machine specification in the compiler.
 *)

structure System :> SYSTEM =
  struct
    structure Tags : TAGS = Tags
  end

(*
 * $Log: system.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:39  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.4  1997/06/30 19:36:21  jhr
 *   Removed System structure; added Unsafe structure.
 *
 * Revision 1.3  1997/05/29  14:44:28  jhr
 *   SML'97 Basis Library changes (phase 1)
 *
 * Revision 1.2  1997/02/11  15:16:11  george
 * moved stuff from System to SMLofNJ
 *
 * Revision 1.1.1.1  1997/01/14  01:38:17  george
 *   Version 109.24
 *
 *)
