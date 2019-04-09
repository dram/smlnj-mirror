(* system.sig
 *
 * Copyright 1989 by AT&T Bell Laboratories.
 *
 * This signature will soon go away, once Tags has moved into the
 * machine specification in the compiler.
 *)

signature SYSTEM =
  sig
    structure Tags : TAGS
  end


(*
 * $Log: system.sig,v $
 * Revision 1.1.1.1  1999/12/03 19:59:39  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.3  1997/06/30 19:36:20  jhr
 *   Removed System structure; added Unsafe structure.
 *
 * Revision 1.2  1997/02/11  15:16:09  george
 * moved stuff from System to SMLofNJ
 *
 * Revision 1.1.1.1  1997/01/14  01:38:17  george
 *   Version 109.24
 *
 *)
