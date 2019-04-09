(* bind-old.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * This file binds various obsolete structures in the pervasive top-level
 * environment.  It should go away some day soon.
 *
 *)

(* system.sig *)
signature TAGS = TAGS
signature CINTERFACE = CINTERFACE
signature UNSAFE = UNSAFE
signature SYSTEM = SYSTEM

structure System = System


(*
 * $Log: bind-old.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:37  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.3  1997/05/29 14:44:17  jhr
 *   SML'97 Basis Library changes (phase 1)
 *
 * Revision 1.2  1997/02/11  15:16:03  george
 * moved stuff from System to SMLofNJ
 *
 * Revision 1.1.1.1  1997/01/14  01:38:12  george
 *   Version 109.24
 *
 *)
