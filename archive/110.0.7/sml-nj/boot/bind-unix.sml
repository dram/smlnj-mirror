(* bind-unix.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * This file binds the Unix specific structures in the pervasive
 * top-level environment.
 *
 *)

signature UNIX_SIGNALS = UNIX_SIGNALS
structure UnixSignals = UnixSignals
signature UNIX = UNIX
structure Unix = Unix

(*
 * $Log: bind-unix.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:37  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.1.1.1  1997/01/14 01:38:13  george
 *   Version 109.24
 *
 *)
