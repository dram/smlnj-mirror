(* open-general.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * Open the General structure to give other basis modules access to its
 * contents, and also make the option type available at top-level.
 *
 *)

open General;

val op =  : ''a * ''a -> bool = InlineT.=;
val op <> : ''a * ''a -> bool = InlineT.<>;

datatype option = datatype Assembly.option;


(*
 * $Log: open-general.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:38  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.3  1997/11/25 22:40:19  jhr
 *   SML Basis changes: removed <> and = from General and changed the type
 *   of General.before.
 *
 * Revision 1.2  1997/02/26  21:00:07  george
 *    Defined a new top level Option structure. All 'a option related
 *    functions have been moved out of General.
 *
 * Revision 1.1.1.1  1997/01/14  01:38:15  george
 *   Version 109.24
 *
 *)
