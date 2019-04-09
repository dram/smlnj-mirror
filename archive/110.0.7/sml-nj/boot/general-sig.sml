(* general-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

signature PRE_GENERAL =
  sig

    type unit
    type exn

    exception Bind
    exception Match
    exception Subscript
    exception Size
    exception Overflow
    exception Chr
    exception Div
    exception Domain
    exception Span

    exception Fail of string

    datatype order = LESS | EQUAL | GREATER

    val !  : 'a ref -> 'a
    val := : ('a ref * 'a) -> unit

    val o      : ('b -> 'c) * ('a -> 'b) -> ('a -> 'c)
    val before : ('a * unit) -> 'a
    val ignore : 'a -> unit

  end

signature GENERAL = 
  sig
    include PRE_GENERAL

    val exnName : exn -> string
    val exnMessage: exn -> string

  end


(*
 * $Log: general-sig.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:38  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.5  1997/11/25 22:40:18  jhr
 *   SML Basis changes: removed <> and = from General and changed the type
 *   of General.before.
 *
 * Revision 1.4  1997/06/02  19:15:00  jhr
 *   SML'97 Basis Library changes (phase 2)
 *
 * Revision 1.3  1997/05/29  14:44:21  jhr
 *   SML'97 Basis Library changes (phase 1)
 *
 * Revision 1.2  1997/02/26  21:00:05  george
 *    Defined a new top level Option structure. All 'a option related
 *    functions have been moved out of General.
 *
 * Revision 1.1.1.1  1997/01/14  01:38:14  george
 *   Version 109.24
 *
 *)
