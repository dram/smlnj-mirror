(* general.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

structure General : PRE_GENERAL =
  struct

    type unit = PrimTypes.unit
    type exn = PrimTypes.exn

    exception Bind = Core.Bind
    exception Match = Core.Match
    exception Subscript = Core.Subscript
    exception Size = Core.Size
    exception Overflow = Assembly.Overflow
    exception Chr = InlineT.Char.Chr
    exception Div = Assembly.Div
    exception Domain
    exception Span

    exception Fail of string

    datatype order = LESS | EQUAL | GREATER

    val ! = InlineT.!
    val op := = InlineT.:=

(*
    fun f o g = fn x => f(g x)
    fun a before b = a
*)
    val op o : ('b -> 'c) * ('a -> 'b) -> ('a -> 'c) = InlineT.compose
    val op before : ('a * unit) -> 'a = InlineT.before
    fun ignore _ = ()

  end (* structure General *)

(*
 * $Log: general.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:38  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.5  1997/11/25 22:40:18  jhr
 *   SML Basis changes: removed <> and = from General and changed the type
 *   of General.before.
 *
 * Revision 1.4  1997/06/02  19:15:01  jhr
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
