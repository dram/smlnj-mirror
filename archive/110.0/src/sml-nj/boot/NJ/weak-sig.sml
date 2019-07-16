(* weak-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)


signature WEAK = 
  sig

    type 'a weak
    val weak : 'a -> 'a weak
    val strong : 'a weak -> 'a option

    type weak'
    val weak' : 'a -> weak'
    val strong' : weak' -> bool

  end (* WEAK *)

signature SUSP =
  sig
    type 'a susp
    val delay : (unit -> 'a) -> 'a susp
    val force : 'a susp -> 'a
  end (* SUSP *)

(*
 * $Log: weak-sig.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:40  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.1  1997/02/11 20:44:48  george
 *   Version 109.25.1
 *
 * Revision 1.1.1.1  1997/01/14  01:38:18  george
 *   Version 109.24
 *
 *)
