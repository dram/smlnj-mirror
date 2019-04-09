(* array-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

signature ARRAY =
  sig
    type 'a array
    type 'a vector

    val maxLen   : int

    val array    : int * 'a -> 'a array
    val tabulate : int * (int -> 'a) -> 'a array
    val fromList : 'a list -> 'a array

    val length   : 'a array -> int
    val sub      : 'a array * int -> 'a
    val update   : 'a array * int * 'a -> unit
    val extract  : ('a array * int * int option) -> 'a vector

    val copy : {
	    src : 'a array, si : int, len : int option,
	    dst : 'a array, di : int
	  } -> unit
    val copyVec : {
	    src : 'a vector, si : int, len : int option,
	    dst : 'a array, di : int
	  } -> unit

    val app    : ('a -> unit) -> 'a array -> unit
    val foldl  : (('a * 'b) -> 'b) -> 'b -> 'a array -> 'b
    val foldr  : (('a * 'b) -> 'b) -> 'b -> 'a array -> 'b
    val modify : ('a -> 'a) -> 'a array -> unit

    val appi    : ((int * 'a) -> unit) -> ('a array * int * int option) -> unit
    val foldli  : ((int * 'a * 'b) -> 'b) -> 'b -> ('a array * int * int option) -> 'b
    val foldri  : ((int * 'a * 'b) -> 'b) -> 'b -> ('a array * int * int option) -> 'b
    val modifyi : ((int * 'a) -> 'a) -> ('a array * int * int option) -> unit

  end

(*
 * $Log: array-sig.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:37  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.1.1.1.2.1  1999/09/08 01:05:55  jhr
 *   Removed array0 from ARRAY signature.
 *
 * Revision 1.1.1.1  1997/01/14 01:38:12  george
 *   Version 109.24
 *
 *)
