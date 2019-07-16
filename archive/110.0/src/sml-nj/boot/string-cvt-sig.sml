(* string-cvt-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

signature STRING_CVT =
  sig

    datatype radix = BIN | OCT | DEC | HEX

    datatype realfmt
      = EXACT
      | SCI of int option
      | FIX of int option
      | GEN of int option

    type ('a, 'b) reader = 'b -> ('a * 'b) option

    val padLeft  : char -> int -> string -> string
    val padRight : char -> int -> string -> string

    val splitl : (char -> bool) -> (char, 'a) reader -> 'a -> (string * 'a)
    val takel  : (char -> bool) -> (char, 'a) reader -> 'a -> string
    val dropl  : (char -> bool) -> (char, 'a) reader -> 'a -> 'a
    val skipWS : (char, 'a) reader -> 'a -> 'a

    type cs
    val scanString : ((char, cs) reader -> ('a, cs) reader) -> string -> 'a option

  end


(*
 * $Log: string-cvt-sig.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:38  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.3  1997/06/02 19:15:03  jhr
 *   SML'97 Basis Library changes (phase 2)
 *
 * Revision 1.2  1997/05/29  14:44:27  jhr
 *   SML'97 Basis Library changes (phase 1)
 *
 * Revision 1.1.1.1  1997/01/14  01:38:17  george
 *   Version 109.24
 *
 *)
