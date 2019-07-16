(* bool-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

signature BOOL =
  sig

    datatype bool = true | false
    val not : bool -> bool

    val toString   : bool -> string
    val fromString : string -> bool option
    val scan : (char, 'a) StringCvt.reader -> (bool, 'a) StringCvt.reader

  end


(*
 * $Log: bool-sig.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:37  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.1.1.1  1997/01/14 01:38:13  george
 *   Version 109.24
 *
 *)
