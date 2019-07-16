(* string-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

signature STRING =
  sig
    type string

    val maxSize : int

    val size      : string -> int
    val sub       : (string * int) -> char
    val substring : (string * int * int) -> string
    val extract   : (string * int * int option) -> string
    val concat    : string list -> string
    val ^         : (string * string) -> string
    val str       : char -> string
    val implode   : char list -> string
    val explode   : string -> char list

    val fromString  : string -> string option
    val toString    : string -> string
    val fromCString : string -> string option
    val toCString   : string -> string

    val map       : (char -> char) -> string -> string
    val translate : (char -> string) -> string -> string
    val tokens    : (char -> bool) -> string -> string list
    val fields    : (char -> bool) -> string -> string list

    val isPrefix : string -> string -> bool
    val compare  : (string * string) -> order
    val collate  : ((char * char) -> order) -> (string * string) -> order

    val <= : (string * string) -> bool
    val <  : (string * string) -> bool
    val >= : (string * string) -> bool
    val >  : (string * string) -> bool

  end

(*
 * $Log: string-sig.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:39  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.5  1997/07/07 18:06:34  jhr
 *   Added extract function to String (moved the implementation from CharVector).
 *
 * Revision 1.4  1997/06/02  19:15:04  jhr
 *   SML'97 Basis Library changes (phase 2)
 *
 * Revision 1.3  1997/05/29  14:44:27  jhr
 *   SML'97 Basis Library changes (phase 1)
 *
 * Revision 1.2  1997/03/19  20:08:49  george
 *   bugfix for 1108 -- String.maxSize missnamed as String.maxLen
 *
 * Revision 1.1.1.1  1997/01/14  01:38:17  george
 *   Version 109.24
 *
 *)
