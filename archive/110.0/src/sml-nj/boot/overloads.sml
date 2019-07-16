(* overloads.sml
 *
 * Copyright (c) 1989 by AT&T Bell Laboratories 
 *
 * Top-level bindings and overloadings.
 *)

type int = Int.int
type real = Real.real
type string = String.string
type substring = Substring.substring

val floor = Real.floor
val size = String.size
val op ^ = String.^
val substring = String.substring
val concat = String.concat

_overload ~ :   ('a -> 'a)        as Int.~   and Real.~
_overload + :   ('a * 'a -> 'a)
  as Int.+ and LargeInt.+
 and Real.+
 (*and Word32.+ *)
_overload - :   ('a * 'a -> 'a)
  as Int.- and LargeInt.-
 and Real.-
 (*and Word32.- *)
_overload * :   ('a * 'a -> 'a)
  as Int.* and LargeInt.*
 and Real.*
_overload div : ('a * 'a -> 'a)   as Int.div
_overload / :   ('a * 'a -> 'a)   as Real./
_overload < :   ('a * 'a -> bool)
  as Int.< and LargeInt.<
 and Real.<
 (*and Word32.< *)
 and String.<
_overload <= :   ('a * 'a -> bool)
  as Int.<= and LargeInt.<=
 and Real.<=
 (*and Word32.<= *)
 and String.<=
_overload > :   ('a * 'a -> bool)
  as Int.> and LargeInt.>
 and Real.>
 (*and Word32.> *)
 and String.>
_overload >= :   ('a * 'a -> bool)
  as Int.>= and LargeInt.>=
 and Real.>=
 (*and Word32.>= *)
 and String.>=

(*
 * $Log: overloads.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:38  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.4  1997/08/26 19:25:28  jhr
 *   Keyword clean-up: abstraction is gone; overload is _overload; lazy is _lazy.
 *
 * Revision 1.3  1997/07/31  17:24:58  jhr
 *   We are now using 32-bit ints to represent the seconds portion of a
 *   time value.  This was required to handle the change in the type of
 *   Time.{to,from}{Seconds,Milliseconds,Microseconds}.
 *
 * Revision 1.2  1997/05/29  14:44:23  jhr
 *   SML'97 Basis Library changes (phase 1)
 *
 * Revision 1.1.1.1  1997/01/14  01:38:15  george
 *   Version 109.24
 *
 *)
