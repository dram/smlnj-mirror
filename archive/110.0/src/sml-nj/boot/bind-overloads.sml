(* bind-overloads.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * This file defines the infix definitions and overloadings in the
 * pervasive top-level environment.
 *
 *)

infix 7  * / mod div
infix 6 ^ + -
infix 3 := o
infix 4 > < >= <= = <>
infixr 5 :: @
infix 0 before

_overload ~ :   ('a -> 'a)
  as  Int.~ and Int32.~
  and Real.~
_overload + :   ('a * 'a -> 'a)
  as  Int.+  and Int32.+
  and Word8.+ and Word31.+ and Word32.+
  and Real.+ 
_overload - :   ('a * 'a -> 'a)
  as  Int.- and Int32.-
  and Word8.- and Word31.- and Word32.-
  and Real.-
_overload * :   ('a * 'a -> 'a)
  as  Int.* and Int32.*
  and Word8.* and Word31.* and Word32.*
  and Real.*
_overload / : ('a * 'a -> 'a)
  as Real./
_overload div : ('a * 'a -> 'a)
  as  Int.div and Int32.div
  and Word8.div and Word31.div and Word32.div
_overload mod : ('a * 'a -> 'a)
  as  Int.mod and Int32.mod
  and Word8.mod and Word31.mod and Word32.mod
_overload < :   ('a * 'a -> bool)
  as  Int.< and Int32.<
  and Word8.< and Word31.< and Word32.<
  and Real.<
  and Char.<
  and String.<
_overload <= :   ('a * 'a -> bool)
  as  Int.<= and Int32.<=
  and Word8.<= and Word31.<= and Word32.<=
  and Real.<=
  and Char.<=
  and String.<=
_overload > :   ('a * 'a -> bool)
  as  Int.> and Int32.>
  and Word8.> and Word31.> and Word32.>
  and Real.>
  and Char.>
  and String.>
_overload >= :   ('a * 'a -> bool)
  as  Int.>= and Int32.>=
  and Word8.>= and Word31.>= and Word32.>=
  and Real.>=
  and Char.>=
  and String.>=
_overload abs : ('a -> 'a)
  as  Int.abs and Int32.abs
  and Real.abs


(*
 * $Log: bind-overloads.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:37  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.3  1997/08/26 19:25:28  jhr
 *   Keyword clean-up: abstraction is gone; overload is _overload; lazy is _lazy.
 *
 * Revision 1.2  1997/07/15  15:51:26  dbm
 *   Rearranged order of variants to get correct defaults.
 *
 * Revision 1.1.1.1  1997/01/14  01:38:12  george
 *   Version 109.24
 *
 *)
