(* time-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

signature TIME =
  sig

    eqtype time

    exception Time

    val zeroTime : time

    val fromReal : real -> time
    val toReal   : time -> real

    val toSeconds        : time -> LargeInt.int
    val fromSeconds      : LargeInt.int -> time
    val toMilliseconds   : time -> LargeInt.int
    val fromMilliseconds : LargeInt.int -> time
    val toMicroseconds   : time -> LargeInt.int
    val fromMicroseconds : LargeInt.int -> time

    val +  : (time * time) -> time
    val -  : (time * time) -> time

    val compare : (time * time) -> order

    val <  : (time * time) -> bool
    val <= : (time * time) -> bool
    val >  : (time * time) -> bool
    val >= : (time * time) -> bool

    val now : unit -> time

    val toString   : time -> string
    val fromString : string -> time option
    val fmt : int -> time -> string
    val scan : (char, 'a) StringCvt.reader -> (time, 'a) StringCvt.reader

  end (* TIME *)

(*
 * $Log: time-sig.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:39  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.2  1997/07/31 17:25:00  jhr
 *   We are now using 32-bit ints to represent the seconds portion of a
 *   time value.  This was required to handle the change in the type of
 *   Time.{to,from}{Seconds,Milliseconds,Microseconds}.
 *
 * Revision 1.1.1.1  1997/01/14  01:38:17  george
 *   Version 109.24
 *
 *)
