(* timer.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

structure Timer : TIMER = InternalTimer;

(*
 * $Log: timer.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:39  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.3  1998/01/15 20:06:57  jhr
 *   Reorganized timer initialization code.  New structure InternalTimer
 *   exposes resetTimers function, which is exported in SMLofNJ.Internals
 *   and is used in the new structure CleanTimer.
 *
 * Revision 1.2  1997/07/31  17:25:01  jhr
 *   We are now using 32-bit ints to represent the seconds portion of a
 *   time value.  This was required to handle the change in the type of
 *   Time.{to,from}{Seconds,Milliseconds,Microseconds}.
 *
 * Revision 1.1.1.1  1997/01/14  01:38:17  george
 *   Version 109.24
 *
 *)
