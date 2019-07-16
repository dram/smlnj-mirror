(* interval-timer-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * An interface to system interval timers.
 *
 *)

signature INTERVAL_TIMER =
  sig

    val tick : unit -> Time.time
	(* the minimum interval that the interval timers support *)

    val setIntTimer : Time.time option -> unit
	(* set the interval timer; NONE means to disable the timer. *)

  end;

(*
 * $Log: interval-timer-sig.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:40  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.1.1.1  1997/01/14 01:38:20  george
 *   Version 109.24
 *
 *)
