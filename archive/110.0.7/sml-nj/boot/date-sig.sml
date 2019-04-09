(* date-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

signature DATE =
  sig

    datatype weekday = Mon | Tue | Wed | Thu | Fri | Sat | Sun

    datatype month
      = Jan | Feb | Mar | Apr | May | Jun
      | Jul | Aug | Sep | Oct | Nov | Dec

    type date

    exception Date
	(* raised on errors, as described below *)

    val year    : date -> int
	(* returns the year (e.g., 1997) *)
    val month   : date -> month
	(* returns the month *)
    val day     : date -> int
	(* returns the day of the month *)
    val hour    : date -> int
	(* returns the hour *)
    val minute  : date -> int
	(* returns the minute *)
    val second  : date -> int
	(* returns the second *)
    val weekDay : date -> weekday
	(* returns the day of the week *)
    val yearDay : date -> int
	(* returns the day of the year *)
    val isDst   : date -> bool option
	(* returns SOME(true) if daylight savings time is in effect; returns
	 * SOME(false) if not, and returns NONE if we don't know.
	 *)
    val offset  : date -> Time.time option
	(* return time west of UTC.  NONE is localtime, SOME(Time.zeroTime)
	 * is UTC.
	 *)

    val date : {
	    year   : int,
	    month  : month,
	    day    : int,
	    hour   : int,
	    minute : int,
	    second : int,
	    offset : Time.time option
	  } -> date
	(* creates a date from the given values. *)

    val fromTimeLocal : Time.time -> date
	(* returns the date for the given time in the local timezone.
	 * this is like the ANSI C function localtime.
	 * was: fromTime
	 *)
    val fromTimeUniv  : Time.time -> date
	(* returns the date for the given time in the UTC timezone.
	 * this is like the ANSI C function gmtime.
	 * was: fromUTC
	 *)
    val toTime   : date -> Time.time
	(* returns the time value corresponding to the date in the
	 * host system.  This raises Date exception if the date cannot
	 * be represented as a time value.
	 *)

    val toString   : date -> string
    val fmt        : string -> date -> string
(** not yet implemented **
    val fromString : string -> date option
    val scan       : (char, 'a) StringCvt.reader -> (date, 'a) StringCvt.reader
**)

    val compare : (date * date) -> order
	(* returns the relative order of two dates. *)

  end;

(*
 * $Log: date-sig.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:38  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.3  1997/09/22 16:09:56  riccardo
 *   correction to Date.compare function
 *
 * Revision 1.2  1997/09/08  16:33:46  jhr
 *   Partial implementation of new Date structure API.
 *
 * Revision 1.1.1.1  1997/01/14  01:38:14  george
 *   Version 109.24
 *
 *)
