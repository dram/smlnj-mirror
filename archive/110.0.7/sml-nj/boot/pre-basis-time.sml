(* pre-basis-time.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * This is the basis Time structure with only the time type, so that the
 * basis signatures can compile.  It has to be in a separate file from
 * pre-basis-structs.sml, since it depends on the binding of LargeInt.
 *)

structure Time =
  struct
    datatype time = TIME of {sec : LargeInt.int, usec : LargeInt.int}
  end;

(*
 * $Log: pre-basis-time.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:38  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.1  1997/07/31 17:27:14  jhr
 *   We are now using 32-bit ints to represent the seconds portion of a
 *   time value.  This was required to handle the change in the type of
 *   Time.{to,from}{Seconds,Milliseconds,Microseconds}.
 *
 *)
