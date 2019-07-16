(* bind-smlnj.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * This file binds the SML/NJ specific signatures and structures 
 * in the pervasive top-level environment.
 *
 *)

signature SIGNALS = SIGNALS
structure Signals = Signals

signature CLEAN_UP = CLEAN_UP
signature CONT = CONT
signature INTERVAL_TIMER = INTERVAL_TIMER
signature INTERNALS = INTERNALS
signature GC = GC
signature SUSP = SUSP
signature SYS_INFO = SYS_INFO
signature WEAK = WEAK
signature SML_OF_NJ = SML_OF_NJ

structure SMLofNJ : SML_OF_NJ =
  struct
    open SMLofNJ
    val exportML = Export.exportML
    val exportFn = Export.exportFn
    structure Cont = Cont
    structure IntervalTimer = IntervalTimer
    structure Internals = Internals
    structure Susp = Susp
    structure SysInfo = SysInfo
    structure Weak = Weak
  end;

signature UNSAFE_OBJECT = UNSAFE_OBJECT
signature POLL = POLL
signature UNSAFE_ARRAY = UNSAFE_ARRAY
signature UNSAFE_VECTOR = UNSAFE_VECTOR
signature UNSAFE_MONO_ARRAY = UNSAFE_MONO_ARRAY
signature UNSAFE_MONO_VECTOR = UNSAFE_MONO_VECTOR
signature UNSAFE = UNSAFE
structure Unsafe = Unsafe


(*
 * $Log: bind-smlnj.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:37  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.4  1997/06/30 19:36:15  jhr
 *   Removed System structure; added Unsafe structure.
 *
 * Revision 1.3  1997/03/03  17:10:39  george
 * moved callcc related functions to SMLofNJ.Cont
 *
 * Revision 1.2  1997/02/11  15:16:04  george
 * moved stuff from System to SMLofNJ
 *
 * Revision 1.1.1.1  1997/01/14  01:38:13  george
 *   Version 109.24
 *
 *)
