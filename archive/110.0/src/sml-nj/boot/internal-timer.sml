(* internal-timer.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

structure InternalTimer : sig

    include TIMER
    val resetTimers : unit -> unit

  end = struct

    structure PB = PreBasis

    datatype cpu_timer = CPUT of {
	usr : PB.time, sys : PB.time, gc : PB.time
      }
    datatype real_timer = RealT of PB.time

    local
      val gettime' : unit -> (Int32.int * int * Int32.int * int * Int32.int * int) =
	    CInterface.c_function "SMLNJ-Time" "gettime"
      fun mkTime (s, us) = PB.TIME{sec=Int32.toLarge s, usec=Int.toLarge us}
    in
    fun getTime () = let val (ts, tu, ss, su, gs, gu) = gettime' ()
	  in
	    { usr = mkTime(ts, tu), sys = mkTime(ss, su), gc = mkTime(gs, gu) }
	  end
    end (* local *)

    fun startCPUTimer () = CPUT(getTime())
    fun checkCPUTimer (CPUT{usr=u0, sys=s0, gc=g0}) = let
	  val {usr, sys, gc} = getTime()
	  in
	    { usr = Time.-(usr, u0),
	      sys = Time.-(sys, s0),
	      gc = Time.-(gc, g0)
	    }
	  end
    val initCPUTime = ref(startCPUTimer ())
    fun totalCPUTimer () = !initCPUTime

    fun startRealTimer () = RealT(Time.now())
    fun checkRealTimer (RealT t) = Time.-(Time.now(), t)
    val initRealTime = ref(startRealTimer ())
    fun totalRealTimer () = !initRealTime

    fun resetTimers () = (
	  initCPUTime := startCPUTimer ();
	  initRealTime := startRealTimer ())

  end (* Timer *)


(*
 * $Log: internal-timer.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:38  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.1  1998/01/15 20:06:56  jhr
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
