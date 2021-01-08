(* timeit.sml
 *
 * COPYRIGHT (c) 2021 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)


signature BMARK =
  sig
    val doit : unit -> unit
    val testit : TextIO.outstream -> unit
  end;

structure Timing : sig

    val timeUse : TextIO.outstream * string -> unit

    val timeIt : TextIO.outstream * (unit -> 'a) -> unit

    val time : int * TextIO.outstream * (unit -> 'a) -> unit

  end = struct

    structure TR = Timer
    structure T = Time

    type timing = {usr:T.time, gc:T.time, sys:T.time, real:T.time}

    fun pad (s, n) = StringCvt.padLeft #" " n s

    fun start () = (
	  SMLofNJ.Internals.GC.doGC 1000;
         {realt = TR.startRealTimer(), timer = TR.startCPUTimer()})

    fun stop {realt, timer} = let
          val rt = TR.checkRealTimer realt
          val {usr, sys} = TR.checkCPUTimer timer
	  val gc = TR.checkGCTime timer
	  in
	    {usr=usr, gc=gc, sys=sys, real=rt}
	  end

    (* convert a time value to a string, padded on the left to 8 characters *)
    fun timeToStr time = pad (Time.toString time, 6)

    fun output (strm, {usr, gc, sys, real} : timing) = let
	  val str = concat[
		  "{usr = ", timeToStr usr,
		  ", sys = ", timeToStr sys,
		  ", gc = ", timeToStr gc,
		  ", real = ", timeToStr real, "}"
		]
	  in
	    TextIO.output (strm, str)
	  end

  (* measure the compile time for a file *)
    fun timeUse (outstrm, file) = let
	  val t0 = start()
	  in
	    use file;
	    output (outstrm, stop t0);
	    TextIO.output1 (outstrm, #"\n")
	  end

  (* Time one run of the benchmark *)
    fun timeIt (outstrm, doit) = let
	  val t0 = start()
	  in
	    doit();
	    TextIO.output1 (outstrm, #"\t");
	    output (outstrm, stop t0);
	    TextIO.flushOut outstrm
	  end

  (* Time n runs of the benchmark *)
    fun time (n, outstrm, doit) = let
	  fun loop 0 = ()
	    | loop 1 = timeIt(outstrm, doit)
	    | loop i = (
		timeIt(outstrm, doit);
		TextIO.output(outstrm, ",\n");
		loop(i-1))
	  in
	    TextIO.output (outstrm, "    Runs=[\n");
	    loop n;
	    TextIO.output (outstrm, "      ]")
	  end

  end
