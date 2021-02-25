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
          Time.now())

    fun stop startT = Time.-(Time.now(), startT)

    (* convert a time value to a string, padded on the left to 8 characters *)
    fun timeToStr time = pad (Time.toString time, 6)

    fun output (strm, t) = TextIO.output (strm, timeToStr t)

  (* measure the compile time for a file *)
    fun timeUse (outstrm, file) = let
	  val t0 = start()
	  in
	    use file;
	    output (outstrm, stop t0)
	  end

  (* Time one run of the benchmark *)
    fun timeOne doit = let
	  val t0 = start()
	  in
	    doit();
	    stop t0
	  end

    fun timeIt (outstrm, doit) = let
	    val t = timeOne doit
	    in
	      TextIO.output1 (outstrm, #"\t");
	      output (outstrm, t);
	      TextIO.output1 (outstrm, #"\n")
	    end

  (* Time n runs of the benchmark *)
    fun time (n, outstrm, doit) = let
	  fun loop 0 = ()
	    | loop i = (
		output (outstrm, timeOne doit);
		if (i > 1) then TextIO.output (outstrm, ", ") else ();
		loop (i-1))
	  in
	    loop n
	  end

  end
