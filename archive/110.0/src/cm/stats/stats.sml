(*
 * stats/stats.sml: timing statistics
 *
 *   Copyright (c) 1997 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
structure Stats :> STATS = struct

    type time = { cpu: Time.time, wall: Time.time }
    type stats = { total: time, subphases: time }
    type phase = { name: string, stats: stats ref }

    local
	val zeroTime = { cpu = Time.zeroTime, wall = Time.zeroTime }
	val zeroStats = { total = zeroTime, subphases = zeroTime }
	val dummyPhase = { name = "Dummy", stats = ref zeroStats }
	val topPhase = { name = "Compilation management",
			 stats = ref zeroStats }
	val phases = ref [topPhase]
	val maxNameLen = ref (size (#name topPhase))
	val phaseStack: (phase * phase list) ref = ref (dummyPhase, [])

	fun addTime ({ cpu = c1, wall = w1 } , { cpu = c2, wall = w2 }) =
	    { cpu = Time.+ (c1, c2), wall = Time.+ (w1, w2) }

	fun addToTotal t { name, stats } = let
	    val { total, subphases } = !stats
	in
	    stats := { total = addTime (t, total), subphases = subphases }
	end

	fun addToSub t { name, stats } = let
	    val { total, subphases } = !stats
	in
	    stats := { total = total, subphases = addTime (t, subphases) }
	end

	fun inPhase' phase f x = let
	    val oldStack as (cur, prev) = !phaseStack
	    val _ = phaseStack := (phase, cur :: prev)
	    val cpuT = Timer.startCPUTimer ()
	    val wallT = Timer.startRealTimer ()
	    fun record () = let
		val { usr, sys, gc } = Timer.checkCPUTimer cpuT
		val cpu = Time.+ (usr, Time.+ (sys, gc))
		val wall = Timer.checkRealTimer wallT
		val t = { cpu = cpu, wall = wall }
	    in
		phaseStack := oldStack;
		addToTotal t phase;
		addToSub t cur
	    end		
	    val result = f x
		handle exn => (record (); raise exn)
	in
	    record ();
	    result
	end

	val timingOn = ref false

	fun reset () =
	    app (fn { name, stats } => stats := zeroStats) (!phases)

	fun report pr = let
	    val padded = StringCvt.padRight #" " (!maxNameLen)
	    fun one { name, stats = ref { total, subphases } } = let
		fun k (label, total, subphases) = let
		    val here = Time.- (total, subphases)
		in
		    concat [Time.toString here, "+",
			    Time.toString subphases, " ", label]
		end
		val { cpu = tc, wall = tw } = total
		val { cpu = sc, wall = sw } = subphases
	    in
		pr (concat ["\t", padded name, ": ", k ("CPU", tc, sc), " ",
			    k ("wall clock", tw, sw), "\n"])
	    end
	in
	    pr "Timing statistics:\n";
	    app one (!phases)
	end
    in
	fun newPhase name = let
	    val phase = { name = name, stats = ref zeroStats }
	    val _ = phases := phase :: (!phases)
	    val _ = maxNameLen := Int.max (!maxNameLen, size name)
	in
	    phase
	end

	fun inPhase phase f x =
	    if !timingOn then inPhase' phase f x else f x

	fun allPhases () = !phases
	fun statsOf { name, stats } = !stats
	fun phaseName { name, stats } = name

	fun withTiming pr f x = let
	    fun finish () = let
		val _ = timingOn := false
		val _ = phaseStack := (dummyPhase, [])
	    in
		report pr
	    end
	    fun work () =
		(timingOn := true;
		 phaseStack := (dummyPhase, []);
		 reset ();
		 inPhase' topPhase f x)
	in
	    (work () before finish ()) handle exn => (finish (); raise exn)
	end
    end
end
