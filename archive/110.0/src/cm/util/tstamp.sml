structure TStamp = struct

    datatype t =
	NOTSTAMP
      | STABLETSTAMP of Time.time
      | TSTAMP of Time.time

    (*
     * If f1 depends on f2, then earlier (modtime f1, modtime f2) implies
     * that f1 needs to be recompiled...     *
     *)
    fun earlier (_, NOTSTAMP) = false	(* prerequisite missing *)
      | earlier (NOTSTAMP, _) = true	(* object missing *)
      | earlier (STABLETSTAMP _, _) = false (* object stable *)
      | earlier (TSTAMP t1, STABLETSTAMP t2) = Time.< (t1, t2)
      | earlier (TSTAMP t1, TSTAMP t2) = Time.< (t1, t2)

    local
	(* the timestamp is set to NOTSTAMP if the file doesn't exist *)
	fun modtime0 TS pn =
	    if AbsPath.exists pn then TS (AbsPath.modTime pn)
	    else NOTSTAMP
    in
	val modtime = modtime0 TSTAMP
	val stabletime = modtime0 STABLETSTAMP
    end
end
