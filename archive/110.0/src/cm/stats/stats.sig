(*
 * stats/stats.sig: timing statistics
 *
 *   Copyright (c) 1997 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
signature STATS = sig
    type phase
    type time = { cpu: Time.time, wall: Time.time }
    type stats = { total: time, subphases: time }

    val inPhase: phase -> ('a -> 'b) -> ('a -> 'b)

    val newPhase: string -> phase

    val allPhases: unit -> phase list
    val statsOf: phase -> stats
    val phaseName: phase -> string

    val withTiming: (string -> unit) -> ('a -> 'b) -> ('a -> 'b)
end
