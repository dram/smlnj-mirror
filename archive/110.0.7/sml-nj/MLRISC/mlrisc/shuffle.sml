(* shuffle.sml -- implements the parallel copy instruction as a sequence
 *		of moves. Makes use of asmTmpR from CELLS.
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

functor Shuffle(C:CELLS) =
struct

  fun shuffle(mvInstr, regMap, tmpR, rds, rss) = let
    (* perform unconstrained moves *)
    fun loop((p as (rd,rs))::rest, changed, used, acc, instrs) = 
	if List.exists (fn (r:int) => r = rd) used then
	   loop(rest, changed, used, p::acc, instrs)
	else loop(rest, true, used, acc, mvInstr(rd, rs)::instrs)
      | loop([], changed, _, acc, instrs) = (changed, acc, instrs)

    fun cycle([], instrs) = instrs
      | cycle(moves, instrs) = 
	(case loop(moves, false, map #2 moves, [], instrs)
	  of (_, [], instrs) => instrs
	   | (true, acc, instrs) => cycle(acc, instrs)
	   | (false, (rd,rs)::acc, instrs) => let
	       fun rename(p as (a,b)) = if b=rd then (a, tmpR) else p
	       val acc' = (rd, rs) :: map rename acc
	       val instrs' = mvInstr(tmpR, rd)::instrs
	       val (_, acc'', instrs'') = 
		 loop(acc', false, map #2 acc', [], instrs')
	     in cycle(acc'', instrs'')
	     end
	(*esac*))

    (* remove moves that have been coalesced. *)
    fun rmvCoalesced(rd::rds, rs::rss) = let
	  val rd' = regMap rd
	  val rs' = regMap rs
	in
	  if rd' = rs' then rmvCoalesced(rds, rss)
	  else (rd', rs')::rmvCoalesced(rds, rss)
	end
      | rmvCoalesced([], []) = []
  in 
    rev (cycle (rmvCoalesced(rds, rss), []))
  end
end


(*
 * $Log: shuffle.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:35  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.2  1997/07/03 13:55:56  george
 *   The shuffle routine now takes a temporary register so that it can be
 *   used for both general purpose and floating point shuffling.
 *
# Revision 1.1.1.1  1997/04/19  18:14:21  george
#   Version 109.27
#
 *)
