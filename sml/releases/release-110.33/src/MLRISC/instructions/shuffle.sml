(* shuffle.sml -- implements the parallel copy instruction as a sequence
 *		of moves. 
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)


functor Shuffle(I : INSTRUCTIONS) :
  sig
    val shuffle : 
      {mvInstr : {dst:I.ea, src:I.ea} -> I.instruction list,
       ea : I.C.cell -> I.ea} 
      ->
	{tmp : I.ea option,
	 dst : I.C.cell list,
	 src : I.C.cell list} 
	-> I.instruction list
  end = 
struct
  structure C = I.C

  datatype obj = TEMP | CELL of C.cell

  fun equal (r1, r2) = C.sameColor(r1,r2)

  fun equalObj (TEMP, TEMP) = true
    | equalObj (CELL u, CELL v) = equal(u, v)
    | equalObj _ = false

  fun shuffle{mvInstr, ea} {tmp, dst, src} = let
    fun mv{dst, src, instrs} = List.revAppend(mvInstr{dst=dst,src=src}, instrs)

    fun valOf(SOME x) = x
      | valOf NONE = raise Option

    fun opnd dst = case dst of 
                     TEMP     => valOf tmp 
                   | CELL dst => ea dst

    (* perform unconstrained moves *)
    fun loop((p as (rd,rs))::rest, changed, used, done, instrs) = 
	if List.exists (fn r => equalObj(r, rd)) used then
	   loop(rest, changed, used, p::done, instrs)
	else loop(rest, true, used, done,
                  mv{dst=opnd rd, src=opnd rs, instrs=instrs})
      | loop([], changed, _, done, instrs) = (changed, done, instrs)

    fun cycle([], instrs) = instrs
      | cycle(moves, instrs) =
	(case loop(moves, false, map #2 moves, [], instrs)
	  of (_, [], instrs) => instrs
	   | (true, acc, instrs) => cycle(acc, instrs)
	   | (false, (rd,rs)::acc, instrs) => let
	       fun rename(p as (a,b)) =
                   if equalObj(rd, b) then (a, TEMP) else p
	       val acc' = (rd, rs) :: map rename acc
	       val instrs' = mv{dst=valOf tmp,src=opnd rd,instrs=instrs}
	       val (_, acc'', instrs'') = 
		 loop(acc', false, map #2 acc', [], instrs')
	     in cycle(acc'', instrs'')
	     end
	 (*esac*))

    (* remove moves that have been coalesced. *)
    fun rmvCoalesced(rd::rds, rs::rss, mvs) = 
        if equal(rd, rs) then rmvCoalesced(rds, rss, mvs)
        else rmvCoalesced(rds, rss, (CELL rd, CELL rs)::mvs)
      | rmvCoalesced([], [], mvs) = mvs
  in rev (cycle (rmvCoalesced(dst, src, []), []))
  end
end

