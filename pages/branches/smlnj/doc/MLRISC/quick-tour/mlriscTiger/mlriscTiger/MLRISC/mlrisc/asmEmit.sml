functor AsmEmit
  (structure F : FLOWGRAPH
   structure E : EMITTER_NEW
      sharing F = E.F) = 
struct
  structure PseudoOp = F.P
  fun asmEmit(F.CLUSTER{blocks, regmap, ...}) = let
    fun emit(F.PSEUDO pOp) = E.pseudoOp pOp
      | emit(F.LABEL lab) = E.defineLabel lab
      | emit(F.BBLOCK{insns, ...}) =
         app (fn insn => E.emitInstr(insn, regmap)) (rev (!insns))
      | emit(F.ORDERED blks) = app emit blks
  in app emit blocks
  end
end

(*
 * $Log$
 * Revision 1.1  2001/10/11 09:52:26  macqueen
 * Initial revision
 *
 * Revision 1.1.1.1  1998/04/08 18:39:01  george
 * Version 110.5
 *
 *)
