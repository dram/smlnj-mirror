(* cfg-util.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure CFGUtil : sig

  (* does a cluster contain any raw C calls? *)
    val hasRCC : CFG.cluster -> bool

    val tyToString : CFG_Type.ty -> string

  end = struct

    structure C = CFG

    fun hasRCC ((_, _, _, stm), frags) = let
	  fun stmHasRCC stm = (case stm
		 of C.LET(_, _, k) => stmHasRCC k
		  | C.ALLOC(_, _, _, k) => stmHasRCC k
		  | C.APP _ => false
		  | C.GOTO _ => false
		  | C.SWITCH(_, stms) => List.exists stmHasRCC stms
		  | C.BRANCH(_, _, _, k1, k2) =>  stmHasRCC k1 orelse  stmHasRCC k2
		  | C.ARITH(_, _, _, k) => stmHasRCC k
		  | C.SETTER(_, _, k) => stmHasRCC k
		  | C.RCC _ => true
		(* end case *))
	  fun fragHasRCC (_, _, _, stm) = stmHasRCC stm
	  in
	    stmHasRCC stm orelse List.exists fragHasRCC frags
	  end

    fun tyToString (CFG_Type.NUMt sz) = "i" ^ Int.toString sz
      | tyToString (CFG_Type.FLTt sz) = "f" ^ Int.toString sz
      | tyToString CFG_Type.PTRt = "ptr"
      | tyToString CFG_Type.FUNt = "fun"
      | tyToString CFG_Type.CNTt = "cont"

  end
