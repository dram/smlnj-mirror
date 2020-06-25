(* svar.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)
(* "simple variables *)

structure SVar : SVAR =
struct

local
    structure T  = Types
    structure S  = Symbol
in

    datatype svar
      = SVAR of			(* ordinary variables *)
	  {name : S.symbol,
	   typ : T.ty ref,
	   lvar : LambdaVar.lvar}

    fun mkSvar (id, ,ty, lvar) =
	  SVAR{name = id,
	       typ = ty,
	       lvar = lvar}

    fun newSvar (id, ty) =  (* internally generates fresh lvar *)
	SVAR{name = id,
	     typ = ty,
	     lvar = LambdaVar.mkLvar()}

    fun svarName (SVAR{name,...}) = name
    fun svarType (SVAR{typ,...}) = typ
    fun svarLvar (SVAR{lvar,...}) = lvar				       

end (* local *)
end (* structure VarCon *)
