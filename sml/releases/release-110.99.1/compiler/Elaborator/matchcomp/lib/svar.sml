(* svar.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)
(* "simple variables *)

structure SVar :> SVAR =
struct

local
    structure S  = Symbol
    structure T  = Types
    structure LV = LambdaVar
in

    datatype svar
      = SVAR of			(* ordinary variables *)
	  {name : S.symbol,
	   typ : T.ty,
	   lvar : LV.lvar}

    (* mkSvar : Symbol.symbol * T.ty * LV.lvar -> svar *)
    fun mkSvar (id, ty, lvar) =
	  SVAR{name = id,
	       typ = ty,
	       lvar = lvar}

    (* newSvar : string * T.ty -> svar *)
    fun newSvar (id, ty) =  (* internally generates fresh lvar *)
	SVAR{name = S.make id,
	     typ = ty,
	     lvar = LV.mkLvar()}

    fun svarName (SVAR{name,...}) = name
    fun svarType (SVAR{typ,...}) = typ
    fun svarLvar (SVAR{lvar,...}) = lvar

    fun svarToVar (SVAR{name,typ,lvar}) =
	Var.VALvar{name = name,
		   typ = ref typ,
		   btvs = ref nil,
		   access = (* Access.LVAR *) lvar}

(*
    fun svarToVar (SVAR{name,typ,lvar}) =
	VarCon.VALvar{path = SymPath.SPATH [name],
		      typ = ref typ,
		      btvs = ref nil,
		      access = Access.LVAR(lvar),
		      prim = PrimopId.NonPrim}
*)
end (* local *)
end (* structure VarCon *)
