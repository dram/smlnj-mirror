(* var.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure Var : VAR =
struct

    structure T  = Types
    structure S  = Symbol

    datatype var
      = VALvar of			(* ordinary variables *)
	  {name : S.symbol,
	   typ : T.ty ref,
	   btvs : T.tyvar list ref,
	   access : LambdaVar.lvar}

    fun mkVALvar (id, lvar) =
	  VALvar{name = id,
		 typ = ref T.UNDEFty,
		 access = lvar,
		 btvs = ref []}

    fun varName (VALvar{name,...}) = name

end (* structure VarCon *)
