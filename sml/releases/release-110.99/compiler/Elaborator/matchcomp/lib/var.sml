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
	   btvs : T.metavar list ref,
	   access : LambdaVar.lvar}

    (* mkVALvar : Symbol.symbol * LambdaVar.lvar -> var *)
    fun mkVALvar (id, lvar) =
	  VALvar{name = id,
		 typ = ref T.UNDEFty,
		 access = lvar,
		 btvs = ref []}

    (* newVALvar : string * T.ty -> var *)
    fun newVALvar (id, ty) = 
	  VALvar{name = id,
		 typ = ref ty,
		 access = LambdaVar.mkLvar(),
		 btvs = ref []}

		
    fun varName (VALvar{name,...}) = name

    fun varAccess (VALvar{access,...}) = access

end (* structure VarCon *)
