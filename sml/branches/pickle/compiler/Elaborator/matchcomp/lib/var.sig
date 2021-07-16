(* var.sig
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature VAR =
sig

    datatype var
      = VALvar of	                (* ordinary variables *)
	  {name : Symbol.symbol,
	   typ : Types.ty ref,
	   btvs : Types.metavar list ref,
	   access : LambdaVar.lvar}

    val mkVALvar : Symbol.symbol * LambdaVar.lvar ->  var

    val newVALvar : string * Types.ty -> var

    val varName : var -> Symbol.symbol

    val varAccess : var -> LambdaVar.lvar

end (* signature VAR *)
