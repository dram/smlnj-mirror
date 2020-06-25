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
	   btvs : Types.tyvar list ref,
	   access : LambdaVar.lvar}

    val mkVALvar : Symbol.symbol * LambdaVar.lvar ->  var

    val varName : var -> Symbol.symbol

end (* signature VAR *)
