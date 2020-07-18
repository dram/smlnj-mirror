(* svar.sig *)

signature SVAR =
sig

    (* "simple" variables, with fixed type and lvar as _access_ *)

    type svar
(*      = SVAR of	                (* "simple" variables *)
	  {name : Symbol.symbol,
	   typ : Types.ty,
	   lvar : LambdaVar.lvar}
*)
    val mkSvar : Symbol.symbol * Types.ty * LambdaVar.lvar ->  svar
    val newSvar : string * Types.ty -> svar

    val svarName : svar -> Symbol.symbol
    val svarType : svar -> Types.ty
    val svarLvar : svar -> LambdaVar.lvar

    val svarToVar : svar -> Var.var  (* svar -> VarCon.var *)

end (* signature SVAR *)
