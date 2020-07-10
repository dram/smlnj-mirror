(* svar.sig *)

signature SVAR =
sig

    (* "simple" variables, with fixed type and lvar as _access_ *)
    
    datatype svar
      = SVAR of	                (* "simple" variables *)
	  {name : Symbol.symbol,
	   typ : Types.ty,
	   lvar : LambdaVar.lvar}

    val mkSvar : Symbol.symbol * Types.ty * LambdaVar.lvar ->  svar
    val newSvar : string * Types.ty -> svar

    val svarName : svar -> Symbol.symbol
    val svarType : svar -> Types.ty
    val svarLvar : svar -> LambdaVar.lvar

end (* signature SVAR *)
