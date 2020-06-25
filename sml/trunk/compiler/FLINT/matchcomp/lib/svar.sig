(* svar.sig *)

signature SVAR =
sig

    (* "simple" variables, with fixed type and lvar as _access_ *)
    
    datatype var
      = SVAR of	                (* "simple" variables *)
	  {name : Symbol.symbol,
	   typ : Types.ty,
	   lvar : LambdaVar.lvar}

    val mkSvar : Symbol.symbol * Types.ty * LambdaVar.lvar ->  var
    val newSvar : Symbol.symbol * Types.ty -> svar

    val svarName : var -> Symbol.symbol
    val svarType : var -> Types.ty
    val svarLvar : var -> LambdaVar.lvar

end (* signature SVAR *)
