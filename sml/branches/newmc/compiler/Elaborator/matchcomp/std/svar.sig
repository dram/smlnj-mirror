(* svar.sig *)

signature SVAR =
sig

    (* "simple" variables, with fixed type and lvar as _access_.
     * used as a special interface to VarCon.var in the match compiler. *)

    type svar = VarCon.var

    val mkSvar : Symbol.symbol * Types.ty * LambdaVar.lvar ->  svar
    val newSvar : string * Types.ty -> svar

    val svarName : svar -> Symbol.symbol
    val svarType : svar -> Types.ty
    val svarLvar : svar -> LambdaVar.lvar

    val svarToVar : svar -> VarCon.var

end (* signature SVAR *)
