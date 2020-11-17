(* COPYRIGHT 1996 Bell Laboratories *)
(* lookup.sig *)

signature LOOKUP =
sig
  val lookFix : StaticEnv.staticEnv * Symbol.symbol -> Fixity.fixity

  val lookSig : StaticEnv.staticEnv * Symbol.symbol * ErrorMsg.complainer
                -> Modules.Signature

  val lookFsig : StaticEnv.staticEnv * Symbol.symbol * ErrorMsg.complainer
		 -> Modules.fctSig

  val lookStr : StaticEnv.staticEnv * SymPath.path * ErrorMsg.complainer
                -> Modules.Structure

  val lookStrDef : StaticEnv.staticEnv * SymPath.path * ErrorMsg.complainer
                   -> Modules.strDef

  val lookFct : StaticEnv.staticEnv * SymPath.path * ErrorMsg.complainer
                -> Modules.Functor

  val lookTyc : StaticEnv.staticEnv * SymPath.path * ErrorMsg.complainer
                -> Types.tycon

  val lookArTyc : StaticEnv.staticEnv * SymPath.path * int
                    * ErrorMsg.complainer -> Types.tycon

  (* lookValSym and lookSym return value or constructor bindings (Absyn.value) *)

  val lookValSym : StaticEnv.staticEnv * Symbol.symbol * ErrorMsg.complainer
		   -> Absyn.value

  val lookVal : StaticEnv.staticEnv * SymPath.path * ErrorMsg.complainer
                -> Absyn.value

  val lookExn : StaticEnv.staticEnv * SymPath.path * ErrorMsg.complainer
                -> Types.datacon

end (* signature LOOKUP *)
