(* Copyright 1989 by AT&T Bell Laboratories *)
(* envaccess.sig *)

signature ENVACCESS = sig
    val transBinding : {path:Access.path, strenv:Basics.strenv} * Symbol.symbol list
				 -> Basics.binding -> Basics.binding
    val lookTYCinStr : Basics.Structure * Symbol.symbol * 'a * 'b *
				(string->unit) -> Basics.tycon
    val lookVARCONinStr: Basics.Structure * Symbol.symbol * Access.path
			  * Symbol.symbol list * (string->unit) -> Basics.binding
    val bogusSTR: Basics.structureVar
    val bogusSIGbody : Basics.Structure

    val openStructureVar : Basics.env -> Basics.structureVar -> Basics.env
    val lookTYC : Basics.env -> Symbol.symbol -> Basics.tycon
    val lookPathArTYC : Basics.env -> Symbol.symbol list * int * (string->unit)
			   -> Basics.tycon
    val lookPathCON : Basics.env -> Symbol.symbol list * (string->unit) 
				-> Basics.datacon
    val lookPathVARCON : Basics.env -> Symbol.symbol list * (string->unit) 
					-> Basics.binding
    val lookVARCON  : Basics.env -> Symbol.symbol -> Basics.binding
    val lookSIG  : Basics.env -> Symbol.symbol -> Basics.signatureVar
    val lookSTR : Basics.env -> Symbol.symbol -> Basics.structureVar
    val lookPathSTR : Basics.env -> Symbol.symbol list * (string->unit) -> Basics.structureVar
    val lookFCT : Basics.env -> Symbol.symbol -> Basics.functorVar
    val lookFIX : Basics.env -> Symbol.symbol -> Basics.fixity
    val staleLvars : Basics.env * Basics.env -> int list
end  (* signature ENVACCESS *)


