(* Copyright 1989 by AT&T Bell Laboratories *)
(* envaccess.sig *)

signature ENVACCESS = sig

    structure Access : ACCESS
    structure Basics : BASICS
    structure Env : ENV

    val openStructureVar : Basics.structureVar -> unit

    val lookTYCinTable : Basics.symtable * Basics.Symbol.symbol -> Basics.tycon
    val lookTYCinStr : Basics.Structure * Basics.Symbol.symbol * 'a * 'b *
				(string->unit) -> Basics.tycon
    val lookTYC : Basics.Symbol.symbol -> Basics.tycon
    val lookTYClocal : Basics.Symbol.symbol -> Basics.tycon
    val bindTYC : Basics.Symbol.symbol * Basics.tycon -> Basics.binding
    val lookPathArTYCinSig : int -> 
		(Basics.Symbol.symbol list * int * (string->unit))
			   -> Basics.tycon
    val lookPathArTYC : Basics.Symbol.symbol list * int * (string->unit)
			   -> Basics.tycon

    val dconApplied: Basics.datacon * Env.info -> Basics.datacon
    val lookCONinTable : Basics.symtable * Basics.Symbol.symbol -> Basics.datacon
    val lookCON  : Basics.Symbol.symbol -> Basics.datacon
    val lookCONlocal  : Basics.Symbol.symbol -> Basics.datacon
    val lookPathCON : Basics.Symbol.symbol list * (string->unit) 
				-> Basics.datacon
    val bindCON : Basics.Symbol.symbol * Basics.datacon -> Basics.binding

    val varApplied: Basics.var * Env.info * Basics.Symbol.symbol list -> Basics.var
    val lookVARinTable : Basics.symtable * Basics.Symbol.symbol -> Basics.var
    val lookVARCONinTable : Basics.symtable * Basics.Symbol.symbol -> Basics.binding
    val lookVARCONinStr: Basics.Structure * Basics.Symbol.symbol * Access.path
			  * Basics.Symbol.symbol list * (string->unit) -> Basics.binding
    val lookPathVARCON : Basics.Symbol.symbol list * (string->unit) 
					-> Basics.binding
    val lookVARCON  : Basics.Symbol.symbol -> Basics.binding
    val lookVARCONlocal : Basics.Symbol.symbol -> Basics.binding

    val bindVAR : Basics.Symbol.symbol * Basics.var -> Basics.binding
    
    val lookSIG  : Basics.Symbol.symbol -> Basics.signatureVar
    val bindSIG  : Basics.Symbol.symbol * Basics.signatureVar -> unit

    val strApplied: Basics.structureVar * Env.info * Basics.Symbol.symbol list
		    -> Basics.structureVar
    val lookSTRinTable : Basics.symtable * Basics.Symbol.symbol -> Basics.structureVar
    val lookSTR0 : Basics.Symbol.symbol -> Basics.structureVar * Env.info
    val lookSTR : Basics.Symbol.symbol -> Basics.structureVar
    val lookSTRlocal : Basics.Symbol.symbol -> Basics.structureVar
    val lookPathSTR : Basics.Symbol.symbol list * (string->unit) -> Basics.structureVar
    val lookPathSTRinSig : Basics.Symbol.symbol list * (string->unit)
			   -> Basics.Structure * int list
    val bindSTR : Basics.Symbol.symbol * Basics.structureVar -> Basics.binding
    val bogusSTR: Basics.structureVar
    val bogusSIGbody : Basics.Structure

    val lookFCT : Basics.Symbol.symbol -> Basics.functorVar

    val bindFCT : Basics.Symbol.symbol * Basics.functorVar -> unit

    val lookFIX : Basics.Symbol.symbol -> Basics.fixity
    val bindFIX : Basics.Symbol.symbol * Basics.fixityVar -> Basics.binding

    val staleLvars : Env.env * Env.env -> int list

end  (* signature ENVACCESS *)
