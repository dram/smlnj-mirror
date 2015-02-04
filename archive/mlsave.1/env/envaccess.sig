(* envaccess.sig *)

signature ENVACCESS = sig

    structure Access : ACCESS
    structure Basics : BASICS
    structure Env : ENV

    val lookTYCinTable : Env.symtable * Basics.Symbol.symbol -> Basics.tycon ref
    val evalSYMtyc : Basics.spath * Basics.Symbol.symbol * Basics.Structure
		       -> Basics.tycon ref
    val lookTYCinStr : Basics.Structure * Basics.Symbol.symbol -> Basics.tycon ref
    val tyconInCtx : Basics.tycon ref * Basics.context -> Basics.tycon ref
    val lookTYC  : Basics.Symbol.symbol -> Basics.tycon ref
    val bindTYC  : Basics.Symbol.symbol * Basics.tycon -> Basics.tycon ref
    val enterDb  : unit -> unit
    val exitDb  : unit -> unit
    val lookPatchTYC : Basics.Symbol.symbol -> Basics.tycon ref
    val patchTycons  : Basics.tycon ref list -> unit

    val lookCON  : Basics.Symbol.symbol -> Basics.datacon
(*    val lookCONinTable  : Env.symtable * Basics.Symbol.symbol -> Basics.datacon *)
    val lookCONinStr : Basics.Structure * Basics.Symbol.symbol * Access.path
		         -> Basics.datacon
    val bindCON  : Basics.Symbol.symbol * Basics.datacon -> Basics.datacon

(*    val lookVARCONinTable : Env.symtable * Basics.Symbol.symbol -> Basics.binding
*)
    val unboundVAR : Basics.Symbol.symbol -> Basics.binding
    val lookVARCONinStr : Basics.Structure * Basics.Symbol.symbol * Access.path
			    -> Basics.binding
    val lookVARCON  : Basics.Symbol.symbol -> Basics.binding

    val lookVARinBase : Basics.Symbol.symbol -> Basics.var 

    val looksLikeExn : Basics.Symbol.symbol -> bool
    val newVAR  : Basics.binder list ref * Basics.Symbol.symbol ->
		  Basics.var
    val bindVAR : Basics.Symbol.symbol * Basics.var -> unit
    val bindVARs  : Basics.binder list -> unit
    type patchList
    val getPatchVar : Basics.Symbol.symbol -> Basics.var ref
    val protectPatchList : (unit -> patchList) * (patchList -> unit)
    
    datatype BoundTyvars
      = CLOSEDbtv of Basics.tyvar list 
      | OPENbtv of Basics.tyvar list ref
    val protectTyvars : BoundTyvars -> (unit->BoundTyvars) * (BoundTyvars -> unit)
    val currentTyvars : unit -> Basics.tyvar list
    val bindTYV  : Basics.Symbol.symbol * Basics.tyvar -> Basics.tyvar
    val lookTyvar  : Basics.Symbol.symbol -> Basics.tyvar

    val unboundEXN : Basics.Symbol.symbol -> Basics.datacon
    val lookEXN  : Basics.Symbol.symbol -> Basics.datacon
    val lookEXNinStr : Basics.Structure * Basics.Symbol.symbol * Access.path ->
			  Basics.datacon
    val bindEXN  : Basics.Symbol.symbol * Basics.datacon -> Basics.datacon

    val lookSIG  : Basics.Symbol.symbol -> Basics.signatureVar
    val bindSIG  : Basics.Symbol.symbol * Basics.signatureVar ->
		   Basics.signatureVar

    val lookSTR  : Basics.Symbol.symbol -> Basics.structureVar
    val lookSTRinStr : Basics.Structure * Basics.Symbol.symbol * Access.path ->
			 Basics.structureVar
    val bindSTR : Basics.Symbol.symbol * Basics.structureVar ->
		  Basics.structureVar

    val lookFCT : Basics.Symbol.symbol -> Basics.functorVar
    val bindFCT : Basics.Symbol.symbol * Basics.functorVar ->
		  Basics.functorVar

    val lookFIX : Basics.Symbol.symbol -> Basics.fixity
    val bindFIX : Basics.Symbol.symbol * Basics.fixity -> unit

    val buildStrTable : Env.marker -> Access.path list * Env.symtable
    val buildSigTable : Env.marker -> Basics.binding list * Env.symtable

    val reset : unit -> unit

    val equalref : int ref
    val notequalref : int ref
    val assignref : int ref
    val updateref : int ref

end  (* signature ENVACCESS *)
