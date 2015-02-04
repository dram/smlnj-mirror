(* envaccess.sig *)

signature ENVACCESS = sig

    structure Access : ACCESS
    structure Basics : BASICS
    structure Env : ENV

    val openStructureVar : Basics.structureVar -> unit
    val setPervasives : Basics.structureVar list -> unit
    val openPervasives : unit -> unit

    val lookTYCinTable : Env.symtable * Basics.Symbol.symbol -> Basics.tycon ref
    val lookTYCinStr : Basics.Structure * Basics.Symbol.symbol -> Basics.tycon ref
    val lookTYC  : Basics.Symbol.symbol -> Basics.tycon ref
    val bindTYC  : Basics.Symbol.symbol * Basics.tycon -> Basics.tycon ref
    val protectDb : (unit -> unit) * (unit -> unit)
    val lookPatchTYC : Basics.Symbol.symbol -> Basics.tycon ref
    val patchTycons  : Basics.tycon ref list -> unit

    val lookCONinTable : Env.symtable * Basics.Symbol.symbol -> Basics.datacon
    val lookCON  : Basics.Symbol.symbol -> Basics.datacon
    val lookCONinStr : Basics.Structure * Basics.Symbol.symbol * Access.path
		         -> Basics.datacon
    val bindCON  : Basics.Symbol.symbol * Basics.datacon -> Basics.datacon

    val lookVARCONinTable : Env.symtable * Basics.Symbol.symbol -> Basics.binding
    val lookVARinTable : Env.symtable * Basics.Symbol.symbol -> Basics.var
    val unboundVAR : Basics.Symbol.symbol -> Basics.binding
    val lookVARinBase : Basics.Symbol.symbol -> Basics.var
    val lookVARCONinStr : Basics.Structure * Basics.Symbol.symbol * Access.path
			    -> Basics.binding
    val lookVARCON  : Basics.Symbol.symbol -> Basics.binding
    val getPatchVar : Basics.Symbol.symbol -> Basics.var ref
    val protectPatchList : (unit -> Basics.var ref list) *
			   (Basics.var ref list -> unit)
    val checkBinding : Basics.binder list -> unit 
    val newVAR  : Basics.binder list ref * Basics.Symbol.symbol ->
		  Basics.var
    val bindVAR : Basics.Symbol.symbol * Basics.var -> unit
    val bindVARs  : Basics.binder list -> unit
    
    datatype BoundTyvars
      = CLOSEDbtv of Basics.tyvar list 
      | OPENbtv of Basics.tyvar list ref
    val protectTyvars : BoundTyvars -> (unit->BoundTyvars) * (BoundTyvars -> unit)
    val currentTyvars : unit -> Basics.tyvar list
    val bindTYV  : Basics.Symbol.symbol * Basics.tyvar -> Basics.tyvar
    val lookTyvar  : Basics.Symbol.symbol -> Basics.tyvar

    val looksLikeExn : Basics.Symbol.symbol -> bool
    val fixExnName : Basics.Symbol.symbol -> Basics.Symbol.symbol
    val lookEXNinBase : Basics.Symbol.symbol -> Basics.datacon
    val unboundEXN : Basics.Symbol.symbol -> Basics.datacon
    val lookEXN  : Basics.Symbol.symbol -> Basics.datacon
    val lookEXNinStr : Basics.Structure * Basics.Symbol.symbol * Access.path ->
			  Basics.datacon
    val lookFixedEXNinStr : Basics.Structure * Basics.Symbol.symbol * Access.path ->
			      Basics.datacon
    val bindEXN  : Basics.Symbol.symbol * Basics.datacon -> Basics.datacon

    val lookSIG  : Basics.Symbol.symbol -> Basics.signatureVar
    val bindSIG  : Basics.Symbol.symbol * Basics.signatureVar ->
		   Basics.signatureVar

    val lookSTRinTable : Env.symtable * Basics.Symbol.symbol -> Basics.structureVar
    val lookSTR_sig : Basics.Symbol.symbol -> Basics.structureVar * Env.info
    val lookSTR  : Basics.Symbol.symbol -> Basics.structureVar
    val lookSTRinStr : Basics.Structure * Basics.Symbol.symbol * Access.path ->
			 Basics.structureVar
    val bindSTR : Basics.Symbol.symbol * Basics.structureVar -> unit

    val lookFCT : Basics.Symbol.symbol -> Basics.functorVar
    val bindFCT : Basics.Symbol.symbol * Basics.functorVar ->
		  Basics.functorVar

    val lookFIX : Basics.Symbol.symbol -> Basics.fixity
    val bindFIX : Basics.Symbol.symbol * Basics.fixity -> unit

    val buildStrTable : Env.marker -> Basics.trans list * Env.symtable
    val buildSigTable : Env.marker -> Basics.binding list * Env.symtable
    val buildFctTable : Env.marker -> Basics.trans list * Env.symtable *
				      Basics.strenv

    val reset : unit -> unit

    val getVARvars : Basics.Symbol.symbol -> Access.lvar list
    val getSTRvars : Basics.Symbol.symbol -> Access.lvar list
    val getFCTvars : Basics.Symbol.symbol -> Access.lvar list

end  (* signature ENVACCESS *)
