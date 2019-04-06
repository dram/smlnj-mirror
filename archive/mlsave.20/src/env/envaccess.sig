(* envaccess.sig *)

signature ENVACCESS = sig

    structure Access : ACCESS
    structure Basics : BASICS
    structure Env : ENV

    val openStructureVar : Basics.structureVar -> unit
    val setPervasives : Basics.structureVar list -> unit
    val openPervasives : unit -> unit

    exception UnboundInStr of string

    val lookTYCinTable : Env.symtable * Basics.Symbol.symbol -> Basics.tycon ref
    val lookTYCinStr : Basics.Structure * Basics.Symbol.symbol -> Basics.tycon ref
    val lookTYC  : Basics.Symbol.symbol -> Basics.tycon ref
    val lookTYClocal : Basics.Symbol.symbol -> Basics.tycon ref
    val bindTYC  : Basics.Symbol.symbol * Basics.tycon ref -> unit
    val protectDb : (unit -> unit) * (unit -> unit)
    val lookPatchTYC : Basics.Symbol.symbol * int -> Basics.tycon ref
    val patchTycons  : Basics.tycon ref list -> unit

    val dconApplied: Basics.datacon * Env.info -> Basics.datacon
    val lookCONinTable : Env.symtable * Basics.Symbol.symbol -> Basics.datacon
    val lookCON  : Basics.Symbol.symbol -> Basics.datacon
    val lookCONlocal  : Basics.Symbol.symbol -> Basics.datacon
    val lookCONinStr : Basics.Structure * Basics.Symbol.symbol * Access.path
		         -> Basics.datacon
    val bindCON  : Basics.Symbol.symbol * Basics.datacon -> unit

    val varApplied: Basics.var * Env.info -> Basics.var
    val lookVARCONinTable : Env.symtable * Basics.Symbol.symbol -> Basics.binding
    val lookVARinTable : Env.symtable * Basics.Symbol.symbol -> Basics.var
    val unboundVAR : Basics.Symbol.symbol -> Basics.binding
    val lookVARinBase : Basics.Symbol.symbol -> Basics.var
    val lookVARCONinStr : Basics.Structure * Basics.Symbol.symbol * Access.path
			    -> Basics.binding
    val lookVARCON  : Basics.Symbol.symbol -> Basics.binding
    val lookVARCONlocal : Basics.Symbol.symbol -> Basics.binding

    val getPatchVar : Basics.Symbol.symbol -> Basics.var ref
    val protectPatchList : (unit -> Basics.var ref list) *
			   (Basics.var ref list -> unit)
    val checkBinding : Basics.binder list -> unit 
    val newVAR  : Basics.binder list ref * Basics.Symbol.symbol ->
		  Basics.var
    val bindVAR : Basics.Symbol.symbol * Basics.var -> unit
    val bindVARs  : Basics.binder list -> unit
    
    val protectTyvars : Basics.tyvar list option ->
		        (unit -> Basics.tyvar list) * (Basics.tyvar list -> unit)
    val currentTyvars : unit -> Basics.tyvar list
    val bindTYV  : Basics.Symbol.symbol * Basics.tyvar -> Basics.tyvar
    val lookTyvar  : Basics.Symbol.symbol -> Basics.tyvar

    val looksLikeExn : Basics.Symbol.symbol -> bool
    val fixExnName : Basics.Symbol.symbol -> Basics.Symbol.symbol
    val lookEXNinBase : Basics.Symbol.symbol -> Basics.datacon
    val unboundEXN : Basics.Symbol.symbol -> Basics.datacon
    val lookEXN : Basics.Symbol.symbol -> Basics.datacon
    val lookEXNlocal : Basics.Symbol.symbol -> Basics.datacon
    val lookEXNinStr : Basics.Structure * Basics.Symbol.symbol * Access.path ->
			  Basics.datacon
    val lookFixedEXNinStr : Basics.Structure * Basics.Symbol.symbol * Access.path ->
			      Basics.datacon
    val bindEXN  : Basics.Symbol.symbol * Basics.datacon -> Basics.datacon

    val lookSIG  : Basics.Symbol.symbol -> Basics.signatureVar
    val bindSIG  : Basics.Symbol.symbol * Basics.signatureVar ->
		   Basics.signatureVar

    val strApplied: Basics.structureVar * Env.info -> Basics.structureVar
    val lookSTRinTable : Env.symtable * Basics.Symbol.symbol -> Basics.structureVar
    val lookSTRinSig : Basics.Symbol.symbol -> Basics.structureVar * Env.info
    val lookSTR : Basics.Symbol.symbol -> Basics.structureVar
    val lookSTRlocal : Basics.Symbol.symbol -> Basics.structureVar
    val lookSTRinStr : Basics.Structure * Basics.Symbol.symbol * Access.path ->
			 Basics.structureVar
    val bindSTR : Basics.Symbol.symbol * Basics.structureVar -> unit

    val lookFCT : Basics.Symbol.symbol -> Basics.functorVar

    val lookPath: Basics.Symbol.symbol list * 
		  (Basics.Structure * Basics.Symbol.symbol * Access.path -> 'a) -> 'a

    val bindFCT : Basics.Symbol.symbol * Basics.functorVar ->
		  Basics.functorVar

    val lookFIX : Basics.Symbol.symbol -> Basics.fixity
    val bindFIX : Basics.Symbol.symbol * Basics.fixity -> unit

    val binderGt : Basics.binder * Basics.binder -> bool

    val buildStrTable : unit -> Basics.trans list * Env.symtable
    val buildSigTable : unit -> Basics.binding list * Env.symtable
    val buildFctTable : unit -> Basics.trans list * Env.symtable *
				      Basics.strenv

    val getVARvars : Basics.Symbol.symbol -> Access.lvar list
    val getSTRvars : Basics.Symbol.symbol -> Access.lvar list
    val getFCTvars : Basics.Symbol.symbol -> Access.lvar list

    val reset : unit -> unit

end  (* signature ENVACCESS *)
