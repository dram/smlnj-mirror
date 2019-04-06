(* envaccess.sig *)

signature ENVACCESS = sig

    structure Access : ACCESS
    structure Basics : BASICS
    structure Env : ENV

    val openStructureVar : Basics.structureVar -> unit

    val lookTYCinTable : Basics.symtable * Basics.Symbol.symbol -> Basics.tycon ref
    val lookTYCinStr : Basics.Structure * Basics.Symbol.symbol -> Basics.tycon ref
    val lookTYC : Basics.Symbol.symbol -> Basics.tycon ref
    val lookTYClocal : Basics.Symbol.symbol -> Basics.tycon ref
    val ibindTYC : int * string * Basics.tycon ref -> unit
    val bindTYC : Basics.Symbol.symbol * Basics.tycon ref -> unit
    val lookArTYCinSig : int -> Basics.Symbol.symbol * int -> Basics.tycon ref
    val lookArTYC : (Basics.Symbol.symbol * int -> Basics.tycon ref) ref
    val lookPathArTYCinSig : int -> (Basics.Symbol.symbol list * int) ->
			      Basics.tycon ref
    val lookPathArTYC : (Basics.Symbol.symbol list * int -> Basics.tycon ref) ref
    val protectDb : unit -> (unit -> unit) * (unit -> unit)

    val dconApplied: Basics.datacon * Env.info -> Basics.datacon
    val lookCONinTable : Basics.symtable * Basics.Symbol.symbol -> Basics.datacon
    val lookCON  : Basics.Symbol.symbol -> Basics.datacon
    val lookCONlocal  : Basics.Symbol.symbol -> Basics.datacon
    val lookCONinStr : Basics.Structure * Basics.Symbol.symbol * Access.path
		         -> Basics.datacon
    val ibindCON : int * string * Basics.datacon -> unit
    val bindCON : Basics.Symbol.symbol * Basics.datacon -> unit

    val unboundVAR : Basics.Symbol.symbol -> Basics.binding
    val varApplied: Basics.var * Env.info -> Basics.var
    val lookVARinTable : Basics.symtable * Basics.Symbol.symbol -> Basics.var
    val lookVARCONinTable : Basics.symtable * Basics.Symbol.symbol -> Basics.binding
    val lookVARCONinStr : Basics.Structure * Basics.Symbol.symbol * Access.path
			    -> Basics.binding
    val lookVARCON  : Basics.Symbol.symbol -> Basics.binding
    val lookVARCONlocal : Basics.Symbol.symbol -> Basics.binding

    val getPatchVar : Basics.Symbol.symbol -> Basics.var ref
    val protectPatchList : (unit -> Basics.var ref list) *
			   (Basics.var ref list -> unit)
    val checkBinding : (Basics.Symbol.symbol * Basics.var) -> unit 
    val newVAR : (Basics.Symbol.symbol * Basics.var) list ref * Basics.Symbol.symbol ->
		  Basics.var
    val ibindVAR : int * string * Basics.var -> unit
    val bindVAR : Basics.Symbol.symbol * Basics.var -> unit
    val bindVARs : (Basics.Symbol.symbol * Basics.var) list -> unit
    
    val protectTyvars : Basics.tyvar list option ->
		        (unit -> Basics.tyvar list) * (Basics.tyvar list -> unit)
    val currentTyvars : unit -> Basics.tyvar list
    val lookTyvar  : Basics.Symbol.symbol -> Basics.tyvar

    val looksLikeExn : Basics.Symbol.symbol -> bool
    val unboundEXN : Basics.Symbol.symbol -> Basics.datacon
    val lookEXNinStr : Basics.Structure * Basics.Symbol.symbol * Access.path ->
			  Basics.datacon

    val lookSIG  : Basics.Symbol.symbol -> Basics.signatureVar
    val bindSIG  : Basics.Symbol.symbol * Basics.signatureVar -> unit

    val strApplied: Basics.structureVar * Env.info -> Basics.structureVar
    val lookSTRinTable : Basics.symtable * Basics.Symbol.symbol -> Basics.structureVar
    val lookSTR0 : Basics.Symbol.symbol -> Basics.structureVar * Env.info
    val lookSTR : Basics.Symbol.symbol -> Basics.structureVar
    val lookSTRlocal : Basics.Symbol.symbol -> Basics.structureVar
    val lookSTRinStr : Basics.Structure * Basics.Symbol.symbol * Access.path ->
			 Basics.structureVar
    val ibindSTR : int * string * Basics.structureVar -> unit
    val bindSTR : Basics.Symbol.symbol * Basics.structureVar -> unit
    val bogusSTR: Basics.structureVar

    val lookFCT : Basics.Symbol.symbol -> Basics.functorVar

    val lookPathinStr: Basics.Structure * Access.path * Basics.Symbol.symbol list *
		  (Basics.Structure * Basics.Symbol.symbol * Access.path -> 'a) -> 'a
    val lookPath: Basics.Symbol.symbol list * 
		  (Basics.Structure * Basics.Symbol.symbol * Access.path -> 'a) -> 'a

    val bindFCT : Basics.Symbol.symbol * Basics.functorVar -> unit

    val lookFIX : Basics.Symbol.symbol -> Basics.fixity
    val ibindFIX : int * string * Basics.fixityVar -> unit
    val bindFIX : Basics.Symbol.symbol * Basics.fixityVar -> unit

    val staleLvars : Env.env * Env.env -> int list

    val binderGt : Basics.binder * Basics.binder -> bool
         (*(int * string * Basics.binding) * 
		     (int * string * Basics.binding) -> bool*)

    val buildStrTable : unit -> Basics.trans list * Basics.symtable
    val buildSigTable : unit -> Basics.binding list * Basics.symtable
    val buildFctTable : unit -> Basics.trans list * Basics.symtable *
				      Basics.strenv

    val abstractBody : Basics.Structure * int -> unit

    val exnBind : Basics.datacon ref
    val exnMatch : Basics.datacon ref
    val stringequalPath : int list ref
    val polyequalPath : int list ref
    val currentPath : int list ref
    val toplevelPath : int list ref
    val getDebugVar : Basics.var ref
    val setCore : Basics.structureVar -> unit

    val reset : unit -> unit

end  (* signature ENVACCESS *)
