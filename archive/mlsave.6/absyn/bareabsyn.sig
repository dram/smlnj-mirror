(* bareabsyn.sig *)

signature BAREABSYN = sig

structure Basics : BASICS
structure Access : ACCESS

datatype numberedLabel = LABEL of {name : Basics.Symbol.symbol, number: int};

datatype exp	= VARexp of Basics.var ref
		| CONexp of Basics.datacon
		| INTexp of int
		| REALexp of string
		| STRINGexp of string
		| RECORDexp of (numberedLabel * exp) list
		| SEQexp of exp list		
		| APPexp of exp * exp		
		| CONSTRAINTexp of exp * Basics.ty
		| HANDLEexp of exp * handler
		| RAISEexp of exp
		| LETexp of dec * exp
		| CASEexp of exp * rule list
		| FNexp of rule list

and rule	= RULE of pat * exp

and handler	= HANDLER of exp

and pat		= WILDpat
		| VARpat of Basics.var
		| INTpat of int
		| REALpat of string
		| STRINGpat of string
		| CONpat of Basics.datacon
		| RECORDpat of
		    {fields : (Basics.label * pat) list,
		     flex : bool,
		     typ : Basics.ty ref,
		     pats : pat list ref}
		| APPpat of Basics.datacon * pat
		| CONSTRAINTpat of pat * Basics.ty
		| LAYEREDpat of pat * pat

and strexp	= VARstr of Basics.structureVar
		| STRUCTstr of
		    {body: dec list,
		     locations: Basics.trans list}  (* component paths *)
		| APPstr of
		    {oper: Basics.functorVar,
		     argexp: strexp,
		     argthin: Basics.thinning}

and dec		= VALdec of vb list
		| VALRECdec of rvb list
		| TYPEdec of Basics.tycon list
		| DATATYPEdec of Basics.tycon list 
		| EXCEPTIONdec of eb list
		| STRdec of strb
		| FCTdec of fctb
		| OPENdec of Basics.structureVar list
		| LOCALdec of dec * dec
		| SEQdec of dec list

and vb		= VB of
		    {pat:pat,
		     exp:exp,
		     tyvars: Basics.tyvar list}
and rvb		= RVB of
		    {var:Basics.var,
		     exp:exp,
		     resultty: Basics.ty option,
		     tyvars: Basics.tyvar list}
and fb		= FB of
		    {var:Basics.var,
		     clauses: clause list,
		     tyvars: Basics.tyvar list}
and clause	= CLAUSE of
		    {pats: pat list,
		     resultty: Basics.ty option,
		     exp:exp}
and eb	 	= EB of
		    {exn:Basics.datacon,
		     ty: Basics.ty option,
		     def: Basics.datacon option}
and strb	= STRB of 
		    {strvar: Basics.structureVar,
		     def: strexp,
		     thin: Basics.thinning,
		     constraint: Basics.Structure option}  (* info only *)
and shareSpec   = STRshare of Basics.spath list
		| TYCshare of Basics.spath list
and fctb	= FCTB of
		    {fctvar: Basics.functorVar,
		     param: Basics.structureVar,
		     def: strexp,
		     thin: Basics.thinning,
		     constraint: Basics.Structure option}  (* info only *)

end (* signature BAREABSYN *)
