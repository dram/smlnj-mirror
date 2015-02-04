(* bareabsyn.sig *)

signature BAREABSYN = sig

structure Basics : BASICS

datatype numberedLabel = LABEL of {name : Basics.Symbol.symbol, number: int};

datatype transElem
  = VALtrans of int    		(* old position, val or exn component *)
  | INLtrans of int		(* inline function index *)
  | STRtrans of int * thinning  (* old substr position, substr thinning *)
  | CONtrans of Basics.datacon	(* constructor as value component *)

and thinning
  = NOTHIN
  | THIN of transElem list

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
		| RAISEXexp of exp
		| LETexp of dec * exp
		| CASEexp of exp * rule list
		| FNexp of rule list

and rule	= RULE of pat * exp

and handler	= HANDLERX of exp

and hrule	= WITHhrule of Basics.datacon * rule list
		| WILDhrule of exp

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
		     locations: Basics.Access.path list}
				(* component paths *)
		| APPstr of
		    {oper: Basics.functorVar,
		     args: (strexp * thinning) list}

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
		     resultty: Basics.ty Basics.Option,
		     tyvars: Basics.tyvar list}
and fb		= FB of
		    {var:Basics.var,
		     clauses: clause list,
		     tyvars: Basics.tyvar list}
and clause	= CLAUSE of
		    {pats: pat list,
		     resultty: Basics.ty Basics.Option,
		     exp:exp}
and eb	 	= EB of
		    {exn:Basics.datacon,
		     ty: Basics.ty Basics.Option,
		     def: Basics.datacon Basics.Option}
and strb	= STRB of 
		    {strvar: Basics.structureVar,
		     def: strexp,
		     constraint: Basics.Signature Basics.Option,  (* information only *)
		     thin: thinning}
and shareSpec   = STRshare of Basics.spath list
		| TYCshare of Basics.spath list
and fctb	= FCTB of
		    {fctvar: Basics.functorVar,
		     params: Basics.structureVar list,
		     share: shareSpec list Basics.Option,
		     def: strexp,
		     constraint: Basics.Signature Basics.Option}

end (* signature BAREABSYN *)
