(* bareabsyn.sml *)

(* Abstract syntax of bare ML *)

structure BareAbsyn : BAREABSYN = struct

structure Basics = Basics

open Basics

datatype numberedLabel = LABEL of {name : Symbol.symbol, number: int};

datatype transElem
  = VALtrans of int    		(* old position, val or exn component *)
  | INLtrans of int		(* inline function index *)
  | STRtrans of int * thinning  (* old substr position, substr thinning *)
  | CONtrans of datacon		(* constructor as value component *)

and thinning
  = NOTHIN
  | THIN of transElem list

datatype exp	= VARexp of var ref
		| CONexp of datacon
		| INTexp of int
		| REALexp of string
		| STRINGexp of string
		| RECORDexp of (numberedLabel * exp) list
		| SEQexp of exp list		
		| APPexp of exp * exp		
		| CONSTRAINTexp of exp * ty
		| HANDLEexp of exp * handler
		| RAISEXexp of exp
		| LETexp of dec * exp
		| CASEexp of exp * rule list
		| FNexp of rule list

and rule	= RULE of pat * exp

and handler	= HANDLERX of exp

and hrule	= WITHhrule of datacon * rule list
		| WILDhrule of exp

and pat		= WILDpat
		| VARpat of var
		| INTpat of int
		| REALpat of string
		| STRINGpat of string
		| CONpat of datacon
		| RECORDpat of
		    {fields : (label * pat) list,
		     flex : bool,
		     typ : ty ref,
		     pats : pat list ref}
		| APPpat of datacon * pat
		| CONSTRAINTpat of pat * ty
		| LAYEREDpat of pat * pat

and strexp	= VARstr of structureVar
		| STRUCTstr of
		    {body: dec list,
		     locations: Access.path list}  (* component paths *)
		| APPstr of
		    {oper: functorVar,
		     args: (strexp * thinning) list}

and dec		= VALdec of vb list
		| VALRECdec of rvb list
		| TYPEdec of tycon list
		| DATATYPEdec of tycon list 
		| EXCEPTIONdec of eb list
		| STRdec of strb
		| FCTdec of fctb
		| LOCALdec of dec * dec
		| SEQdec of dec list
		| OPENdec of structureVar list

and vb		= VB of
		    {pat:pat,
		     exp:exp,
		     tyvars: tyvar list}

and rvb		= RVB of
		    {var:var,
		     exp:exp,
		     resultty: ty Option,
		     tyvars: tyvar list}

and fb		= FB of
		    {var:var,
		     clauses: clause list,
		     tyvars: tyvar list}

and clause	= CLAUSE of
		    {pats: pat list,
		     resultty: ty Option,
		     exp:exp}

and eb	 	= EB of
		    {exn:datacon,
		     ty: ty Option,
		     def:datacon Option}

and strb	= STRB of 
		    {strvar: structureVar,
		     def: strexp,
		     constraint: Signature Option,  (* information only *)
		     thin: thinning}

and shareSpec   = STRshare of spath list
		| TYCshare of spath list

and fctb	= FCTB of
		    {fctvar: functorVar,
		     params: structureVar list,
		     share: shareSpec list Option,
		     def: strexp,
		     constraint: Signature Option}

end (* structure BareAbsyn *)
