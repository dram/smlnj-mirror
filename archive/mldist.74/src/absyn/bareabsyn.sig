(* Copyright 1989 by AT&T Bell Laboratories *)
(* bareabsyn.sig *)

signature BAREABSYN = sig

structure Variables : VARIABLES
structure Types : TYPES
structure Modules : MODULES
structure Access : ACCESS 
type linenum
structure Fixity : FIXITY

datatype numberedLabel = LABEL of {name : Symbol.symbol, number: int}

datatype exp	= VARexp of Variables.var ref
		| CONexp of Types.datacon
		| INTexp of int
		| REALexp of string
		| STRINGexp of string
		| RECORDexp of (numberedLabel * exp) list
		| SEQexp of exp list		
		| APPexp of exp * exp		
		| CONSTRAINTexp of exp * Types.ty
		| HANDLEexp of exp * handler
		| RAISEexp of exp
		| LETexp of dec * exp
		| CASEexp of exp * rule list
		| FNexp of rule list
	        | MARKexp of exp * linenum * linenum

and rule	= RULE of pat * exp

and handler	= HANDLER of exp

and pat		= WILDpat
		| VARpat of Variables.var
		| INTpat of int
		| REALpat of string
		| STRINGpat of string
		| CONpat of Types.datacon
		| RECORDpat of
		    {fields : (Types.label * pat) list,
		     flex : bool,
		     typ : Types.ty ref,
		     pats : pat list ref}
		| APPpat of Types.datacon * pat
		| CONSTRAINTpat of pat * Types.ty
		| LAYEREDpat of pat * pat

and strexp	= VARstr of Modules.structureVar
		| STRUCTstr of
		    {body: dec list,
		     str: Modules.Structure,
		     locations: Modules.trans list}  (* component paths *)
		| APPstr of
		    {oper: Modules.functorVar,
		     argexp: strexp,
		     argthin: Modules.thinning,
		     str: Modules.Structure}
		| LETstr of dec * strexp
		| MARKstr of strexp * linenum * linenum

and dec		= VALdec of vb list
		| VALRECdec of rvb list
		| TYPEdec of tb list
		| DATATYPEdec of
		    {datatycs: Types.tycon list,
		     withtycs: tb list}
		| ABSTYPEdec of
		    {abstycs: Types.tycon list,
		     withtycs: tb list,
		     body: dec}
		| EXCEPTIONdec of eb list
		| STRdec of strb list
		| ABSdec of strb list
		| FCTdec of fctb list
		| SIGdec of Modules.signatureVar list
		| OPENdec of Modules.structureVar list
		| OVLDdec of Variables.var
		| FIXdec of {fixity:Fixity.fixity,
			     ops: Symbol.symbol list}
		| LOCALdec of dec * dec
		| SEQdec of dec list
                | IMPORTdec of string list
	        | MARKdec of dec * linenum * linenum

and vb		= VB of
		    {pat:pat,
		     exp:exp,
		     tyvars: Types.tyvar list}
and rvb		= RVB of
		    {var:Variables.var,
		     exp:exp,
		     resultty: Types.ty option,
		     tyvars: Types.tyvar list}
and fb		= FB of
		    {var:Variables.var,
		     clauses: clause list,
		     tyvars: Types.tyvar list}
and clause	= CLAUSE of
		    {pats: pat list,
		     resultty: Types.ty option,
		     exp:exp}
and tb		= TB of
		    {tyc : Types.tycon,
		     def : Types.ty}
and eb	 	= EBgen of
		    {exn: Types.datacon,
		     etype: Types.ty option}
		| EBdef of
		    {exn: Types.datacon,
		     edef: Types.datacon}
and strb	= STRB of 
		    {strvar: Modules.structureVar,
		     def: strexp,
		     thin: Modules.thinning,
		     constraint: Modules.Signature option}  (* info only *)
and fctb	= FCTB of
		    {fctvar: Modules.functorVar,
		     param: Modules.structureVar,
		     def: strexp,
		     thin: Modules.thinning,
		     constraint: Modules.Signature option}  (* info only *)

end (* signature BAREABSYN *)
