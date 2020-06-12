(* absyn.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure Absyn  (* : ABSYN  *) =
struct

    structure S = Symbol
    structure T = Types

    datatype numberedLabel = LABEL of {name: S.symbol, number: int}

    datatype exp
      = VARexp of VarCon.var ref * T.tyvar list
      | CONexp of T.datacon * T.tyvar list
      | NUMexp of string * num_lit
      | STRINGexp of string
      | CHARexp of string
      | RECORDexp of (numberedLabel * exp) list
      | SELECTexp of numberedLabel * exp	(* record selections *)
      | VECTORexp of exp list * T.ty
      | APPexp of exp * exp
      | HANDLEexp of exp * fnrules
      | RAISEexp of exp * T.ty
      | CASEexp of exp * rule list * bool	(* true: match; false: bind *)
      | IFexp of { test: exp, thenCase: exp, elseCase: exp }
      | ANDALSOexp of exp * exp
      | ORELSEexp of exp * exp
      | FNexp of fnrules
      | LETexp of dec * exp
      | SEQexp of exp list
      | CONSTRAINTexp of exp * T.ty

    and rule = RULE of pat * exp

    and pat
      = WILDpat
      | VARpat of VarCon.var
      | NUMpat of string * num_lit	(* string is source text of literal *)
      | STRINGpat of string
      | CHARpat of string
      | CONpat of T.datacon * T.tyvar list (* See comment for VARexp *)
      | RECORDpat of {fields: (T.label * pat) list, flex: bool, typ: T.ty ref}
      | APPpat of T.datacon * T.tyvar list * pat
      | CONSTRAINTpat of pat * T.ty
      | LAYEREDpat of VarCon.var * pat
      | ORpat of pat * pat
      | VECTORpat of pat list * T.ty
      | NOpat

    and dec
      = VALdec of vb list
      | VALRECdec of rvb list
      | DOdec of exp
      | LOCALdec of dec * dec
      | SEQdec of dec list

    and vb = VB of {pat: pat, exp: exp, boundtvs: T.tyvar list,
		    tyvars: T.tyvar list ref}

     and rvb = RVB of {var: VarCon.var, exp: exp, boundtvs: T.tyvar list,
		      resultty: T.ty option, tyvars: T.tyvar list ref}

     withtype fnrules = rule list * T.ty
         and num_lit = Types.ty IntConst.t

  end (* structure Absyn *)
      
