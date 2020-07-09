(* vcode.sml *)

(* virtual version of mcexp where pseudo-constructors translate to absyn
 * A pseudo-constructor function is defined for each constructor of the mcexp
 * (e.g. vVar for Var) *)

structure Vcode =
struct

local
  structure A = Absyn
  structure SV = SVar  (* simplified (virtual) variables *)
  structure MT = MCTypes
  open Absyn
in

type mcexp = A.exp

type branch = MT.key * SV.svar option * mcexp

fun mkVarExp svar =
    VARexp(ref(SV.svarToVar sv, nil)

(* mkLet : SV.svar * mcexp * mcexp -> mcexp *)
(* "let svar = defexp in body" *)
fun mkLet (svar, defexp, body) = 
    LETexp(VALdec [VB{pat = VARpat(SV.svarToVar sv),
		      exp = defexp,
		      boundtvs = nil,
		      tyvars = ref nil}],
	   body)

fun mkTupplePat () = ()
		
fun caseToRule (key, svarOp, rhsexp) =
    let val pat = (*make pattern from key and svarOp *)
	    (case key
	      of (D dcon) =>
		 (case svarOp
		   of NONE => CONpat(dcon, nil)
		    | SOME sv => APPpat(dcon, nil, VARpat(SV.svarToVar sv)))
	       | I num => NUMpat("", num)
	       | S s => STRINGpat s
	       | C c => CHARpat c
        (* what about V? *)
     in RULE(pat, rhsexp)
    end

(* vVar: SV.svar -> mcexp *)
fun vVar (sv : SV.svar) = A.VARexp(ref(SV.svarToVar sv), nil)

(* vLetr : SV.svar * SV.svar list * mcexp -> mcexp *)
(* "let svars = svar in body" *)
fun vLetr (svar, svars, body) =
    let val defvar = SV.svarToVar svar
	fun wrapLets (nil, _) = body
	  | wrapLets (sv::rest, n) = 
	      mkLet(sv, <<select>>(n, defvar),
		    wrapLets (rest, n+1))
    in wrapLets (svars, 0)
    end

(* vLetf : SV.svar * mcexp * mcexp -> mcexp *)
(* "let svar = funexp in body" *)
fun vLetf (svar, funexp, body) = mkLet(svar, funexp, body)

(* vLetr : V.var list * SV.svar list * mcexp -> mcexp *)
fun vLetm (vars, svars, body) =
    let fun wrapLets (nil,nil) = body
	  | wrapLets (v::restv, sv::restsv) = 
	    mkLet(v, VARexp(ref(SV.svarToVar sv), nil),
		  wrapLets(restv,restsv))
	  | wrapLets _ = bug "vLetm"
    in wrapLets (vars,svars)
    end

(* vCase : SV.svar * branch list * mcexp option *)
fun vCase (svar, cases, defaultOp) =
    let val scrutinee = vVar svar
	val rules = map caseToRule case
        val rules' =
	    case defaultOp
	      of NONE => rules
	       | SOME exp => rules@[RULE(WILDpat, exp)]
     in CASEexp(scrutinee, rules', << "match or bind?" >>)
    end

(* vSfun : V.var list * mcexp -> mcexp *)
fun vSfun (pvars, body) =
    let val rule = RULE(mkTuplePat pvars, body)
	val ty = << ??? >>
    in FUNexp([rule], ty)
    end

(* vSapp : SV.svar * SV.svar list -> mcexp *)
fun vSapp (funsvar, argsvars) =
    APPexp(vVar funsvar, mkTupleExp(map vVar argsvars))

(* vMatch : mcexp *)
val vMatch = RAISEexp(<< Match exn >>, << ty? >>)

end (* local *)
end (* structure Vcode *)

(*
datatype mcexp
  = Var of SV.svar  (* how/where used? *)
  | Letr of SV.svar * SV.svar list * mcexp  (* destructure an AND *)
  | Letf of SV.svar * mcexp * mcexp   (* 1st mcexp will be an Sfun *)
  | Letm of V.var list * SV.svar list * Absyn.exp  (* non-functionalized RHS *)
  | Case of SV.svar * (key * SV.svar option * mcexp) list * mcexp option
      (* destructure an OR, with svar binding if key is not a constant *)
  | Sfun of V.var list * Absyn.exp  (* functionalized RHS expression *)
  | Sapp of SV.svar * SV.svar list  (* A-normal-style. Function and args have all been
				     * bound to svars *)
  | Tfun of T.typevar list * mcexp    (* type function bindings tyvars *)
  | MATCH  (* raise a match exception -- may be redundant if matches guaranteed exhaustive *)
*)			
