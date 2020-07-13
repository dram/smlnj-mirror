(* vcode.sml *)

(* virtual version of mcexps where pseudo-constructors translate to absyn
 * A pseudo-constructor function is defined for each constructor of the mcexp
 * (e.g. vVar for Var) *)

structure Vcode =
struct

local
  structure A = Absyn
  structure AU = AbsynUtil
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

(* caseToRule : MCTypes.key * SV.svar option * Absyn.exp -> Absyn.rule *)
(* svarOp is SOME sv if key is D dcon where dcon is not constant *)
fun caseToRule (key, svarOp, rhsexp) =
    let val pat = (*make pattern from key and svarOp *)
	    (case key
	      of (D dcon) =>
		 (case svarOp
		   of NONE => CONpat(dcon, nil)  (* => constant dcon *)
		    | SOME sv => APPpat(dcon, nil, VARpat(SV.svarToVar sv)))
	       | I num => NUMpat("", num)
	       | S s => STRINGpat s
	       | C c => CHARpat c
        (* what about V? *)
     in RULE(pat, rhsexp)
    end

(* vVar: SV.svar -> mcexp *)
(* Convert an svar to a VarCon.var and treat as expression. 
 * Should the tyvar list (polymorphic instantiation args) be reconstructed? *)
fun vVar (sv : SV.svar) = A.VARexp(ref(SV.svarToVar sv), nil)

(* vLetr : SV.svar list * SV.svar * mcexp -> mcexp *)
(* "let (sv1,...,svn) = sv0 in body"; destructure a tuple *)
fun vLetr (svars, svar, body) =
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
(* "let (v1, ..., vn) = (sv1, ..., svn) in rhsexp" *)
fun vLetm (vars, svars, body) =
    let fun wrapLets (nil,nil) = body
	  | wrapLets (v::restv, sv::restsv) = 
	    mkLet(v, VARexp(ref(SV.svarToVar sv), nil),
		  wrapLets(restv,restsv))
	  | wrapLets _ = bug "vLetm"
    in wrapLets (vars,svars)
    end

(* vCase1 : SV.svar * T.datacon * SV.svar * mcexp -> mcexp *)
fun vCase1 (svar, dcon, svardest, rhsexp) =
    let val scrutinee = vVar svar
	val rule = caseToRule (D dcon, SOME svardest, rhsexp)
     in CASEexp(scrutinee, [rule], true)  (* true signals vCase1 form; kludge! *)
    end

(* vCase : SV.svar * branch list * mcexp option -> mcexp *)
(* The default, if available, is integrated into the rules. *)
fun vCase (svar, cases, defaultOp) =
    let val scrutinee = vVar svar
	val rules = map caseToRule case
        val rules' =
	    case defaultOp
	      of NONE => rules
	       | SOME exp => rules@[RULE(WILDpat, exp)]
     in CASEexp(scrutinee, rules', false)  (* false signals vCase form; kludge! *)
    end

(* vSfun : V.var list * mcexp -> mcexp *)
(* "fn (v0, ..., vn) => body"; functionalized, multi-use rule RHS *)
fun vSfun (pvars, body) =
    let val pats = map (fn sv => VARpat(SV.svarToVar sv)) pvars
	val rule = RULE(TUPLEpat pats, body)
	val ty = << ty??? >>
     in FNexp([rule], T.UNDEFty)  (* FNexp always applied to UNDEFty in elabcore.sml *)
    end

(* vSapp : SV.svar * SV.svar list -> mcexp *)
fun vSapp (funsvar, argsvars) =
    APPexp(vVar funsvar, AU.TUPLEexp(map vVar argsvars))

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
