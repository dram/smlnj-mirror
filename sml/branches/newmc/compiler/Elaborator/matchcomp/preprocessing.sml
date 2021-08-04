(* FLINT/trans/preprocessing.sml *)

(* preprocessing patterns to expand OR patterns into additional rules sharing original RHS *)

structure Preprocessing =
struct

local

  structure BT = BasicTypes
  structure V = VarCon		       
  structure AS = Absyn
  structure AU = AbsynUtil
  structure MC = MCCommon
  structure MU = MCUtil
  open Absyn MCCommon

  fun bug msg = ErrorMsg.impossible ("Preprocessing: " ^ msg)

in

fun allConses (hds, tls) =
      List.concat (map (fn hd => (map (fn tl => hd::tl) tls)) hds)

(* info relating post-expansion rule numbers with the shared pre-expansion
 * rule *)
type ruleInfo =
     (V.var list   (* the (common) pattern-bound variables (pvars) *)
    * V.var        (* the var naming the abstracted rhs function *)
    * ruleno)      (* the pre-expansion ruleno *)

type ruleMap = int -> ruleInfo

(* orExpand : pat -> pat list *)
(* does not deal with MARKpat *)
fun orExpand (ORpat(pat1,pat2)) =
      (orExpand pat1)@(orExpand pat2)
  | orExpand (pat as RECORDpat{fields,...}) =
     map (mkRECORDpat pat) (foldr allConses [nil] (map (orExpand o #2) fields))
  | orExpand (VECTORpat(pats,t)) =
      map (fn p => VECTORpat(p,t)) (foldr allConses [nil] (map orExpand pats))
  | orExpand (APPpat(k,t,pat)) =
      map (fn pat => APPpat(k,t,pat)) (orExpand pat)
  | orExpand (CONSTRAINTpat(pat,_)) =
      orExpand pat
  | orExpand (LAYEREDpat(CONSTRAINTpat(lpat, _), bpat)) =
      orExpand (LAYEREDpat(lpat, bpat))
  | orExpand (LAYEREDpat(lpat, bpat)) =
      map (fn pat => LAYEREDpat(lpat,pat)) (orExpand bpat)
  | orExpand pat = [pat]

(* expandPats : (pat * lexp) list  (* pre-expansion rules *)
                -> int * pat list * (exp -> exp) list * ruleMap
 *  preProcessPat is applied to each hybrid rule in the match. All the patterns
 *  in the ramifiedLHS are derived from the original single pattern by OR expansion.
 *)
fun expandPats (rules, argTy, resTy) =
let
    fun processRule ((pat, rhs), (expandedRuleno, originalRuleno, ruleTable, pats, rhsBinders)) =
	 let val pat = AU.stripPatMarks pat
	     val patVariables = AU.patternVars pat (* works even with OR patterns *)

	     (* abstractRHS : V.var list * AS.exp -> AS.exp *)
	     fun abstractRHS (pvars, rhs) =
		 (case pvars
		   of nil =>
		        let val argVar = V.newVALvar (Symbol.varSymbol "marg", BT.unitTy)
			 in AS.FNexp ([RULE(AS.VARpat(argVar), rhs)], BT.unitTy, resTy)
			end
		    | [var] =>
		        let val varTy = V.varType var
			in AS.FNexp ([AS.RULE(AS.VARpat var, rhs)], varTy, resTy)
			end
		    | vars =>
			let val argTy = BT.tupleTy (map V.varType vars)
			    val argVar = V.newVALvar (Symbol.varSymbol "margs", argTy)
			    fun wrapLet (nil, n) = rhs
			      | wrapLet (v::rest, n) =
				  MU.mkLetVar(v, AS.RSELECTexp (argVar, n), wrapLet (rest, n+1))
			    val body = wrapLet (vars, 0)
			 in AS.FNexp ([AS.RULE (AS.VARpat(argVar), body)], argTy, resTy)
			end)

	     val rhsFun = abstractRHS (patVariables, rhs)
   
             (* var naming the abstracted rhs function *)
	     val fvar: V.var = V.newVALvar (Symbol.varSymbol "fvar", BT.--> (argTy, resTy))

             (* rhsFunBinder : AS.exp -> AS.exp *)
	     val rhsFunBinder = fn (body: AS.exp) => MU.mkLetVar (fvar, rhsFun, body)

	     (* list of pats produced by or-expansion of pat (ramification of pat)
              *  all pats in this ramified family share the same bound pvars fvar *)
	     val ramifiedPats = orExpand pat

	     (* number of rules produced by or-expansion of pat *)
	     val expandSize = length ramifiedPats

	     val nextExpandedRuleno = expandedRuleno + expandSize
	     val nextOriginalRuleno = originalRuleno + 1

	 in (nextExpandedRuleno, nextOriginalRuleno,
	     (expandedRuleno, originalRuleno, expandSize, patVariables, fvar)::ruleTable,
	     ramifiedPats::pats, rhsFunBinder::rhsBinders)
	 end

    val (numRules, _, ruleTable, patss, rhsBinders) = foldl processRule (0, 0, nil, nil, nil) rules

    val ruleTable = rev ruleTable

    (* ruleMap : ruleMap *)
    fun ruleMap (r: ruleno) : (V.var list * V.var * ruleno) =
        let fun loop nil = bug "lookTable"
	      | loop ((exr, orr, sz, pvars, fvar) :: rest) =
		if r >= exr andalso r < exr + sz
		then (pvars, fvar, orr)
		else loop rest
	 in loop ruleTable
        end

    val expandedPats = List.concat (rev patss)

    (* ASSERT: length expandedPats = numRules *)
 in (numRules, expandedPats, rhsBinders, ruleMap)
end

end (* local *)
end (* structure Preprocessing *)
