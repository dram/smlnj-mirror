(* FLINT/trans/preprocessing.sml *)

(* preprocessing patterns to expand OR patterns into additional rules sharing original RHS *)

structure Preprocessing =
struct

local
  structure DA = Access
  structure LV = LambdaVar
  structure V = VarCon		       
  structure LT = PLambdaType
  structure MT = MCCommon
  structure PL = PLambda
  open Absyn MCCommon PLambda

  fun bug msg = ErrorMsg.impossible ("Preprocessing: " ^ msg)

  val mkv = LambdaVar.mkLvar
in

fun allConses (hds, tls) =
      List.concat (map (fn hd => (map (fn tl => hd::tl) tls)) hds)

(* info relating post-expansion rule numbers with the shared pre-expansion
 * rule *)
type ruleInfo =
     (V.var list   (* the (common) pattern-bound variables (pvars) *)
      * LV. lvar   (* the lvar naming the abstracted rhs function *)
      * ruleno)    (* the pre-expansion ruleno *)

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

(* expandPats : (T.ty -> LT.lty)  (* type translation *)
                -> (pat * lexp) list  (* hybrid pre-expansion rules *)
                -> int * pat list * (PL.lexp -> PL.lexp) list * ruleMap
 *  preProcessPat is applied to each hybrid rule in the match. All the patterns
 *  in the ramifiedLHS are derived from the original single pattern by OR expansion.
 *)
fun expandPats toLty rules =
let
    fun processRule ((pat, rhs), (expandedRuleno, originalRuleno, ruleTable, pats, rhsBinders)) =
	 let val pat = AbsynUtil.stripPatMarks pat
	     val patVariables = AbsynUtil.patVariables pat (* works even with OR patterns *)

	     (* abstractRHS : V.var list * PL.lexp -> PL.lexp *)
             (* Assumption: the translated rhs incorporates the lvar(s) of the pattern pvar(s) *)
	     fun abstractRHS ([], rhs) = FN(mkv(), LT.ltc_unit, rhs)  (* no pvars *)
	       | abstractRHS ([var], rhs) =
		   let val argLvar = V.varToLvar var
		       val argLty = toLty (V.varToType var)
		    in FN (argLvar, argLty, rhs)
		   end
	       | abstractRHS (vars, rhs) =
		   let val argLvar = mkv () (* one lvar "bound to" tuple of pat variables' values *)
		       fun wrapLet (nil, n) = (rhs, nil)
			 | wrapLet (v::rest, n) =
			     let val lv  = V.varToLvar v
				 val lt = toLty (V.varToType v)
				 val (le, tt) = wrapLet (rest, n+1)
			      in (LET(lv, SELECT(n, VAR argLvar), le), lt :: tt)
			     end
		       val (body, tt) = wrapLet (vars, 0)
		    in FN (argLvar, LT.ltc_tuple tt, body)
		   end

	     val rhsFun = abstractRHS (patVariables, rhs)

	     val fvar: LambdaVar.lvar = mkv ()  (* lvar to which rhs function is bound *)

             (* rhsFunBinder : PL.lexp -> PL.lexp *)
	     val rhsFunBinder = fn (body: PL.lexp) => PL.LET (fvar, rhsFun, body)

	     (* list of pats produced by or-expansion of pat (ramification of pat)
              *  all pats in this ramified family share the same bound pvars fvar *)
	     val ramifiedPats = orExpand pat

	     (* number of sub-rules produced by or-expansion of pat *)
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
    fun ruleMap (r: ruleno) =
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
