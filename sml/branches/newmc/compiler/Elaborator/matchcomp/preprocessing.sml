(* FLINT/trans/preprocessing.sml *)

(* preprocessing patterns to expand OR patterns into additional rules sharing original RHS *)

structure Preprocessing =
struct

local
  structure DA = Access
  structure LV = LambdaVar
  structure V = VarCon		       
  structure LT = PLambdaType
  structure P = Paths
  structure MT = MCCommon
  structure MU = MCUtil
  structure PL = PLambda
  open Absyn MCCommon PLambda

  fun bug msg = ErrorMsg.impossible ("Preprocessing: " ^ msg)

  val mkLvar = LambdaVar.mkLvar
in

fun allConses (hds, tls) =
      List.concat (map (fn hd => (map (fn tl => hd::tl) tls)) hds)

(* info relating post-expansion rule numbers with the shared pre-expansion
 * rule *)
type ruleInfo =
     (P.path list   (* the andor paths of the pattern-bound variables (pvars) *)
    * LV.lvar   (* the lvar naming the abstracted rhs function *)
    * ruleno)    (* the pre-expansion ruleno *)

type ruleMap = int -> ruleInfo

(* orExpand : pat -> pat list *)
(* does not deal with MARKpat (but could if necessary, discarding the MARKpat?) *)
fun orExpand (ORpat(pat1,pat2)) =
      (orExpand pat1)@(orExpand pat2)
  | orExpand (pat as RECORDpat{fields,...}) =
     map (MU.mkRECORDpat pat) (foldr allConses [nil] (map (orExpand o #2) fields))
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
let fun processRule ((pat, rhs), (expandedRuleno, originalRuleno, ruleTable, pats, paths, rhsBinders)) =
	 let val pat = AbsynUtil.stripPatMarks pat
	     val patVariables = AbsynUtil.patVariables pat (* works even with OR patterns *)

	     (* abstractRHS : V.var list * PL.lexp -> PL.lexp *)
             (* Assumption: the translated rhs incorporates the lvar(s) of the pattern pvar(s) *)
	     fun abstractRHS ([], rhs) = FN(mkLvar (), LT.ltc_unit, rhs)  (* no pvars *)
	       | abstractRHS ([var], rhs) =
		   let val argLvar = V.varToLvar var
		       val argLty = toLty (V.varToType var)
		    in FN (argLvar, argLty, rhs)
		   end
	       | abstractRHS (vars, rhs) =  (* multiple pattern variables *)
		   let val argLvar = mkLvar () (* one lvar "bound to" tuple of pat variables' values *)
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

	     val fvar: LV.lvar = mkLvar ()  (* lvar to which rhs function is bound *)

             (* rhsFunBinder : PL.lexp -> PL.lexp *)
	     val rhsFunBinder = fn (body: PL.lexp) => PL.LET (fvar, rhsFun, body)

	     (* list of pats produced by or-expansion of pat (ramification of pat)
              *  all pats in this ramified family share the same bound pvars and fvar *)
	     val ramifiedPats = orExpand pat

	     (* number of sub-rules produced by or-expansion of pat *)
	     val expansion = length ramifiedPats

	     val nextExpandedRuleno = expandedRuleno + expansion
	     val nextOriginalRuleno = originalRuleno + 1

	     val blockVarPaths: P.path list list =
		 map (MU.bindingPaths patVariables) ramifiedPats						   
	 in (nextExpandedRuleno, nextOriginalRuleno,
	     (expandedRuleno, originalRuleno, expansion, patVariables, fvar)::ruleTable,
	     ramifiedPats::pats, blockVarPaths::paths, rhsFunBinder::rhsBinders)
	 end

    val (numRules, _, ruleTable, patss, pathss, rhsBinders) =
	  foldl processRule (0, 0, nil, nil, nil, nil) rules

    val expandedPats : pat list = List.concat (rev patss)

    val varPaths : P.path list list = List.concat (rev pathss)

    val ruleTable = rev ruleTable

    (* ruleMap : ruleMap
     *  r: ruleno is index in OR-expanded rules *)
    fun ruleMap (r: ruleno) : ruleInfo =
        let fun loop nil = bug "lookTable"
	      | loop ((exr, orr, sz, pvars, fvar) :: rest) =
		if r >= exr andalso r < exr + sz
		then (fvar, orr)
		else loop rest
	    val (fvar, orr) = loop ruleTable
	    val paths = List.nth (varPaths, r)
	 in (paths, fvar, orr)
        end

    (* ASSERT: length expandedPats = numRules *)
 in (numRules, expandedPats, rhsBinders, ruleMap)
end (* fun expandPats *)

end (* local *)
end (* structure Preprocessing *)
