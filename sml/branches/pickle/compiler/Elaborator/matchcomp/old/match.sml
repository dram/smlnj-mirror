(* ElabData/matchcomp/match.sml *)

structure MatchComp =
struct

local
  structure AS = Absyn
  structure T = Types
  structure MT = MCTypes
  structure MU = MatchUtil
  structure PP = PrettyPrint
  structure PU = PPUtil
  structure ED = ElabDebug
  (* Uses AndOr, DecisionTree, MCCode *)

  val printAndor = ElabControl.printAndor
  val printDecisionTree = ElabControl.printDecisionTree
  val printMatchAbsyn = ElabControl.printMatchAbsyn

  fun ppAndor andor =
      PP.with_default_pp
	  (fn ppstrm =>
	      (PP.string ppstrm "andor:\n";
	       MCPrint.ppAndor ppstrm andor))

  fun ppDecisionTree dectree =
      PP.with_default_pp
	  (fn ppstrm =>
	      (PP.string ppstrm "dectree:\n";
	       MCPrint.ppDecTree ppstrm dectree))

  fun ppCode exp =
      PP.with_default_pp
          (fn ppstrm =>
	      (PP.string ppstrm "Absyn.exp:\n";
	       PPAbsyn.ppExp (StaticEnv.empty, NONE) ppstrm (exp, 20)))

in

(* matchComp: AS.rule list * T.ty * T.datacon -> AS.exp * VarCon.var *)
(* lhsTy is the type of the patterns (i.e. the domain or lhs) of the match
 * Called by transMatch.  fillPat has been applied to rule patterns. *)
fun matchComp (rules: AS.rule list, lhsTy: T.ty, rhsTy: T.ty, varenvAC,
	       matchFailExn: T.datacon) =
    let val _ = print ">>> matchComp\n" (* -- debugging *)
	val patterns = map (fn (AS.RULE(pat,_)) => pat) rules (* strip RULE constructor *)
	val andor = AndOr.makeAndor(patterns, lhsTy)
	val _ = if !printAndor
		then ppAndor andor
		else ()
	val (dectree,ruleCounts) = DecisionTree.decisionTree andor
	val _ = if !printDecisionTree
		then ppDecisionTree dectree
		else ()
        val (matchExp, rootVar) =
	    MCCode.genMatch (rules, andor, dectree, ruleCounts, rhsTy,
			     varenvAC, matchFailExn)
	val _ = if !printMatchAbsyn
		then ED.withInternals (fn () => ppCode matchExp)
		else ()
    in print "<<< matchComp\n";  (* -- debugging *)
       (matchExp, SVar.svarToVar rootvar)
    end

end (* local *)
end (* structure MatchComp *)
