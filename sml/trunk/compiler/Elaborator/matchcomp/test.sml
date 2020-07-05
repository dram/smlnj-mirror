(* FLINT/matchcomp/test.sml *)

structure Test =
struct

local
  structure TU = TypesUtil
  structure E = ElaboratePat
  structure MT = MCTypes
in

fun bind (dcon,env) =
    E.insert(env,TU.dataconName dcon, dcon)

val env0 = foldl bind nil Setup.dcons

fun testp (rules: MT.rule list, polyTy: Types.polyTy) =
    let val patterns = map #1 rules;
	val _ = (print "*** patterns ***\n";
		 MCPrint.tppPats patterns; print "\n")
        val (typevars, ty) = TU.instantiatePoly polyTy
	val andor = AndOr.makeAndor(patterns, ty)
        val _ = (print "*** andor ***\n";
                 MCPrint.tppAndor andor)
	val (dectree,ruleCounts) = DecisionTree.decisionTree andor
        val _ = (print "\n*** decision tree***\n";
                 MCPrint.tppDecTree dectree)
	val code = MCCode.code (rules,andor,dectree,ruleCounts,typevars)
     in print "\n*** code ***\n";
	MCPrint.tppCode code
    end

fun tests (s: string, polyTy) = 
    let val pats = ParsePat.parsepats(s,env0)
	val rules = map (fn p => (p, Absyn.STRINGexp "x")) pats
    in testp (rules, polyTy)
    end

end (* local *)
end (* structure Test *)

