(* ElabData/matchcomp/match.sml *)

structure Match =
struct

local
  structure TU = TypesUtil
  structure E = ElaboratePat
  structure MT = MCTypes
in

(* matchComp: rules * ty -> Absyn.exp * VarCon.var *)
fun matchComp (rules: Absyn.rule list, ty: Types.ty) =
    let val patterns = map #1 rules;
	val andor = AndOr.makeAndor(patterns, ty)
        val rootvar = getSvar andor
	val (dectree,ruleCounts) = DecisionTree.decisionTree andor
        val code = MCCode.code (rules,andor,dectree,ruleCounts)
     in (code, rootvar)
    end

end (* local *)
end (* structure Match *)

