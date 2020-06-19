(* FLINT/matchcomp/test.sml *)

structure Test =
struct

local
  structure TU = TypesUtil
  structure E = ElaboratePat
in

fun bind (dcon,env) =
    E.insert(env,TU.dataconName dcon, dcon)

val env0 = foldl bind nil Setup.dcons

fun testp (example: Absyn.pat list) =
    (print "*** patterns ***\n"; MCPrint.tppPats example; print "\n";
    let val andor = AndOr.makeAndor example
        val _ = (print "*** andor ***\n";
                 MCPrint.tppAndor andor)
	val dectree = DecisionTree.decisionTree andor
        val _ = (print "\n*** decision tree***\n";
                 MCPrint.tppDecTree dectree)
	val code = MCCode.code (andor,dectree)
    in print "\n*** code ***\n";
       MCPrint.tppCode code
    end)

fun tests (s: string) = 
    let val example = ParsePat.parsepats(s,env0)
    in testp example
    end

end (* local *)
end (* structure Test *)

