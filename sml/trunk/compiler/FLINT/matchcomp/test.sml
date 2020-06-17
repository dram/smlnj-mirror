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

fun test0 (example: Absyn.pat list) =
    let val ao = AndOr.makeAndor example
	val dtOp = DecisionTree.decisionTree ao
    in print "*** andor ***\n";
       MCPrint.tppAndor ao;
       case dtOp
	of SOME dt => 
	   let val cd = MCCode.code (ao,dt)
            in print "\n*** decision tree***\n";
               MCPrint.tppDecTree dt;
	       print "\n*** code ***\n";
	       MCPrint.tppCode cd;
	       print "\n"
	   end
	 | NONE => print "\n*** No decision tree ***\n"
    end

fun test1 (s: string) = 
    let val example = ParsePat.parsepats(s,env0)
    in test0 example
    end

end (* local *)
end (* structure Test *)

