(* FLINT/matchcomp/test.sml *)

structure Test =
struct

fun test (example: Absyn.pat list) =
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

end (* structure Test *)
    
