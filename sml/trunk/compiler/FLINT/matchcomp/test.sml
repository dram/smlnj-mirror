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
    let val ao = AndOr.makeAndor example
        val _ = print "*** andor ***\n"
        val _ = MCPrint.tppAndor ao
	val dtOp = DecisionTree.decisionTree ao
		   handle _ => NONE
      in (case dtOp
	    of SOME dt => 
                 (print "\n*** decision tree***\n";
                  MCPrint.tppDecTree dt;
		  let val cd = MCCode.code (ao,dt)
		  in print "\n*** code ***\n";
		     MCPrint.tppCode cd;
		     print "\n"
		  end)
	     | NONE => print "\n*** No decision tree ***\n")
    end

fun tests (s: string) = 
    let val example = ParsePat.parsepats(s,env0)
    in testp example
    end

end (* local *)
end (* structure Test *)

