(* varenvmc.sml *)

structure VarEnvMC =
struct

local
  structure LV = LambdaVar
  structure M = LV.Map
  structure V = VarCon
  structure PU = PrintUtil
  open MCTypes
in

(* varenvMC environments
 * map "pattern variables" (i.e. source variables) to a list of all "special" 
 * (administrative) variables annotated by the rule and path where that special
 * variable corresponds to an occurrence in the patterns of the pattern variable.
*)

type varenvMC = (ruleno * path * V.var) list M.map

val empty : varenvMC = M.empty

(* lookVar : varenvMC * V.var -> (ruleno * path * V.var) list option *)
(* lookVar is called only once in MatchComp..bindSVars. *)
fun lookVar (varenvmc, var) =
    let val result = M.find (varenvmc, V.varToLvar var)
	fun pr (r,_,v) = print (concat[Int.toString r, ":", V.toString v])
    in (* case result
	of SOME bindings =>
	   (print (concat ["VarEnvMV.look: ", V.toString var, " -> ["]);
	    PU.printSequence ", " pr bindings; print "]\n")
	 | NONE =>  
	   print (concat ["VarEnvMV.look: ", V.toString var, " unbound", "\n"]); *)
        result
    end

(* bindVar : V.var * (ruleno * path * svar) * varenvmc -> varenvmc *)
(* bindVar is called only in MatchComp..bindPatVars to bind all the primary and
 * secondary pattern variables that are associated with the node defined by path
 * (in the node's info.vars and info.asvars fields), with the designated svar and
 * the ruleno specified in the (varBindings, asBindings) containing the var. *)
fun bindVar (var, (ruleno, path, svar), varenvmc) =
    if V.isWildVar var then varenvmc
    else let val lvar = V.varToLvar var
	     (* val _ = print (concat ["VarEnvMC.bindVar: ", V.toString var, "  ",
		                       V.toString svar, "\n"]) *)
	  in case lookVar(varenvmc, var)
               of NONE => M.insert(varenvmc, lvar, [(ruleno, path, svar)])
		| SOME bindings =>
		   M.insert (varenvmc, lvar, (ruleno, path, svar) :: bindings)
	 end

end (* local *)
end (* structure VarEnvMC *)
