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

type varenvMC = (ruleno * path * V.var) list M.map

val empty : varenvMC = M.empty

(* lookVar : varenvMC * V.var -> (ruleno * path * V.var) list option *)
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
fun bindVar (var, (ruleno, path, svar), varenvmc) =
    if V.isWildVar var then varenvmc
    else let val lvar = V.varToLvar var
	     (* val _ = print (concat ["VarEnvMC.bindVar: ", V.toString var, "  ", V.toString svar, "\n"]) *)
	  in case lookVar(varenvmc, var)
               of NONE => M.insert(varenvmc, lvar, [(ruleno, path, svar)])
		| SOME bindings =>
		   M.insert (varenvmc, lvar, (ruleno, path, svar) :: bindings)
	 end

end (* local *)
end (* structure VarEnvMC *)
