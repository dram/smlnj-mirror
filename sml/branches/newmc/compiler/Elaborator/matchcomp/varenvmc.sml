(* varenvmc.sml *)

structure VarEnvMC =
struct

local
  structure LV = LambdaVar
  structure M = LV.Map
  structure V = VarCon
  open MCTypes
in

type varenvMC = (ruleno * path * V.var) list M.map

val empty : varenvMC = M.empty

(* lookVar : varenvMC * V.var -> (ruleno * path * V.var) list option *)
fun lookVar (varenvmc, var) = M.find (varenvmc, V.varToLvar var)

(* bindVar : V.var * (ruleno * path * svar) * varenvmc -> varenvmc *)
fun bindVar (var, (ruleno, path, svar), varenvmc) =
    let val lvar = V.varToLvar var
     in case lookVar(varenvmc, var)
          of NONE => M.insert(varenvmc, lvar, [(ruleno, path, svar)])
           | SOME bindings =>
	     M.insert(varenvmc, lvar, (ruleno, path, svar) :: bindings)
    end

end (* local *)
end (* structure VarEnvMC *)
