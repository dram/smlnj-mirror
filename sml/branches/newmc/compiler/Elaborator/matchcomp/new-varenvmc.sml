(* varenvmc.sml *)

structure VarEnvMC =
struct

local
  structure S = Symbol
  structure LV = LambdaVar
  structure M = LV.Map
  structure V = VarCon
  structure PU = PrintUtil
  fun bug msg = ErrorMsg.impossible ("VarEnvMC: " ^ msg)
in

(* varenvMC environments
 * map "pattern variables" (i.e. source variables) to a list of pairs of
 * layers and "special" (administrative) variables, where that pattern variable
 * in that layer should be replaced by the designated special (svar) variable.
 *
 * In terms of the interface functions lookVar and bindVar, varenvMC is treated
 * as a partial mapping from pvars (pattern variables) and layers to svars.
*)

type varenvMC = (Layers.layer * V.var) list M.map

val empty : varenvMC = M.empty

(* lookVar : varenvMC * V.var * Layers.layer -> V.var *)
(* lookVar is called only once in MatchComp..bindSVars. *)
fun lookVar (varenvmc, var, layer) =
    (case M.find (varenvmc, V.varToLvar var)
      of SOME bindings =>
	      let fun scan (layer', svar) =
		      case Layers.layerCompare(layer', layer)
			of EQUAL => SOME svar
			 | _ => NONE
	       in case List.mapPartial scan bindings
		   of nil => bug (concat[ "lookVar: no binding for var, layer:",
					  V.varToString var, ", ", Layers.toString layer])
		     | _::_::_ =>
		       bug (concat["lookVar: multiple consistent bindings for var, layer: ",
				   V.varToString var, ", ", Layers.toString layer])
		     | [b] => b  (* there should be a unique svar for this var, rule, path *)
	      end
       | NONE => bug (concat ["lookVar: unbound pattern var: ", V.varToString var])

(* bindVar : V.var * layer * svar * varenvmc -> varenvmc *)
(* bindVar is called only in MatchComp..bindPatVars to bind all the primary and
 * secondary pattern variables that are associated with an AndOr node
 * (in the node's info.vars and info.asvars fields), with their specified layer,
 * to an svar which should be be substituted for the pattern variable in the RHS
 * for that layer. *)
fun bindVar (var, layer, svar, varenvmc) =
    if V.isWildVar var then varenvmc  (* wildcard "variables" do not need svars *)
    else let val lvar = V.varToLvar var
	     (* val _ = print (concat ["VarEnvMC.bindVar: ", V.toString var, "  ",
		                       V.toString svar, "\n"]) *)
	  in case M.find (varenvmc, lvar)
               of NONE => M.insert(varenvmc, lvar, [(layer, svar)])
		| SOME bindings =>
		   M.insert (varenvmc, lvar, (layer, svar) :: bindings)
	 end

end (* local *)
end (* structure VarEnvMC *)
