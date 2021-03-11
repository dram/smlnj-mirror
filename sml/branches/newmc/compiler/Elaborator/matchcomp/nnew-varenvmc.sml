(* varenvmc.sml *)

structure VarEnvMC =
struct

local
  structure S = Symbol
  structure LV = LambdaVar
  structure M = LV.Map
  structure V = VarCon
  structure L = Layers
  structure PU = PrintUtil

  val debugging = ElabControl.mcdebugging
  val say = Control_Print.say
  fun newline () = say "\n"
  fun dbsay msg =
      if !debugging
      then (say msg; newline())
      else ()
  fun dbsays msgs = dbsay (concat msgs)
  fun bug msg = ErrorMsg.impossible ("VarEnvMC: " ^ msg)

in

(* varenvMC environments
 * map "pattern variables" (i.e. source variables) to a list of pairs of
 * layers and "special" (administrative) variables, where that pattern variable
 * in that layer should be replaced by the designated special (svar) variable.
 *
 * In terms of the interface functions lookVar and bindVar, varenvMC is treated
 * as a partial mapping from pvars (pattern variables) and layers to svars.
 *
 * The high-level function of varenvMC environments is to be able to "disambiguate"
 * variables that have multiple bindings in different (OR) layers, e.g. a pattern
 * variable x that appears in both branches of an OR pattern p | q. It is also 
 * used to deal with a single variable that occurs in multiple rules (vertical layers).
 *
 * The varenvMC environment must be threaded through the entire "code generation"
 * machinery (MatchComp.genAndor and MatchComp.genDecTree). In other words, the varenvMC
 * needs to accumulate bindings for _all_ pattern variables, no matter where they occur
 * in the pattern (i.e. within constructor (OR) subpatterns).
 * 
 * Q: What is the exact role of varenvMC in maintaining the FLINT requirement of unique
 * lvar bindings?  (i.e. in comparison with svarenv)
*)

type varenvMC = (L.layer * V.var) list M.map

val empty : varenvMC = M.empty

(* lookVar : varenvMC * V.var * L.layer -> V.var *)
(* lookVar is called only once in MatchComp..bindSVars. *)
fun lookVar (varenvmc, var, layer) =
    (case M.find (varenvmc, V.varToLvar var)
      of SOME bindings => (* var's lvar is bound in varenvMC *)
	      let fun scan (layer', svar) =
		      case L.layerCompare(layer', layer)
			of EQUAL => SOME svar
			 | _ => NONE
	       in case List.mapPartial scan bindings
		   of nil => (* no binding matches layer *)
		        bug (concat[ "lookVar: no binding for var, layer: ",
				     V.toString var, ", ", L.layerToString layer])
		     | _::_::_ => (* multiple bindings match layer *)
		        bug (concat["lookVar: multiple consistent bindings for var, layer: ",
				    V.toString var, ", ", L.layerToString layer])
		     | [svar] => (* a unique svar binding matches layer *)
		       (dbsays [">> lookVar: ", V.toString var, " @ ",
				Layers.layerToString layer, " --> ", V.toString svar];
			svar)
	      end
       | NONE => bug (concat ["lookVar: unbound pattern var: ", V.toString var]))

(* bindVar : V.var * layer * svar * varenvmc -> varenvmc *)
(* bindVar is called only in MatchComp..bindPatVars to bind all the primary and
 * secondary pattern variables that are associated with an AndOr node
 * (in the node's info.vars and info.asvars fields), with their specified layer,
 * to an svar which should be be substituted for the pattern variable in the RHS
 * for that layer. The var will come frome either some nodes info.vars or info.asvars,
 * and will not be a "wildcard" variable. *)
fun bindVar (var, layer, svar, varenvmc) =
    if V.isWildVar var then varenvmc  (* don't bind V.wildVar in an varenvMC *)
    else let val lvar = V.varToLvar var
	     val _ = dbsays ["VarEnvMC.bindVar: ", V.toString var, " @ ",
			     Layers.layerToString layer,  " --> ", V.toString svar]
	  in case M.find (varenvmc, lvar)
	      of NONE => M.insert(varenvmc, lvar, [(layer, svar)])
	       | SOME bindings =>
		   M.insert (varenvmc, lvar, (layer, svar) :: bindings)
	 end

end (* local *)
end (* structure VarEnvMC *)
