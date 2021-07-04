(* FLINT/trans/andor2.sml *)

(* Translate original andor tree to andorD tree, a slightly augmented form of
 * andor tree. (was "flatteningAndor, etc.) *)

structure Andor2 =
struct

local
  open MCCommon
  structure RS = RuleSet

  val debugging = FLINT_Control.mcdebugging

  val say = Control_Print.say
  fun newline () = say "\n"
  fun saynl msg = (say msg; newline())
  fun says strings = saynl (concat strings)

  fun dbsay msg =
      if !debugging
      then (say msg; newline())
      else ()
  fun dbsays msgs = dbsay (concat msgs)

  fun bug msg = ErrorMsg.impossible ("Flattening: " ^ msg)

in

(* bindingsRules : varBindings * ruleset -> ruleset
   bindBindings (bindings, active):
   collect the rule parts of the bindings and filter by the active rule set;
   result is a subset of active. The variables from the bindings are discarded. *)
fun bindingsRules (varBindings, active) =
    let fun collectRules (nil, rules) = RS.intersection(rules, active)
	  | collectRules ((rule, _)::rest, rules) =
             collectRules (rest, RS.add(rules, rule))
    in collectRules (varBindings, RS.empty)
    end

(* translateAndor : andor * path * ruleset -> andorD
   Traverses the andor tree accumulating a bindenv binding all generated paths
   (starting with path as the root) to the associated "active" rules from
   the bindings fields in all the andor tree's nodes.

 * calcDefaults: case list * ruleset -> ruleset
   calcDefaults (cases, active):
   take a starting set of "active" rules and successively subtract the rules
   component of each "case" in the cases arg. The result is a subset of the
   original "active" ruleset that is disjoint from the rules of each caseAndor.
*)
(* translateAndor : andor * path * ruleset -> andorD *)
fun translateAndor (AND {bindings, children}, path, active) =
      let val _ = dbsay "flattenAndor:AND\n"
	  fun transChildren (n, nil) = nil
	    | transChildren (n, child::rest) =
	        translateAndor (child, PIPATH(n,path), active)
	        :: transChildren(n + 1, rest)
	  val childrenDecs = transChildren (0, children)
	  val bindRules = bindingsRules (bindings, active)
       in ANDd {path = path, brules = bindRules, children = childrenDecs}
      end
  | translateAndor (OR {bindings, cases, sign}, path, active) =
      let val bindRules = bindingsRules(bindings, active)
	  fun calcDefaults (nil, active) = active
	    | calcDefaults ((_,rules,_)::rest, active)  =
	        calcDefaults (rest, RS.setDifference (active, rules))
	  val defaults = calcDefaults(cases, active) (* subset of active *)
	  fun transCase ao_case = translateCase (ao_case, path, active, defaults)
	  val casesD = map transCase cases
       in ORd {path = path, brules = bindRules, path = path, sign = sign,
	       defaults = defaults, cases = casesD}
      end
  | translateAndor (LEAF {bindings}, path, active) =
      LEAFd {path = path, brules = bindingsRules(bindings, active))}

(* translateCase : caseA * path * ruleset * ruleset -> caseD *)
and translateCase ((pcon, rules, subcase), path, active, defaults) =
      let val stillActive = RS.intersection (RS.union (rules, defaults), active)
	  val newActive = RS.intersection(rules, active)
      in case (pcon, subcase)
	  of (VLENpcon (k, t), VEC elements) =>
	     let val path' = VLENPATH (k, t, path)
		 fun transVecElems (n, nil) = nil
		   | transVecElems (n, andor::rest) =
		     translateAndor (andor, VPIPATH(n,t,path'), stillActive) ::
		     transVecElems (n + 1, rest)
	      in (pcon, newActive, VEC (transVecElems(0, elements)))
	     end
	   | (DATApcon _, DCON andor) => (* non-constant datacon *)
             (pcon, newActive,
	      DCON (transAndor (andor, CONPATH (pcon, path), stillActive)))
	   | (_, CONST) => (* pcon should be constant, not verified *)
	     (pcan, newActive, CONST)
	   | _ => bug "translateCase: inconsistent cases")
      end

end (* local *)
end (* structure Andor2 *)
