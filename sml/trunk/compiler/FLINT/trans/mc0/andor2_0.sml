(* FLINT/trans/andor2.sml *)

(* Translate proto-andor trees (andor0) to andor trees. This involves
 *  (1) computing the "defaults" rule set for OR nodes, a path component for all nodes,
 *  (2) converting the "bindings" field to brules (extracting the rules from bindings
 *      and discarding the variables, but "filtering" with the inherited "active" ruleset).
 *  (3) adding a path for each node of the andor tree.
 *  (was derived from the "flatten" functions (e.g. "flatteningAndor)
 *)

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

  fun intCon n = INTcon{ival = IntInf.fromInt n, ty = Target.defaultIntSz}

in

(* bindingsRules : varBindings * ruleset -> ruleset
   bindBindings (bindings, active):
   collect the rule parts of the bindings and filter by the active rule set;
   result is a subset of active. The variables from the bindings are discarded. *)
fun bindingsToRules (varBindings, active) =
    let fun collectRules (nil, rules) = RS.intersection(rules, active)
	  | collectRules ((rule, _)::rest, rules) =
             collectRules (rest, RS.add(rules, rule))
    in collectRules (varBindings, RS.empty)
    end

(* translateAndor : andor0 * path * ruleset -> andor
   Traverses the andor tree accumulating a bindenv binding all generated paths
   (starting with path as the root) to the associated "active" rules from
   the bindings fields in all the andor tree's nodes.

 * mkDefaults: variant list * ruleset -> ruleset
   calcDefaults (cases, active):
   take a starting set of "active" rules and successively subtract the rules
   component of each variant in the cases arg. The result is a subset of the
   original "active" ruleset that is disjoint from the rules of each caseAndor.
   Defaults are rules that are "live" at the OR node but not because they
   match the (explicit) discriminant of any variant.
   [Does this mean that they are live because of matching a variable
   (along the path to this node)? YES (check).]
   NOTE: the paths to vector "variant" nodes (children of a vector OR node) 
   end in VPI links, but do not include a VLEN link, while the path of the
   vector OR node itself terminates in a VLEN link.
*)
(* translateAndor : andor * path * ruleset -> andor *)
fun translateAndor (AND0 {bindings, children}, path, active) =
      let val _ = dbsay "flattenAndor:AND\n"
	  fun transChildren (n, nil) = nil
	    | transChildren (n, child::rest) =
	        translateAndor (child, PI n :: path, active)
	        :: transChildren(n + 1, rest)
	  val childrenDecs = transChildren (0, children)
	  val bindRules = bindingsToRules (bindings, active)
       in AND {path = path, brules = bindRules, children = childrenDecs}
      end
  | translateAndor (OR0 {bindings, cases, sign}, path, active) =
      let val bindRules = bindingsToRules(bindings, active)
	  fun getDefaults (nil, active) = active
	    | getDefaults ((_,rules,_)::rest, active)  =
	        getDefaults (rest, RS.difference (active, rules))
	  val defaults = getDefaults (cases, active) (* subset of active *)
	  fun transCase ao_case = translateCase (ao_case, path, active, defaults)
	  val path' =
	      case cases
	       of (VLENcon (_, t), _, _) :: _ => VLEN t :: path
		  (* add a VLEN link to the OR node path if it discriminates
                   * on vector length, but the variant nodes do not get the VLEN link *)
		| _ => path
	  val cases' = map transCase cases
       in OR {path = path', brules = bindRules, sign = sign,
	      defaults = defaults, cases = cases'}
      end
  | translateAndor (LEAF0 {bindings}, path, active) =
      LEAF {path = path, brules = bindingsToRules(bindings, active)}

(* translateCase : variant0 * path * ruleset * ruleset -> variant *)
and translateCase ((con, rules, subcase), path, active, defaults) =
      let val stillActive = RS.intersection (RS.union (rules, defaults), active)
	  val newActive = RS.intersection(rules, active)
      in case (con, subcase)
	  of (VLENcon (k, t), VEC elements) =>
	     let (* val path' = VLEN t :: path  -- not in flattenACase *)
		 fun transVecElems (n, nil) = nil
		   | transVecElems (n, andor::rest) =
		       translateAndor (andor, VPI (n,t) :: path, stillActive) ::
		       transVecElems (n + 1, rest)
	      in (intCon k, newActive, VEC (transVecElems(0, elements)))
	     end
	   | (DATAcon _, DCON andor) => (* non-constant datacon *)
             (con, newActive,
	      DCON (translateAndor (andor, CON con :: path, stillActive)))
	   | (_, CONST) => (* con should be constant, not verified *)
	     (con, newActive, CONST)
	   | _ => bug "translateCase: inconsistent cases"
      end

end (* local *)
end (* structure Andor2 *)
