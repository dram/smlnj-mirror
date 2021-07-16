structure FlattenAndOrs =
struct

local open MCCommon
in

(* addABinding : path * ruleno * decision list -> decision list *)
fun addABinding (path, rule, nil) = [BINDDEC{path=path, rules=[rule]}]
  | addABinding (path, rule, (bind as BINDDEC{path=path', rules=rules})::rest) =
      if pathEq(path, path') then BINDDEC{path=path, rules=rule::rules} :: rest
      else bind::(addABinding(path, rule, rest))
  | addABinding _ = bug "addABinding - non BINDDEC in binding list"

(* flattenBindings : (ruleno * var) list * path * ruleset -> decision list *)
fun flattenBindings (nil, path, active) = nil
  | flattenBindings (((rule, v)::rest), path, active) =
      if member(rule, active) then
	addABinding(path, rule, flattenBindings(rest, path,active))
      else
	flattenBindings(rest, path, active)

(* flattenAndor : andor * path * ruleset -> decision list *)
and flattenAndor (AND {bindings, subtrees}, path, active) =
      let val btests = flattenBindings(bindings, path, active)
	  fun dotree (n, nil) = nil
	    | dotree (n, subtree::rest) =
	        flattenAndor(subtree,PIPATH(n,path),active) @ dotree(n + 1, rest)
       in btests @ dotree(0, subtrees)
      end
  | flattenAndor (OR {bindings, cases, sign}, path, active) =
      let val btests = flattenBindings(bindings, path, active)
       in btests @ [flattenCases(cases, path, active, sign))]
      end
  | flattenAndor (LEAF {bindings}, path, active) = flattenBindings(bindings, path, active)

(* flattenACase : (pcon * ruleset * andor list) * path * ruleset * ruleset
		  -> pcon * ruleset * decision list *)
and flattenACase ((VLENpcon(n, t), ruleset, subtrees), path, active, defaults) =
      let val stillActive = intersect(union(ruleset, defaults), active)
	  val ruleActive = intersect(ruleset, active)
	  fun flattenVSubs (n, nil) = nil
	    | flattenVSubs (n, subtree::rest) =
	      (flattenAndor(subtree, VPIPATH(n,t,path), stillActive))
	      @ (flattenVSubs(n + 1, rest))
       in (intCon n, ruleActive, flattenVSubs(0, subtrees))
      end
  | flattenACase ((k as DATApcon (_,t), ruleset,[subtree]),path,active,defaults) =
      let val stillActive = intersect(union(ruleset, defaults), active)
	  val ruleActive = intersect(ruleset, active)
	  val newPath = DELTAPATH(k,path)
       in (k,ruleActive,flattenAndor(subtree,newPath,stillActive))
      end
  | flattenACase ((constant,ruleset,nil),path,active,defaults) =
      (constant, intersect(ruleset, active), nil)
  | flattenACase _ =
      bug "flattenACase - unexpected arg"

(* flattenCases: or_case list * path * ruleset * DA.consig -> decision *)
and flattenCases (cases, path, active,sign) =
  let fun calcDefaults (nil, active) = active
	| calcDefaults ((_,ruleset,_)::rest, active)  =
	    calcDefaults(rest, setDifference(active, ruleset))
      val defaults = calcDefaults(cases, active)
      fun doit nil = nil
	| doit (aCase::rest) =
	    ((flattenACase(aCase, path, active, defaults))
	     :: (doit(rest)))
   in case cases
       of (VLENpcon (_,t), _, _)::_ =>
	     CASEDEC{path=VLENPATH(t, path), sign=sign, cases=doit cases, defaults=defaults}
	| cases => CASEDEC{path=path, sign=sign, cases=doit cases, defaults=defaults}
  end

(* flattenAndors : (path * andor) list * ruleset -> (path * decision list) list *)
fun flattenAndors (nil, allrules) = nil
  | flattenAndors ((path, andor)::rest, allrules) =
      (path, flattenAndor(andor, path, allrules))
        :: (flattenAndors(rest, allrules))

end (* local open ... *)
end (* structure FlattenAndOrs *)
