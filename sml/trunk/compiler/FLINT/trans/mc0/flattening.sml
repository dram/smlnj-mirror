(* FLINT/trans/flattening.sml *)
(* "flattening" AndOr tree to decision list *)

structure Flattening =
struct

local
  open MCCommon

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

type bindenv = decision list (* containing only BINDEC decisions *)

(* bind:
   successive applications accumulate a list of BINDDECs, each with
   distinct path and a ruleset containing rules that have been "bound" at
   that path.  Produces a list of BINDDECs only.
   Such a list constitutes an environment mapping paths to rulesets.
   Let's call this a binding env (or bindenv).
*)
(* bind : path * ruleno * decision list -> bindenv *)
fun bind (path, rule, nil) = [BINDDEC(path, singleton rule)]
  | bind (path, rule, (binder as BINDDEC(path', rules))::rest) =
      if pathEq(path, path') then BINDDEC(path, add(rules,rule))::rest
      else binder :: bind(path, rule, rest)
  | bind _ = bug "bind - non BINDDEC in binding list"

(* bindingsToBindenv: bindings * active -> bindenv
   takes the "bindings" from an andor node, and a path (presumably the
   unique path to that node in the andor tree) and "binds" the rules
   in the bindings to that path in a bindenv, but only if the rule is
   "active", meaning it is in the active ruleset.
   Produces a bindenv (a list of BINDDECs).
*)
(* bindingsToBindenv : (ruleno * var) list * path * ruleset -> bindenv *)
fun bindingsToBindenv (nil, path, active) = nil
  | bindingsToBindenv (((rule, v)::rest), path, active) =
      if member(active, rule)
      then bind(path, rule, bindingsToBindenv(rest, path, active))
      else bindingsToBindenv(rest, path, active)

(* andorToBindenv
   Traverses the andor tree accumulating a bindenv binding all generated paths
   (starting with path as the root) to the associated "active" rules from
   the bindings fields in all the andor tree's nodes.
   Produces a bindenv (possibly _appended to_ a CASEDEC)
*)
(* andorToBindenv : andor * path * ruleset -> decision list (bindenv?!) *)
and andorToBindenv (AND {bindings, subtrees}, path, active) =
    let val _ = dbsay "flattenAndor:AND\n"
	  fun dotree (n, nil) = nil
	    | dotree (n, subtree::rest) =
	        flattenAndor (subtree, PIPATH(n,path), active) @ dotree(n + 1, rest)
	  val subtreesBindenv = dotree (0, subtrees)
	  val localBindenv = bindingsToBindenv(bindings, path, active)
       in localBindenv @ subtreesBindenv
      end
  | andorToBindenv (CASE {bindings, cases, sign}, path, active) =
      let val btests = bindingsToBindenv(bindings, path, active)
       in btests@((flattenCases(cases, path, active,sign))::nil)
      end
  | andorToBindenv (LEAF {bindings}, path, active) =
      bindingsToBindenv(bindings, path, active)

(* flattenACase : andorCase * path * ruleset * ruleset -> andorCase *)
and flattenACase ((VLENpcon(n, t), rules, subtrees), path, active, defaults) =
      let val stillActive = intersection(union(rules, defaults), active)
	  val newActive = intersection(rules, active)
	  fun flattenVSubs (n, nil) = nil
	    | flattenVSubs (n, subtree::rest) =
	      (andorToBindenv(subtree, VPIPATH(n,t,path), stillActive))
	      @ (flattenVSubs(n + 1, rest))
       in (Andor.intCon n, newActive, flattenVSubs(0, subtrees))
      end
  | flattenACase ((k as DATApcon (_,t), rules,[subtree]),path,active,defaults) =
      let val stillActive = intersection(union(rules, defaults), active)
	  val newActive = intersection(rules, active)
	  val newPath = CONPATH (k,path)
       in (k,newActive,andorToBindenv(subtree,newPath,stillActive))
      end
  | flattenACase ((constant,rules,nil),path,active,defaults) =
      (constant, intersection(rules, active), nil)
  | flattenACase _ =
      bug "flattenACase - unexpected arg"

(* calcDefaults: case list * ruleset -> ruleset
   calcDefaults (cases, active):
   take the starting set of "active" rules and successively subtract the rules
   component of each "case" in the cases arg. The result is a subset of the
   original "active" ruleset that is disjoint from the rules of each andorCase.
*)

(* flattenCases : case list * path * ruleset * A.consig -> decision   *)
and flattenCases (cases, path, active, sign) =
  let fun calcDefaults (nil, active) = active
	| calcDefaults ((_,rules,_)::rest, active)  =
	    calcDefaults(rest, setDifference(active, rules))
      val defaults = calcDefaults(cases, active)
      fun doit nil = nil
	| doit (aCase::rest) =
	    ((flattenACase(aCase, path, active, defaults))
	     :: (doit(rest)))
   in case cases
       of (VLENpcon (n,t), _, _)::_ =>
	     CASEDEC(VLENPATH(t, path), sign, doit cases, defaults)
	| cases => CASEDEC(path, sign, doit cases, defaults)
  end

(* flattenAndor : andor * rules -> path list * decision list *)
fun flattenAndor (andor, allrules) =
    andorToBindenv (andor, ROOTPATH, allrules)

end (* local *)
end (* structure Flattening *)
