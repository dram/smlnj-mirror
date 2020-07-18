(* varmap.sml *)

structure RuleVarMap =
struct

local
    structure V = Var
    structure SV = SVar
    structure M = IntBinaryMap
    open MCTypes
in
(* rulemap maps rule numbers to a list that associates an svar with each (source/VALvar)
 * var appearing in that rule's pattern. *)
type rulemap = (V.var * SV.svar) list IntBinaryMap.map

val emptyRuleMap = M.empty

fun lookuplist (map, rule) =
    M.lookup(map, rule)
    handle LibBase.NotFound => nil

(* makemap : andor * rulemap -> rulemap *)
fun makemap (VARS{svar,vars,asvars,...}, map) =
    let val map1 = foldl (fn ((var, rule),map) => M.insert(map,rule,(var,svar)::lookuplist(map,rule)))
			 map vars
	val map2 = foldl (fn ((var, rule),map) => M.insert(map,rule,(var,svar)::lookuplist(map,rule)))
			 map1 asvars
    in map2
    end
  | makemap (AND{svar,vars,asvars,children,...}, map) =
    let val map1 = foldl (fn ((var, rule),map) => M.insert(map,rule,(var,svar)::lookuplist(map,rule)))
			 map vars
	val map2 = foldl (fn ((var, rule),map) => M.insert(map,rule,(var,svar)::lookuplist(map,rule)))
			 map1 asvars
	val map3 = foldl makemap map2 children
    in map3
    end
  | makemap (OR{svar,vars,asvars,variants,...}, map) =
    let val map1 = foldl (fn ((var, rule),map) => M.insert(map,rule,(var,svar)::lookuplist(map,rule)))
			 map vars
	val map2 = foldl (fn ((var, rule),map) => M.insert(map,rule,(var,svar)::lookuplist(map,rule)))
			 map1 asvars
	val map3 = foldl (fn ((k,andor),map) => (makemap(andor,map))) map2 variants
    in map3
    end
  | makemap (SINGLE{svar,vars,asvars,arg,...}, map) =
    let val map1 = foldl (fn ((var, rule),map) => M.insert(map,rule,(var,svar)::lookuplist(map,rule)))
			 map vars
	val map2 = foldl (fn ((var, rule),map) => M.insert(map,rule,(var,svar)::lookuplist(map,rule)))
			 map1 asvars
    in makemap(arg,map2)
    end
  | makemap (_, map) = map

(* makeRuleMap : andor -> rulemap *)
fun makeRuleMap andor =
    makemap(andor, emptyRuleMap)

val lookup : rulemap * ruleno -> (V.var * SV.svar) list =  lookuplist

end (* local *)
end (* structure RuleVarMap *)
