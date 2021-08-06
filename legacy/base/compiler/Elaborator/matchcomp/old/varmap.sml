(* varmap.sml *)

structure RuleVarMap =
struct

local
    structure V = VarCon
    structure SV = SVar
    structure M = IntBinaryMap
    open MCTypes
in

(* The rulemap maps rule numbers to an association list that maps each var in that rule's
 * pattern to the pattern point (path) at which the variable occurs and it svar. *)

type rulemap = (V.var * path * SV.svar) list IntBinaryMap.map

val emptyRuleMap = M.empty

(* lookupmap : rulemap * ruleno -> (V.var * SV.svar) list *)
fun lookupmap (map, rule) =
    M.lookup(map, rule)
    handle LibBase.NotFound => nil

(* insert : rulemap * ruleno * (V.var * SV.svar) -> rulemap *)
fun insert (map, ruleno, (var, path, svar)) =
    (MCUtil.printMappedVars (var, svar, "I: ");
     M.insert (map, ruleno, (var, path, svar) :: lookupmap (map, ruleno)))

(* makemap : andor * rulemap -> rulemap *)
fun makemap (VARS{svar,path,vars,asvars,...}, map) =
    let val map1 =
	    foldl (fn ((var, rule),map) => insert(map,rule,(var,path,svar)))
		  map vars
	val map2 =
	    foldl (fn ((var, rule),map) => insert(map,rule,(var,path,svar)))
		  map1 asvars
     in map2
    end
  | makemap (AND{svar,path,vars,asvars,children,...}, map) =
    let val map1 =
	    foldl (fn ((var, rule),map) => insert(map,rule,(var,path,svar)))
		  map vars
	val map2 =
	    foldl (fn ((var, rule),map) => insert(map,rule,(var,path,svar)))
		  map1 asvars
	val map3 =
	    foldl makemap map2 children
     in map3
    end
  | makemap (OR{svar,path,vars,asvars,variants,...}, map) =
    let val map1 =
	    foldl (fn ((var, rule),map) => insert(map,rule,(var,path,svar)))
		  map vars
	val map2 =
	    foldl (fn ((var, rule),map) => insert(map,rule,(var,path,svar)))
		  map1 asvars
	val map3 =
	    foldl (fn ((k,andor),map) => (makemap(andor,map))) map2 variants
     in map3
    end
  | makemap (SINGLE{svar,path,vars,asvars,arg,...}, map) =
    let val map1 =
	    foldl (fn ((var, rule),map) => insert(map,rule,(var,path,svar)))
		  map vars
	val map2 =
	    foldl (fn ((var, rule),map) => insert(map,rule,(var,path,svar)))
		  map1 asvars
     in makemap(arg,map2)
    end
  | makemap (_, map) = map

(* makeRuleMap : andor -> rulemap *)
fun makeRuleMap andor =
    (print ">>>makeRuleMap\n";
     makemap(andor, emptyRuleMap) before print "<<<makeRuleMap\n")

val lookup : rulemap * ruleno -> (V.var * path * SV.svar) list =  lookupmap

end (* local *)
end (* structure RuleVarMap *)
