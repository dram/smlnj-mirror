(* rules.sml *)
(* rule numbers and (ordered) rule number sets *)

structure Rules (* :> RULES *) =
struct

type ruleno = int

local structure S = IntBinarySet
in

type ruleset = set

fun next (rule: ruleno) : ruleno = rule + 1
		   
val empty = S.empty
val isEmpty = S.isEmpty
val add = S.add
val singleton = S.singleton
val union = S.union
val difference = S.difference
val listItems = S.listItems
		    
val unionList rulesets =
    List.foldl (fn (set, acc) => union(set,acc)) empty rulesets

end (* local *)
end (* structure Rules *)
