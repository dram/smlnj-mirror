(* rules.sml *)
(* rule numbers and (ordered) rule number sets *)

structure Rules (* :> RULES *) =
struct

local
  structure S = IntBinarySet
in

type ruleno = int
type ruleset = IntBinarySet.set

fun next (rule: ruleno) : ruleno = rule + 1
		   
val empty = S.empty
val isEmpty = S.isEmpty
val add = S.add
val singleton = S.singleton
val union = S.union
val difference = S.difference
val listItems = S.listItems
		    
fun unionList rulesets =
    List.foldl S.union S.empty rulesets

end (* local *)
end (* structure Rules *)
