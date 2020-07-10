(* rules.sml *)
(* rule numbers and (ordered) rule number sets *)

structure Rules :> RULES =
struct

local
  structure S = IntBinarySet
in

type ruleno = int
type ruleset = IntBinarySet.set

fun increment (rule: ruleno) : ruleno = rule + 1
		   
val empty = S.empty
val isEmpty = S.isEmpty
val member = S.member
val fromList = S.fromList
val add = S.add
val addList = S.addList
val singleton = S.singleton
val union = S.union
val intersection = S.intersection
val difference = S.difference

val minItem = S.minItem
val listItems = S.listItems
val numItems = S.numItems
		   
fun unionList rulesets =
    List.foldl S.union S.empty rulesets

end (* local *)
end (* structure Rules *)
