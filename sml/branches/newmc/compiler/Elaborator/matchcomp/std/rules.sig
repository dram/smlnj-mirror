(* rules.sig *)

signature RULES =
sig

    type ruleno = int
    type ruleset

    val increment : ruleno -> ruleno

    val empty : ruleset	     
    val isEmpty : ruleset -> bool
    val member : ruleset * ruleno -> bool
    val fromList : ruleno list -> ruleset
    val add : ruleset * ruleno -> ruleset
    val addList : ruleset * ruleno list -> ruleset
    val singleton : ruleno -> ruleset
    val union : ruleset * ruleset -> ruleset
    val unionList : ruleset list -> ruleset
    val intersection : ruleset * ruleset -> ruleset
    val difference : ruleset * ruleset -> ruleset
    val minItem : ruleset -> ruleno
    val listItems : ruleset -> ruleno list
    val numItems : ruleset -> int

end (* signature RULES *)
