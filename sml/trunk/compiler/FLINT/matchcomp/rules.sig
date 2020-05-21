(* rules.sig *)

signature RULES =
sig

    type ruleno = int
    type ruleset

    val next : ruleno -> ruleno

    val empty : ruleset	     
    val isEmpty : ruleset -> bool
    val add : ruleset * ruleno -> ruleset
    val singleton : ruleno -> ruleset
    val union : ruleset * ruleset -> ruleset
    val difference : ruleset * ruleset -> ruleset
    val listItems : ruleset -> ruleno list
					 
    val unionList : ruleset list -> ruleset

end (* signature RULES *)
