(* t5b.sml  -- based on yacc.grm.sml *)
(* 1st 1 (+1) rule, minimized set of 3 constructors, eliminated 1 list element in rule 0 *)

structure T4_1b =
struct

  datatype svalue
     = C0
     | C1 of unit
     | C2 of unit

  type spp = svalue * int * int
  type stack = spp list

fun actions (state: int, stack: stack): int =
    case (state,stack)
      of (0, (C1 x0a, _, right) ::
	     _ ::
	     (C2 x0b, _, _) ::
	     rest) =>
	 0
       | _ => 1

end (* structure T4_1b *)

(* 
ruleCounts
 0 = 1
 1 = 6
*)
