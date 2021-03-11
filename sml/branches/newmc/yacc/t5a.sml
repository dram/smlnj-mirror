(* t5a.sml  -- based on yacc.grm.sml *)
(* first 1 (+1) rule, minimized set of 4 constructors *)

structure T5a =
struct

  datatype svalue
     = C0
     | C1 of unit
     | C2 of unit
     | C3 of unit

  type spp = svalue * int * int
  type stack = spp list

fun actions (state: int, stack: stack): int =
    case (state,stack)
      of (0, (C1 x0a, _, right) ::
             _ ::
	     (C2 x0b, _, _) ::
	     (C3 x0c, left, _) ::
	     rest) =>
	 0

       | _ => 1

end (* structure *)
