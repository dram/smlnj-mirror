(* t5m.sml  -- based on yacc.grm.sml *)
(* 1st 1 (+1) rules of t4.sml, 4 svalue constructors. ruleCounts: [1,8] *)

structure T5m =
struct

  datatype svalue
    = C1 of unit  (* C14 *)
    | C2 of unit  (* C19 *)
    | C3 of unit  (* C6 *)

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

 | _ => 2

end (* structure *)
