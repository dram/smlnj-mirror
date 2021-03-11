(* t6.sml  -- based on yacc.grm.sml *)
(* 1st 2 (+1) rules of t4.sml, 4 sv constructors. ruleCounts: [1,5,10] *)
(* dropped C0 constant constructor, which does not occur *)

structure T6 =
struct

  datatype sv
     = C1 of unit  (* C14 *)
     | C2 of unit  (* C19 *)
     | C3 of unit  (* C6 *)
     | C4 of unit  (* C20 *)

  type spp = sv * int * int
  type stack = spp list

fun actions (state: int, stack: stack): int =
case (state,stack)

of (0, (C1 x0a, _, r0) ::
       _ ::
       (C2 x0b, _, _) ::
       (C3 x0c, l0, _) ::
       rest) =>
       0

 | (1, (C4 x1a, l1a, r1) ::
       (C2 x1b, l1b, _) ::
       rest) =>
       1

 | _ => 2

end (* structure *)
