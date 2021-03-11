(* t5l.sml  -- based on yacc.grm.sml *)
(* 1st 2 (+1) rules of t4.sml, 5 svalue constructors. ruleCounts: [1,1,91] *)

structure T5l =
struct

  datatype svalue
     = C0
     | C1 of unit  (* C14 *)
     | C2 of unit  (* C19 *)
     | C3 of unit  (* C6 *)
     | C4 of unit  (* C20 *)

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

 | (1, (C4 x1a, left1, right) ::
       (C2 x1b, left2, _) ::
       rest) =>
       1

 | _ => 2

end (* structure *)
