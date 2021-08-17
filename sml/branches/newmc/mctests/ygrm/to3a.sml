(* to3a.sml  -- based on yacc.grm.sml *)
(* first 3 rules *)

structure Y =
struct

  datatype s
     = VOID
     | C1 of unit
     | C2 of unit
     | C3 of unit
     | C4 of unit

fun actions (state: int, stack: (s * int * int) list) =
case (state,stack)

of (0, (C1 x0a, _, z0)
    :: (C2 x0b, _, _)
    :: (C3 x0c, y0, _)
    :: r0) =>
    0

 | (1, (C4 x1a, y1a, z1)
    :: (C2 x1b, y1b, _)
    :: r1) =>
    1

 | (2, r2) =>
    2

 | _ => 3

end (* structure Y *)
