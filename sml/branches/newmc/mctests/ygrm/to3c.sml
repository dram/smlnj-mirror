(* to3c.sml *)

structure Y =
struct

  datatype s
     = C1 of unit
     | C2 of unit
     | C4 of unit

fun actions (state: int, stack: (s * int) list) =
case (state,stack)

of (0, (C1 x0a, _)
    :: (C2 x0b, _)
    :: r0) =>
    0

 | (1, (C4 x1a, y1a)
    :: (C2 x1b, y1b)
    :: r1) =>
    1

 | (2, r2) =>
    2

 | _ => 3

end (* structure Y *)
