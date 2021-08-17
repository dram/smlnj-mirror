(* to3e.sml *)

structure Y =
struct

  datatype s
     = C1 of unit
     | C2 of unit
     | C3 of unit

fun f (stack: (s * int) list) =
case (stack)

of ((C1 x0a, _)
    :: (C2 x0b, _)
    :: r0) =>
    0

 | ((C3 x1a, y1a)
    :: (C2 x1b, y1b)
    :: r1) =>
    1

 | _ => 2

end (* structure Y *)
