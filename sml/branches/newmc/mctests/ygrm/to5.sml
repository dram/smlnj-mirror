(* t4_5.sml  -- based on yacc.grm.sml *)
(* first 5 rules of t4.sml *)

structure Y =
struct

  datatype svalue
     = VOID
     | C1 of unit
     | C2 of unit
     | C3 of unit
     | C4 of unit
     | C5 of unit

  type spp = svalue * int * int
  type stack = spp list

fun actions (state: int, stack: stack) : int =
case (state,stack)

of (0, (C2 x0a, _, right)
    :: _
    :: (C3 x0b, _, _)
    :: (C1 x0c, left, _)
    :: rest) =>
    0

 | (1, (C4 x1a, left1, right)
    :: (C3 x1b, left2, _)
    :: rest) =>
    1

 | (2, rest) =>
    2

 | (3, (C5 x3, _, right)
    :: (_, left, _)
    :: rest) =>
    3

 | (4, (C5 x4, _, right)
    :: (_, left, _)
    :: rest) =>
    4

 | _ => 5

end (* structure Y *)

(* actions body uses :  C14/C2, C19/C3, C6/C1, C20/C4, C23/C5 *)
