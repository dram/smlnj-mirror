(* t5c.sml  -- based on yacc.grm.sml *)
(* 1st 5 rules from t4.sml, 6 svalue constructors. rule 5 count = 268 *)

structure T5c =
struct

  datatype svalue
     = C0
     | C1 of unit (* C14 *)
     | C2 of unit (* C19 *)
     | C3 of unit (* C6 *)
     | C4 of unit (* C20 *)
     | C5 of unit (* C23 *)

  type spp = svalue * int * int
  type stack = spp list

fun actions (state: int, stack: stack): int =
case (state,stack)

of (0, (C1 x0a, _, right)
    :: _
    :: (C2 x0b, _, _)
    :: (C3 x0c, left, _)
    :: rest) =>
    0

 | (1, (C4 x1a, left1, right)
    :: (C2 x1b, left2, _)
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

end (* structure *)
