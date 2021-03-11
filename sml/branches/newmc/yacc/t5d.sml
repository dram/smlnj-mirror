(* t5d.sml  -- based on yacc.grm.sml *)
(* 1st 10 (+1) rules of t4.sml, 24 svalue constructors. rule 11 count = 810 *)

structure Y =
struct

  datatype svalue
     = C0
     | C1 of unit  (* C14 *)
     | C2 of unit  (* C19 *)
     | C3 of unit  (* C6 *)
     | C4 of unit  (* C20 *)
     | C5 of unit  (* C23 *)
     | C6 of unit  (* C22 *)
     | C7 of unit  (* C5 *)
     | C8 of unit
     | C9 of unit
     | C10 of unit
     | C11 of unit
     | C12 of unit
     | C13 of unit
     | C14 of unit
     | C15 of unit
     | C16 of unit
     | C17 of unit
     | C18 of unit
     | C19 of unit
     | c20 of unit
     | C21 of unit
     | C22 of unit
     | C23 of unit

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

 | (4, (C7 x4, _, right)
    :: (_, left, _)
    :: rest) =>
    4

 | (5, (C6 x5a, _, right)
    :: (C2 x5b, left, _)
    :: rest) =>
    5

 | (6, (C7 x6, _, right)
    :: (_, left, _)
    :: rest) => 
    6

 | (7, (C6 x7, _, right)
    :: (_, left, _)
    :: rest) =>
    7

 | (8, (C6 x8, _, right)
    :: (_, left, _)
    :: rest) =>
    8

 | (9, (C6 x9, _, right)
    :: (_, left, _)
    :: rest) => 
    9

 | _ => 10

end (* structure Y *)
