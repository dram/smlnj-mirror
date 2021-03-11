(* t4_3.sml  -- based on yacc.grm.sml *)
(* first 3 rules *)

structure Y =
struct

  datatype svalue
     = VOID
     | C1 of unit
     | C2 of unit
     | C3 of unit
     | C4 of unit
     | C5 of unit
     | C6 of unit
     | C7 of unit
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

  val result: spp * stack = ((VOID, 0, 0), nil)

fun actions (state: int, stack: stack): spp * stack =
case (state,stack)

of (0, (C14 x0a, _, right)
    :: _
    :: (C19 x0b, _, _)
    :: (C6 header1, left, _)
    :: rest) =>
    result

 | (1, (c20 x1a, left1, right)
    :: (C19 x1b, left2, _)
    :: rest) =>
    result

 | (2, rest) =>
    result
(*
 | (3, (C23 x3, _, right)
    :: (_, left, _)
    :: rest) =>
    result

 | (4, (C23 x4, _, right)
    :: (_, left, _)
    :: rest) =>
    result

 | (5, (C22 x5, _, right)
    :: (C2 prec1, left, _)
    :: rest) =>
    result

 | (6, (C5 x6, _, right)
    :: (_, left, _)
    :: rest) => 
    result

 | (7, (C22 x7, _, right)
    :: (_, left, _)
    :: rest) =>
    result

 | (8, (C22 x8, _, right)
    :: (_, left, _)
    :: rest) =>
    result

 | (9, (C22 x9, _, right)
    :: (_, left, _)
    :: rest) => 
    result

 | (10, (C10 x10, _, right)
     :: (_, left, _)
     :: rest) =>
     result

 | (11, (C12 x11, _, right)
     :: (_, left, _)
     :: rest) =>
     result

 | (12, (C22 x12, _, right)
     :: ( _ ,left, _)
     :: rest) =>
     result

 | (13, (C1 x13, _, right)
     :: (_, left, _)
     :: rest) =>
     result

 | (14, (C1 x14, _, right)
     :: (_, left, _)
     :: rest) =>
     result

 | (15, (C5 x15, _, right)
     :: (_, left, _)
     :: rest) =>
     result
       
 | (16, (C8 x16a, _, right)
     :: _
     :: (C1 x16b, _, _)
     :: (_, left, _)
     :: rest) =>
     result

 | (17, (_, left, right)
     :: rest) =>
     result

 | (18, (_, left, right)
     :: rest) =>
     result

 | (19, (_, left, right)
     :: rest) =>
     result

 | (20, (C8 x20, _, right)
     :: (_, left, _)
     :: rest) => 
     result

 | (21, (C1 x21a, _, right)
     :: (C5 x21b, _, _)
     :: (_, left, _)
     :: rest) =>
     result

 | (22, (C10 x22a, _, right)
     :: _
     :: (C9 x22b, left, _)
     :: rest) =>
     result

 | (23, (C9 x23, left, right)
     :: rest) =>
     result

 | (24, (C22 x24a, _, right)
     :: _
     :: (C22 x24b, left, _)
     :: rest) =>
     result

 | (25, (C12 x25a, _, right)
     :: _
     :: (C11 x25b, left, _)
     :: rest) =>
     result

 | (26, (C11 x26, left, right)
     :: rest) =>
     result

 | (27, (C5 x27a, _, right)
     :: _
     :: (C5 x27b, left, _)
     :: rest) =>
     result

 | (28, (C8 x28a, _, right)
     :: _
     :: (C5 x28b, _, _)
     :: _
     :: (C23 x28c, left, _)
     :: rest) =>
     result

 | (29, (C5 x29a, _, right)
     :: _
     :: (C23 x29b, left, _)
     :: rest) =>
     result

 | (30, (C8 x30a, _, right)
     :: _
     :: (C5 x30b, left, _)
     :: rest) =>
     result

 | (31, (C5 x31, left, right)
     :: rest) =>
     result

 | (32, (C16 x32a, _, right)
     :: _
     :: (C5 x32b, left, _)
     :: rest) =>
     result

 | (33, (C15 x33a, _, right)
     :: (C14 x33b, left, _)
     :: rest) =>
     result

 | (34, (C15 x34, left, right)
     :: rest) =>
     result

 | (35, (C22 x35a, _, right)
     :: (C5 x35b, left, _)
     :: rest) =>
     result

 | (36, rest) =>
     result

 | (37, (C1 x37a, _, right)
     :: (C13 x37b, _, _)
     :: (C22 x37c, left, _)
     :: rest) =>
     result

 | (38, (C1 x38a, _, right)
     :: (C13 x38b, _, _)
     :: (C22 x38c, _, _)
     :: _
     :: (C16 x38d, left, _)
     :: rest) =>
     result

 | (39, (C7 x39, left, right)
     :: rest) =>
     result

 | (40, (_, _, right)
     :: (C17 x40, _, _)
     :: (_, left, _)
     :: rest) =>
     result

 | (41, (_, _, right) 
     :: (_, left, _)
     :: rest) =>
     result

 | (42, (C1 x42, left, right)
     :: rest) =>
     result

 | (43, (C18 x43a, _, right)
     :: (C8 x43b, left, _)
     :: rest) =>
     result

 | (44, (C18 x44, left, right)
     :: rest) =>
     result

 | (45, (C8 x45a, _, right)
     :: _
     :: (C8 x45b, left, _)
     :: rest) => 
     result

 | (46, (C8 x46a, _, right)
     :: _
     :: (C8 x46b, left, _)
     :: rest) =>
     result

 | (47, (C8 x47a, _, right)
     :: _
     :: (C21 x47b, _, _)
     :: _
     :: (C17 x47c, left, _)
     :: rest) =>
     result

 | (48, (C8 x48a, _, right)
     :: _
     :: (C21 x48b, left, _)
     :: rest) =>
     result

 | (49, (C5 x49, left, right)
     :: rest) =>
     result

 | (50, (C18 x50a, _, right)
     :: (C4 x50b, left, _)
     :: rest) =>
     result

 | (51, (C5 x51, left, right)
     :: rest) =>
     result

 | (52, (C3 x52, left, right)
     :: rest) =>
     result

 | (53, (C5 x53, _, right)
     :: (_, left, _)
     :: rest) =>
     result

 | (54, rest) =>
     result
*)
 | _ => raise (Fail "rule 55")

end (* structure Y *)
