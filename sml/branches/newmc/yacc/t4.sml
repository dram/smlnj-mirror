(* t4.sml  -- based on yacc.grm.sml *)

structure T4 =
struct

  datatype svalue
     = C0
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

fun actions (state: int, stack: stack): int =
case (state,stack)

of (0, (C14 x0a, _, right)
    :: _
    :: (C19 x0b, _, _)
    :: (C6 x0c, left, _)
    :: rest) =>
    0

 | (1, (C20 x1a, left1, right)
    :: (C19 x1b, left2, _)
    :: rest) =>
    1

 | (2, rest) =>
    2

 | (3, (C23 x3, _, right)
    :: (_, left, _)
    :: rest) =>
    3

 | (4, (C23 x4, _, right)
    :: (_, left, _)
    :: rest) =>
    4

 | (5, (C22 x5a, _, right)
    :: (C2 x5b, left, _)
    :: rest) =>
    5

 | (6, (C5 x6, _, right)
    :: (_, left, _)
    :: rest) => 
    6

 | (7, (C22 x7, _, right)
    :: (_, left, _)
    :: rest) =>
    7

 | (8, (C22 x8, _, right)
    :: (_, left, _)
    :: rest) =>
    8

 | (9, (C22 x9, _, right)
    :: (_, left, _)
    :: rest) => 
    9

 | (10, (C10 x10, _, right)
     :: (_, left, _)
     :: rest) =>
     10

 | (11, (C12 x11, _, right)
     :: (_, left, _)
     :: rest) =>
     11

 | (12, (C22 x12, _, right)
     :: ( _ ,left, _)
     :: rest) =>
     12

 | (13, (C1 x13, _, right)
     :: (_, left, _)
     :: rest) =>
     13

 | (14, (C1 x14, _, right)
     :: (_, left, _)
     :: rest) =>
     14

 | (15, (C5 x15, _, right)
     :: (_, left, _)
     :: rest) =>
     15
       
 | (16, (C8 x16a, _, right)
     :: _
     :: (C1 x16b, _, _)
     :: (_, left, _)
     :: rest) =>
     16

 | (17, (_, left, right)
     :: rest) =>
     17

 | (18, (_, left, right)
     :: rest) =>
     18

 | (19, (_, left, right)
     :: rest) =>
     19

 | (20, (C8 x20, _, right)
     :: (_, left, _)
     :: rest) => 
     20

 | (21, (C1 x21a, _, right)
     :: (C5 x21b, _, _)
     :: (_, left, _)
     :: rest) =>
     21

 | (22, (C10 x22a, _, right)
     :: _
     :: (C9 x22b, left, _)
     :: rest) =>
     22

 | (23, (C9 x23, left, right)
     :: rest) =>
     23

 | (24, (C22 x24a, _, right)
     :: _
     :: (C22 x24b, left, _)
     :: rest) =>
     24

 | (25, (C12 x25a, _, right)
     :: _
     :: (C11 x25b, left, _)
     :: rest) =>
     25

 | (26, (C11 x26, left, right)
     :: rest) =>
     26

 | (27, (C5 x27a, _, right)
     :: _
     :: (C5 x27b, left, _)
     :: rest) =>
     27

 | (28, (C8 x28a, _, right)
     :: _
     :: (C5 x28b, _, _)
     :: _
     :: (C23 x28c, left, _)
     :: rest) =>
     28

 | (29, (C5 x29a, _, right)
     :: _
     :: (C23 x29b, left, _)
     :: rest) =>
     29

 | (30, (C8 x30a, _, right)
     :: _
     :: (C5 x30b, left, _)
     :: rest) =>
     30

 | (31, (C5 x31, left, right)
     :: rest) =>
     31

 | (32, (C16 x32a, _, right)
     :: _
     :: (C5 x32b, left, _)
     :: rest) =>
     32

 | (33, (C15 x33a, _, right)
     :: (C14 x33b, left, _)
     :: rest) =>
     33

 | (34, (C15 x34, left, right)
     :: rest) =>
     34

 | (35, (C22 x35a, _, right)
     :: (C5 x35b, left, _)
     :: rest) =>
     35

 | (36, rest) =>
     36

 | (37, (C1 x37a, _, right)
     :: (C13 x37b, _, _)
     :: (C22 x37c, left, _)
     :: rest) =>
     37

 | (38, (C1 x38a, _, right)
     :: (C13 x38b, _, _)
     :: (C22 x38c, _, _)
     :: _
     :: (C16 x38d, left, _)
     :: rest) =>
     38

 | (39, (C7 x39, left, right)
     :: rest) =>
     39

 | (40, (_, _, right)
     :: (C17 x40, _, _)
     :: (_, left, _)
     :: rest) =>
     40

 | (41, (_, _, right) 
     :: (_, left, _)
     :: rest) =>
     41

 | (42, (C1 x42, left, right)
     :: rest) =>
     42

 | (43, (C18 x43a, _, right)
     :: (C8 x43b, left, _)
     :: rest) =>
     43

 | (44, (C18 x44, left, right)
     :: rest) =>
     44

 | (45, (C8 x45a, _, right)
     :: _
     :: (C8 x45b, left, _)
     :: rest) => 
     45

 | (46, (C8 x46a, _, right)
     :: _
     :: (C8 x46b, left, _)
     :: rest) =>
     46

 | (47, (C8 x47a, _, right)
     :: _
     :: (C21 x47b, _, _)
     :: _
     :: (C17 x47c, left, _)
     :: rest) =>
     47

 | (48, (C8 x48a, _, right)
     :: _
     :: (C21 x48b, left, _)
     :: rest) =>
     48

 | (49, (C5 x49, left, right)
     :: rest) =>
     49

 | (50, (C18 x50a, _, right)
     :: (C4 x50b, left, _)
     :: rest) =>
     50

 | (51, (C5 x51, left, right)
     :: rest) =>
     51

 | (52, (C3 x52, left, right)
     :: rest) =>
     52

 | (53, (C5 x53, _, right)
     :: (_, left, _)
     :: rest) =>
     53

 | (54, rest) =>
     54

 | _ => 55

end (* structure *)
