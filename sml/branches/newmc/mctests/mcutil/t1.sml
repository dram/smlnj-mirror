(* mcutil/t1.sml *)

datatype t = A | B

datatype s  = C | D

val x = (NONE, nil)

val a =		
    case x
      of (SOME C, nil) => 0
       | (SOME D, A::r) => 1
       | (NONE, _) => 2

