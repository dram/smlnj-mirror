(* to4-sml -- 1st 4 rules of stipped down yacc.grm.sml *)

structure Y =
struct

  datatype v
     = C1 of unit
     | C2 of unit
     | C3 of unit
     | C4 of unit
     | C5 of unit

  type s = v * int * int

fun actions (state: int, stack: s list) : int =
case (state,stack)

of (0, (C2 a, _, b)
    :: _
    :: (C3 c, _, _)
    :: (C1 d, e, _)
    :: f) =>
    0   (* rhs0(a,b,c,d,e,f) *)

 | (1, (C4 g, h, i)
    :: (C3 j, k, _)
    :: l) =>
    1   (* rhs1(g,h,i,j,k,l) *)

 | (2, m) =>
    2   (* rhs2(m) *)

 | (3, (C5 n, _, p)
    :: (_, q, _)
    :: r) =>
    3   (* rhs3(n,p,q,r) *)

 | _ => 4   (* rhs4() *)

end (* structure Y *)
