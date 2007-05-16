(* 20.2.sml *)
(* overloading scheme variables can't generalize *)

datatype t = A;

let fun f(x,y) = x + y
 in f(A,5)
end;
