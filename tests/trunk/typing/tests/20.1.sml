(* 20.1.sml *)
(* overloading scheme variables can't generalize *)

let fun f(x,y) = x + y
 in f(nil,nil)
end;
