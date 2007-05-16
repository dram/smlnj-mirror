(* 20.sml *)
(* Overloading Resolution and Typechecking 
   Overloading scheme type variables should 
   never be instantiated to univar. 
 *)

let fun f(x,y) = x + y
 in f(nil,5)
end;
