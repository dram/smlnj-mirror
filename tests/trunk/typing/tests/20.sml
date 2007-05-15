(* Overloading Resolution and Typechecking 
   Overloading scheme type variables should 
   never be instantiated to univar. 
 *)

val _ = let fun f(x,y) = x + y
		in f(nil,5)
		end

datatype t = A
val _ = let fun f(x,y) = x + y
		in f(A,5)
		end
		