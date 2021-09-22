(* implicit type variable binding *)
fun f1 (x) =
let
 fun h (z) = 
     let fun g () = z :: (nil : 'a list)
     in z :: x :: nil
	end
in h
end;

(* explicit type variable binding *)
fun f1 (x) =
let
 fun h (z) = 
     let fun 'a g () = z :: (nil : 'a list)
     in z :: x :: nil
	end
in h
end;
