(* 23.sml *)
(* testing that overloading scheme type variables don't get instantiated
 * to type terms with univariables *)

type ('a, 'b) fst = 'a;
fun g(x :'a) = (x : ('a,'b) fst);

let fun f(x,y) = g(x) + g(y)
 in f(2,3); f(true,false)
end;
