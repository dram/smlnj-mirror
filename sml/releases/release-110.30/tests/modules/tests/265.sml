(* 265.sml *)

signature X = sig end;

functor Apply (functor F (A : X) : X) (B : X) : X =
	F(B);

functor FUN (functor G (C : X) : X) (D : X) : X =
	Apply(functor F = G)(D);


