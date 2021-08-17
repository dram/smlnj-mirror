(* t3.sml *)
(* from: FLINT/opt/fcontract.sml, line 955 *)
(* Error: Compiler bug: MatchComp: bindSVars: unbound pattern var: le *)

fun f (([(_,le)],NONE) | ([],SOME le)) = 0;
