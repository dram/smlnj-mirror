(* 18.sml *)
(* keyword: nonstrict type constructor *)

type 'a t = int

fun f (x: 'a) = (3: 'a t);

fun g y = g(f y);
