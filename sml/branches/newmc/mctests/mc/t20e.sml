(* t20e.sml -- LContract: unexpected path in lpacc *)

exception E;
fun l () = ();
l handle E  => l;  (* l !; E ! *)
