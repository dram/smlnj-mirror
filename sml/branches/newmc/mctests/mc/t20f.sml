(* t20f.sml -- OK *)

exception E;
fun f () = ();
() handle E  => ();
