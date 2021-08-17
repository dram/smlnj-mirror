(* t20b.sml -- LContract: unexpected path in lpacc *)

exception E;
fun f () = ()
val x = f () handle E  => ();  (* f() !; E ! *)
