(* t20c.sml -- LContract: unexpected path in lpacc *)

exception E
fun f () = ()
val _ = f () handle E  => ()  (* l !; E ! *)
