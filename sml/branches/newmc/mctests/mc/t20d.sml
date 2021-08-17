(* t20c.sml -- LContract: unexpected path in lpacc *)

let exception E
    fun f () = ()
in f () handle E  => ()
end
