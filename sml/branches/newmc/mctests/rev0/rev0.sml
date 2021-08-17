(* rev0.sml -- from newmc/tests/vis/t28i.sml *)

let fun  h () = ()

    and lapp f []      = ()
      | lapp f (x::xs) = (f x; lapp f xs)

    fun g a = ()

in  lapp g []; lapp g [(0,0)]
end;

