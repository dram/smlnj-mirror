(* tests/json/t0b.sml *)

fun dest nil = NONE
  | dest (x::xs) = SOME (x, xs);

fun f (xs: int list) =
    let val SOME(w, _) = dest xs
     in Int.toString w
    end;
