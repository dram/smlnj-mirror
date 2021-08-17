(* e1.sml *)

fun f x =
    case x
     of (_,_) :: (_,_) :: (_,_) :: _ => 0
      | nil => 1;
