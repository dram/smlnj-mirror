(* tests/vector/t4.sml *)

fun f ((#[x,y],b)::vs) = if b then x+y else 0
  | f _ = 1;
