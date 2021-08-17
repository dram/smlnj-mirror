(* tests/red/t1.sml -- redundant match bug *)

fun f ([], []) = 0
  | f ([], _) = 1
  | f (_, []) = 2
  | f (x :: xs, y :: ys) = 3;

