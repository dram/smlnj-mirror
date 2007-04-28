(* bug1293 *)

CharVector.mapi (fn (i, c) => (print (Int.toString i ^ "\n"); c))
(String.extract ("ABCDEFG", 2, SOME 3));

