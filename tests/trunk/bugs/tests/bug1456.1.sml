(* bug1456.1.sml *)

(* should be false, since sign(0) = 0 and sign(1) = 1 *)
Int.sameSign (0, 1);
Int32.sameSign (0, 1);
