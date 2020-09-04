(* tst-ex09.sml *)

fun sumVec (v : int vector) = let
      fun lp (i, s) = if (i < Vector.length v)
	    then lp (i+1, s + Vector.sub(v, i))
	    else s
      in
	lp (0, 0)
      end;

