(* es2.sml *)
(* no defs arg, no error *)

exception E;
	      
datatype se = A of se * int list | B;  (* ! B *)

fun f se =
    let fun loop (A (se,xs)) =
	    let fun loop1 nil = loop (se)  (* ! loop *)
		  | loop1 (x::rest) =
		     (loop1 rest
		      handle E => loop1 rest)
	     in loop1 xs
	    end
	  | loop (se) = (se)
     in loop (se)
    end;


