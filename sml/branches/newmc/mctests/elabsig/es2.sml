(* es2.sml *)

exception E;
	      
datatype se = A of se * int list | B;  (* ! B *)

fun f se =
    let fun loop (A (se,xs), defs) =  (* ! defs *)
	    let fun loop1 nil = loop (se, nil)  (* ! loop *)
		  | loop1 (x::rest) =
		     (loop1 rest
		      handle E => loop1 rest)
	     in loop1 xs
	    end
	  | loop (se, defs) = (se, defs)
     in loop (se, nil)
    end;

(*
cause of bug:
return type of loop is left as an uninstantiated generic meta tyvar
(depth = infinity) while loop1 is being type checked.  Return type of loop is not
"instantiated" until after its body (including loop1) is type checked.
This means that the generalization of the type of loop1 is able to
generalize its return type (i.e. the uninstantiated type of "loop (se, nil)"
as well as it argument type ('a list).

Need to _limit_ (with lamdepth) the type of "defs" in the return type
of loop before typing the call "loop (se, nil)" in the first rule for
loop1.
*)
