(* es4.sml *)

datatype se = A of se * int list | B;  (* ! B *)

fun f se =
    let fun loop (A (se,xs), defs) =  (* ! defs *)
	      let fun loop1 nil = loop (se, nil)  (* ! loop *)
		    | loop1 (x::rest) = loop1 rest
	       in loop1 xs
	      end
	  | loop (se, defs) = (se, defs)  (* when se = B *)
     in loop (se, nil)
    end;

(*
fun f (se: se) =
    let fun loop ['a] (A (se,xs), defs: 'a list) =  <== binding occ. of 'a
	      let fun loop1 (nil: 'b list) = loop (se, nil: 'a list)  (* : se * 'a list *)
		    | loop1 (x::rest) = loop1 rest     (* loop1 : ['b] 'b list -> se * 'a list *)
	       in loop1 [int] xs : se * 'a list
	      end
	  | loop (se, defs) = (se, defs)  (* when se = B *)
         (* loop : ['a] (se * 'a list) -> (se * 'a list) *)
     in loop (se, nil)
    end;
*)

