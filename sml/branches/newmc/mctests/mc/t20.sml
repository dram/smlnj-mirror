(* t20.sml --  LContract: unexpected path in lpacc *)

structure D =  (* D ! *)
struct
  exception E
  fun l () = ()
end

fun g () = D.l ()             (* D.l ! *)
	   handle D.E  => ()  (* handle !; D.E ! *)
and h () = h ()   (* and !  - not fun *)
