(* t20a.sml --  LContract: unexpected path in lpacc *)

structure D =
struct
  exception E
  fun l () = ()
end

val g = D.l ()
	handle D.E  => ()  (* D.E ! *)
