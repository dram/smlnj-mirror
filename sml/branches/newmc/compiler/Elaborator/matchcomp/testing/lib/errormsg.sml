(* matchcomp/lib/errormsg.sml *)

structure ErrorMsg =
struct

(* impossible : string -> 'a *)
fun impossible msg = (print msg; print "\n"; raise Fail msg)

end (* structure ErrorMsg *)

