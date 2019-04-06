(* tuples.sig *)

signature TUPLES = sig
  structure Basics : BASICS
  val numlabel : int -> Basics.label
  val mkTUPLEtyc : int -> Basics.tycon
  val isTUPLEtyc : Basics.tycon -> bool
  val mkRECORDtyc : Basics.label list -> Basics.tycon
end
