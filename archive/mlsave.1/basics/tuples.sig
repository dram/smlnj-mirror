(* tuples.sig *)

signature TUPLES = sig

structure Basics : BASICS

val hashLabels : Basics.label list -> int

val numlabel : int -> Basics.label

val mkTUPLEtyc : int -> Basics.tycon

val isTUPLEtyc : Basics.tycon -> bool

end
