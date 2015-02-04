(* printval.sig *)

signature PRINTVAL = 
  sig
    structure Object : sig type Object end
    structure Basics: BASICS
    val printVal: Object.Object * Basics.ty * int -> unit
  end
