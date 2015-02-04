(* printval.sig *)

signature PRINTVAL = 
  sig
    structure Basics: BASICS
    type object
    val printVal: object * Basics.ty * int -> unit
  end
