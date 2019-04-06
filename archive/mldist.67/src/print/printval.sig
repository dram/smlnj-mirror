(* Copyright 1989 by AT&T Bell Laboratories *)
(* printval.sig *)

signature PRINTVAL = 
  sig
(*     structure Env:ENV *)
    type object
    val printVal: Basics.env -> object * Basics.ty * int -> unit
  end
