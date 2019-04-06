(* Copyright 1989 by AT&T Bell Laboratories *)
(* printdec.sig *)

signature PRINTDEC =
  sig
    type object
    val printDec :  Basics.env -> BareAbsyn.dec -> (int -> object) -> unit
    val printBindingTbl: Basics.env -> unit
  end
