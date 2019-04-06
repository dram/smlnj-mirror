(* Copyright 1989 by AT&T Bell Laboratories *)
(* printdec.sig *)

signature PRINTDEC =
  sig
    type object
    val printDec :  Modules.env -> BareAbsyn.dec -> (int -> object) -> unit
    val printBindingTbl: Modules.env -> unit
  end
