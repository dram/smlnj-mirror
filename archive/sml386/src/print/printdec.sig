(* Copyright 1989 by AT&T Bell Laboratories *)
(* printdec.sig *)

signature PRINTDEC =
  sig
    structure BareAbsn: BAREABSYN
    type object
    val printDec :  BareAbsn.dec -> (int -> object) -> unit
    val printBindingTbl: Basics.symtable -> unit
  end
