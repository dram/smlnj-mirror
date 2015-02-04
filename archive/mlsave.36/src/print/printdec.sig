(* printdec.sig *)

signature PRINTDEC =
  sig
    structure BareAbsyn: BAREABSYN
    type object
    val printDec : (int -> object) -> BareAbsyn.dec -> unit
    val printBindingTbl: Basics.symtable -> unit
  end
