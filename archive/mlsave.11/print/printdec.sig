(* printdec.sig *)

signature PRINTDEC =
  sig
    structure BareAbsyn: BAREABSYN
    type object
    val printDepth : int ref
    val printDec : (int -> object) -> BareAbsyn.dec -> unit
  end
