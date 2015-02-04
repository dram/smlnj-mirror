(* printdec.sig *)

signature PRINTDEC =
  sig
    structure Object: sig type Object end
    structure BareAbsyn: BAREABSYN
    val printDepth : int ref
    val printDec : (int -> Object.Object) -> BareAbsyn.dec -> unit
  end
