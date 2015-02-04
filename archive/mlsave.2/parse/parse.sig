(* parse.sig *)

signature PARSE = sig  (* open *)
  structure Basics : BASICS
  structure BareAbsyn : BAREABSYN
  exceptionx EOF : unit
  val reset : unit -> unit
  val exp : unit -> BareAbsyn.exp
  val pat : Basics.binder list ref * bool -> BareAbsyn.pat
  val ty : unit -> Basics.ty
  val ldec : unit -> BareAbsyn.dec
  val tdec : unit -> BareAbsyn.dec
  val str : unit -> (BareAbsyn.strexp * Basics.Structure)
end
