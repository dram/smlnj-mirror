(* parse.sig *)

signature PARSE = sig
  structure Basics : BASICS
  structure BareAbsyn : BAREABSYN
  structure Env : ENV
  exception Eof
  val reset : unit -> unit
  val tdec : unit -> BareAbsyn.dec
  val interdec : unit -> BareAbsyn.dec
end
