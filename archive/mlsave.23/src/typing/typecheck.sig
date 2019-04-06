signature TYPECHECK = sig

  structure BareAbsyn : BAREABSYN

  val reset : unit -> unit
  val decType : BareAbsyn.dec -> unit

end  (* signature TYPECHECK *)
