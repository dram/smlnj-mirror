signature TYPECHECK = sig

  structure BareAbsyn : BAREABSYN

  val reset : unit -> unit
  val patType : BareAbsyn.pat * BareAbsyn.Basics.tvkind -> BareAbsyn.Basics.ty
  val expType : BareAbsyn.exp -> BareAbsyn.Basics.ty
  val decType : BareAbsyn.dec -> unit

end  (* signature TYPECHECK *)
