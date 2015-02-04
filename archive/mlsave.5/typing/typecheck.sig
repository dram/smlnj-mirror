signature TYPECHECK = sig

  structure BareAbsyn : BAREABSYN

  val reset : unit -> unit
  val generalizePat : BareAbsyn.pat -> unit
  val patType : BareAbsyn.pat * BareAbsyn.Basics.tvstatus -> BareAbsyn.Basics.ty
  val expType : BareAbsyn.exp -> BareAbsyn.Basics.ty
  val decType : BareAbsyn.dec -> unit
  val expTypeTop : BareAbsyn.exp -> BareAbsyn.Basics.ty

end  (* signature TYPECHECK *)
