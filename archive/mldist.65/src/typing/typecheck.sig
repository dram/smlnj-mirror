(* Copyright 1989 by AT&T Bell Laboratories *)
signature TYPECHECK = sig

  structure BareAbsyn : BAREABSYN

  val decType : BareAbsyn.dec * bool
       * (BareAbsyn.linenum * BareAbsyn.linenum->ErrorMsg.severity->string->unit)
       * (BareAbsyn.linenum * BareAbsyn.linenum)
       -> unit

end  (* signature TYPECHECK *)
