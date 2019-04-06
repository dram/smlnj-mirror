(* Copyright 1989 by AT&T Bell Laboratories *)
signature TYPECHECK = sig

  structure BareAbsn : BAREABSYN

  val decType : BareAbsn.dec * bool
       * (BareAbsn.linenum * BareAbsn.linenum->ErrorMsg.severity->string->unit)
       * (BareAbsn.linenum * BareAbsn.linenum)
       -> unit

end  (* signature TYPECHECK *)
