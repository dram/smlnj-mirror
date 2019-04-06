(* Copyright 1990 by AT&T Bell Laboratories *)
signature EQTYPES =
sig

  val eqAnalyze : Basics.Structure * ErrorMsg.complainer -> unit

  val defineEqTycon : (Basics.tycon -> Basics.tycon) -> Basics.tycon -> unit

  val checkEqTySig : Basics.ty * Basics.polysign -> bool

  val isEqTycon : Basics.tycon -> bool

  val isEqType : Basics.ty -> bool
end

