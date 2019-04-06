(* Copyright 1989 by AT&T Bell Laboratories *)
(* functor.sig *)

signature FUNCTOR =
sig
(*   structure Env *)
  val abstractBody : Basics.Structure * Basics.Structure
		     * Stampset.stampsets * int * ErrorMsg.complainer 
			-> Basics.Structure

  val applyFunctor : Basics.env * Basics.Functor * Basics.Structure 
		     * Symbol.symbol list 
		     * Stampset.stampsets * ErrorMsg.complainer
		     -> Basics.Structure * Basics.thinning
end
