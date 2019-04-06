(* Copyright 1989 by AT&T Bell Laboratories *)
(* functor.sig *)

signature FUNCTOR =
sig

  val abstractBody : Basics.Structure * Basics.Structure
		     * Stampset.stampsets * int * ErrorMsg.complainer 
			-> Basics.Structure

  val applyFunctor : Basics.Functor * Basics.Structure * Basics.Symbol.symbol list 
		     * Stampset.stampsets * ErrorMsg.complainer
		     -> Basics.Structure * Basics.thinning
end
