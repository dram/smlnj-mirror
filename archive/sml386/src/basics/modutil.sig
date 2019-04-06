(* Copyright 1989 by AT&T Bell Laboratories *)
signature MODUTIL =
sig

  val mapSubstrs : (Basics.Structure -> Basics.Structure) * Basics.Structure array
		   -> Basics.Structure array

  val setParent : Basics.Structure -> Basics.Structure -> unit

  val resetParent : Basics.Structure -> Basics.Structure -> unit

  val linkParents : Basics.Structure -> unit

  val shiftSigStamps : bool * Stampset.stampsets * (Basics.stamp -> Basics.stamp)
		       * Basics.Structure -> Basics.Structure

  val shiftFctStamps : (Basics.stamp -> Basics.stamp) * Basics.Functor
		       -> Basics.Functor

end
