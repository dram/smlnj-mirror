(* Copyright 1989 by AT&T Bell Laboratories *)
(* sigmatch.sig *)

signature SIGMATCH =
sig
(*   structure Env:ENV *)

  val match0  : {mapstr1: int*Basics.Structure->unit,
	         maptyc: int*Basics.tycon->unit,
		 mapstr: Basics.Structure*Basics.Structure->unit}
	     -> Basics.env * bool * Symbol.symbol list * Stampset.stampsets
	         * Basics.Structure * Basics.Structure * Basics.Structure
		 * ErrorMsg.complainer
	     -> Basics.Structure * Basics.thinning

  val match   : Basics.env * bool * Symbol.symbol list * Stampset.stampsets
	         * Basics.Structure * Basics.Structure * Basics.Structure
		 * ErrorMsg.complainer
	     -> Basics.Structure * Basics.thinning

  val realize : Basics.env * Basics.env * bool * Symbol.symbol list 
		 * Stampset.stampsets * Basics.stamp
                 * Basics.Structure * Basics.Structure
		 * ErrorMsg.complainer
	     -> Basics.Structure * Basics.trans list
end

