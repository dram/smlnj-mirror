(* sparcgen.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure SparcMC =
  CPSCompFn(
    structure Gen=SparcCG
    fun collect epthunk = (SparcCG.finish ();
			   CodeString.getCodeString (epthunk ())))


