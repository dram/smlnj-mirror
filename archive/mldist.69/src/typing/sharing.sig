(* Copyright 1989 by AT&T Bell Laboratories *)
(* sharing.sig *)

signature SHARING =
  sig
    val doSharing : Basics.env * Basics.Structure * ErrorMsg.complainer ->
			Basics.Structure
    val checkSharing : Basics.env * Basics.env * Basics.strenv 
			* Basics.sharespec * ErrorMsg.complainer -> unit
  end
