(* Copyright 1989 by AT&T Bell Laboratories *)
(* buildmod.sig *)

signature BUILDMOD =
sig

    val binderGt : (Symbol.symbol * Basics.binding) *
		   (Symbol.symbol * Basics.binding) -> bool

    val buildStrTable : Basics.env -> Basics.trans list * Basics.env

end  (* signature BUILDMOD *)

