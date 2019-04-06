(* INLINE_OPS:   This defines operators which may be inlined in the
   front-end by replacing them with abstract syntax.*)

signature INLINE_OPS =
   sig
      structure Lambda : LAMBDA
      val inlsubscript : unit -> Lambda.lexp
      val inlupdate : Basics.ty option -> Lambda.lexp
      val inlbyteof : unit -> Lambda.lexp
      val inlstore : unit -> Lambda.lexp
      val inlordof : unit -> Lambda.lexp
   end
