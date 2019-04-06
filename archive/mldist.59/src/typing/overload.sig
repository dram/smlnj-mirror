(* Copyright 1989 by AT&T Bell Laboratories *)
(* overload.sig *)

signature OVERLOAD =
sig
  val resetOverloaded : unit -> unit
  val pushOverloaded : Basics.var ref * ErrorMsg.complainer -> Basics.ty
  val resolveOverloaded : unit -> unit
end  (* signature OVERLOAD *)
