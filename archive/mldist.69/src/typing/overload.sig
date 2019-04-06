(* Copyright 1989 by AT&T Bell Laboratories *)
(* overload.sig *)

signature OVERLOAD =
sig
(*   structure Env:ENV *)
  val resetOverloaded : unit -> unit
  val pushOverloaded : Basics.var ref * ErrorMsg.complainer -> Basics.ty
  val resolveOverloaded : Basics.env -> unit
end  (* signature OVERLOAD *)
