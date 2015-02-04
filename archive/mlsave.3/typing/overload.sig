(* noverload.sig *)

signature OVERLOAD = sig

  structure Basics : BASICS
  
  exceptionx overld: unit
  
  val resetOverloaded : unit -> unit
  val markOverloaded : unit -> unit
  val pushOverloaded : Basics.var ref -> Basics.ty
  val resolveOverloaded : unit -> unit

end  (* signature OVERLOAD *)
