(* Copyright 1990 by AT&T Bell Laboratories *)

signature UNIFY =
sig

  (* type unification *)
  exception Unify of string
  val instantiate : Basics.tyvar * Basics.ty ->  unit
  val unifyTy : Basics.ty * Basics.ty -> unit
  val unifyTypes : Basics.ty list -> Basics.ty

end
