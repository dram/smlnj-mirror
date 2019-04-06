(* Copyright 1990 by AT&T Bell Laboratories *)

signature UNIFY =
sig

  (* type unification *)
  exception Unify of string
  val instantiate : Types.tyvar * Types.ty ->  unit
  val unifyTy : Types.ty * Types.ty -> unit
  val unifyTypes : Types.ty list -> Types.ty

end
