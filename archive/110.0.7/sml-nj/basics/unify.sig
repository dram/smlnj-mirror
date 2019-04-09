(* Copyright 1990 by AT&T Bell Laboratories *)

signature UNIFY =
sig

  (* type unification *)
  exception Unify of string
  val unifyTy : Types.ty * Types.ty -> unit

end

(*
 * $Log: unify.sig,v $
 * Revision 1.1.1.1  1999/12/03 19:59:37  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.1.1.1  1997/01/14 01:38:11  george
 *   Version 109.24
 *
 *)
