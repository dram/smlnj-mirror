(* typesutil.sig *)

(* types.sig *)

signature TYPESUTIL = sig

  structure Basics: BASICS
  
  (* matching a scheme against a target type -- used declaring overloadings *)
  val matchType : Basics.ty * Basics.ty -> Basics.ty list
  
  (* making a "generic" copy of a type *)
  val freshTy : Basics.ty -> Basics.ty
  
  (* type unification *)
  exception Unify

  val instantiate : Basics.tyvar * Basics.ty ->  unit
  val expandTy : Basics.ty -> Basics.ty
  val unifyTy : Basics.ty * Basics.ty -> unit
  val unifyTypes : Basics.ty list -> Basics.ty

end  (* signature TYPESUTIL *)
