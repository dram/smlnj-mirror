(* typesutil.sig *)

(* types.sig *)

signature TYPESUTIL = sig

  structure Basics: BASICS
  
  exception ReduceType
  val bindTyvars : Basics.tyvar list -> unit
  val applyTyfun : Basics.tyfun * Basics.ty list -> Basics.ty
  val reduceType : Basics.ty -> Basics.ty
  val equalTycon : Basics.tycon * Basics.tycon -> bool
  val equalType  : Basics.ty * Basics.ty -> bool

  (* matching a scheme against a target type -- used declaring overloadings *)
  val matchScheme : Basics.tyfun * Basics.ty -> Basics.ty
  
  (* making a "generic" copy of a type *)
  val typeArgs : int -> Basics.ty list
  val applyPoly : Basics.ty -> Basics.ty
  
  (* type unification *)
  exception Unify

  val instantiate : Basics.tyvar * Basics.ty ->  unit
  val unifyTy : Basics.ty * Basics.ty -> unit
  val unifyTypes : Basics.ty list -> Basics.ty

end  (* signature TYPESUTIL *)
