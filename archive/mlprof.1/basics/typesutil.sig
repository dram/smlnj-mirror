(* typesutil.sig *)

(* types.sig *)

signature TYPESUTIL = sig

  structure Basics: BASICS
  
  (* primitive operations on tycons *)
  val tycStamp : Basics.tycon -> int
  val setTycStamp : int * Basics.tycon -> Basics.tycon
  val tycName : Basics.tycon -> Basics.Symbol.symbol
  val tyconArity : Basics.tycon -> int
  val eqTycon : Basics.tycon * Basics.tycon -> bool
  val tyconInContext : Basics.strenv -> Basics.tycon -> Basics.tycon

  val typeInContext : Basics.ty * Basics.strenv -> Basics.ty
  val prune : Basics.ty -> Basics.ty

  val eqTyvar : Basics.tyvar * Basics.tyvar -> bool
  val bindTyvars : Basics.tyvar list -> unit
    
  exception ReduceType
  val applyTyfun : Basics.tyfun * Basics.ty list -> Basics.ty
  val reduceType : Basics.ty -> Basics.ty
  val equalTycon : Basics.tycon * Basics.tycon -> bool
  val equalType  : Basics.ty * Basics.ty -> bool

  (* making a "generic" copy of a type *)
  val typeArgs : int -> Basics.ty list
  val applyPoly : Basics.ty -> Basics.ty
  
  (* matching a scheme against a target type -- used declaring overloadings *)
  val matchScheme : Basics.tyfun * Basics.ty -> Basics.ty
  
  (* type unification *)
  exception Unify
  val instantiate : Basics.tyvar * Basics.ty ->  unit
  val unifyTy : Basics.ty * Basics.ty -> unit
  val unifyTypes : Basics.ty list -> Basics.ty

end  (* signature TYPESUTIL *)
