(* Copyright 1989 by AT&T Bell Laboratories *)
(* typesutil.sig *)

(* types.sig *)

signature TYPESUTIL = sig

  structure Basics: BASICS
  
  (* primitive operations on tycons *)
  val tycName : Basics.tycon -> Symbol.symbol
  val tycPath : Basics.tycon -> Symbol.symbol list
  val tyconArity : Basics.tycon -> int
  val setTycPath : Basics.tycon * Symbol.symbol list -> Basics.tycon
  val eqTycon : Basics.tycon * Basics.tycon -> bool
  val getEpath : int list * Basics.Structure -> Basics.Structure
  val getEpathTyc : int list * Basics.strenv -> Basics.tycon 
  val tyconInContext : Basics.strenv -> Basics.tycon -> Basics.tycon

  val typeInContext : Basics.ty * Basics.strenv -> Basics.ty
  val printableType : Basics.Structure -> Basics.ty -> Basics.ty
  val prune : Basics.ty -> Basics.ty

  val defaultMETA : Basics.tvkind
  val eqTyvar : Basics.tyvar * Basics.tyvar -> bool
  val mkUBOUND : Symbol.symbol * ErrorMsg.complainer -> Basics.tvkind
  val bindTyvars : Basics.tyvar list -> unit
  val bindTyvars1 : Basics.tyvar list -> {weakness:int, eq:bool} list
    
  exception ReduceType
  val applyTyfun : Basics.tyfun * Basics.ty list -> Basics.ty
  val reduceType : Basics.ty -> Basics.ty
  val headReduceType : Basics.ty -> Basics.ty
  val equalType  : Basics.ty * Basics.ty -> bool
  val equalTycon : Basics.tycon * Basics.tycon -> bool

  (* making a "generic" copy of a type *)
  val typeArgs : int -> Basics.ty list
  val mkPolySign : int -> {weakness:int, eq:bool} list
  val applyPoly : Basics.ty * int * int * int -> Basics.ty
  
  (* matching a scheme against a target type -- used declaring overloadings *)
  val matchScheme : Basics.tyfun * Basics.ty -> Basics.ty

  (* get rid of INSTANTIATED indirections in a type *)
  val compressTy : Basics.ty -> unit  

  type occ
  val Rand : occ -> occ
  val Abstr : occ -> occ
  val Rator : occ -> occ
  val LetDef: occ -> occ
  val Root : occ
  val lamdepth : occ -> int
  val abscount : occ -> int
  val base : occ -> int
  val wmax : occ -> int
  val toplevel : occ -> bool

end  (* signature TYPESUTIL *)
