(* Copyright 1989 by AT&T Bell Laboratories *)
(* typesutil.sig *)

(* types.sig *)

signature TYPESUTIL = sig

  structure Basics: BASICS
  
  (* primitive operations on tycons *)
  val tycStamp : Basics.tycon -> int  (* get rid of this *)
  val tycName : Basics.tycon -> Basics.Symbol.symbol
  val tycPath : Basics.tycon -> Basics.Symbol.symbol list
  val tyconArity : Basics.tycon -> int
  val setTycPath : Basics.tycon * Basics.Symbol.symbol list -> Basics.tycon
  val eqTycon : Basics.tycon * Basics.tycon -> bool
  val getEpath : int list * Basics.Structure -> Basics.Structure
  val getEpathTyc : int list * Basics.strenv -> Basics.tycon 
  val tyconInContext : Basics.strenv -> Basics.tycon -> Basics.tycon

  val typeInContext : Basics.ty * Basics.strenv -> Basics.ty
  val printableType : Basics.Structure -> Basics.ty -> Basics.ty
  val prune : Basics.ty -> Basics.ty

  val defaultMETA : Basics.tvkind
  val eqTyvar : Basics.tyvar * Basics.tyvar -> bool
  val mkUBOUND : Basics.Symbol.symbol -> Basics.tvkind
  val bindTyvars : Basics.tyvar list -> unit
  val bindTyvars1 : Basics.tyvar list -> {weakness:int, eq:bool} list
    
  exception ReduceType
  val applyTyfun : Basics.tyfun * Basics.ty list -> Basics.ty
  val reduceType : Basics.ty -> Basics.ty
  val headReduceType : Basics.ty -> Basics.ty
  val equalType  : Basics.ty * Basics.ty -> bool

  (* making a "generic" copy of a type *)
  val typeArgs : int -> Basics.ty list
  val mkPolySign : int -> {weakness:int, eq:bool} list
  val applyPoly : Basics.ty * int * int -> Basics.ty
  
  (* matching a scheme against a target type -- used declaring overloadings *)
  val matchScheme : Basics.tyfun * Basics.ty -> Basics.ty

  (* get rid of INSTANTIATED indirections in a type *)
  val compressTy : Basics.ty -> unit  

end  (* signature TYPESUTIL *)
