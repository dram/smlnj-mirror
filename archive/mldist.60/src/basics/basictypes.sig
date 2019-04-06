(* Copyright 1989 by AT&T Bell Laboratories *)
(* basictypes.sig *)

signature BASICTYPES = sig

structure Basics: BASICS

infix -->
val arrowTycon : Basics.tycon 
val --> : Basics.ty * Basics.ty -> Basics.ty
val isArrowType : Basics.ty -> bool
val domain : Basics.ty -> Basics.ty
val range : Basics.ty -> Basics.ty

val intTycon : Basics.tycon 
val intTy : Basics.ty

val realTycon  : Basics.tycon 
val realTy : Basics.ty

val stringTycon  : Basics.tycon 
val stringTy : Basics.ty

val exnTycon : Basics.tycon 
val exnTy : Basics.ty

val contTycon : Basics.tycon 

val arrayTycon : Basics.tycon 

val unitTycon : Basics.tycon 
val unitTy : Basics.ty
val isUnitTy : Basics.ty -> bool  (* delete this when recompiling everything *)

val recordTy : (Basics.label * Basics.ty) list -> Basics.ty
val tupleTy : Basics.ty list -> Basics.ty

val boolTycon : Basics.tycon 
val boolTy : Basics.ty
val falseDcon : Basics.datacon
val trueDcon : Basics.datacon

val optionTycon : Basics.tycon 
val NONEDcon : Basics.datacon
val SOMEDcon : Basics.datacon

val refTycon : Basics.tycon 
val refPatType : Basics.ty
val refDcon : Basics.datacon

val listTycon : Basics.tycon 
val nilDcon : Basics.datacon
val consDcon : Basics.datacon

end (* signature BASICTYPES *)
