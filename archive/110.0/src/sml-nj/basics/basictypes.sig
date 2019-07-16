(* Copyright 1996 by AT&T Bell Laboratories *)
(* basictypes.sig *)
 
signature BASICTYPES =
sig

  val arrowTycon : Types.tycon 
  val --> : Types.ty * Types.ty -> Types.ty
  val isArrowType : Types.ty -> bool
  val domain : Types.ty -> Types.ty
  val range : Types.ty -> Types.ty

  val intTycon : Types.tycon 
  val intTy : Types.ty

  val int32Tycon : Types.tycon 
  val int32Ty : Types.ty

  val realTycon  : Types.tycon 
  val realTy : Types.ty

  val wordTycon : Types.tycon 
  val wordTy : Types.ty

  val word8Tycon : Types.tycon
  val word8Ty: Types.ty

  val word32Tycon : Types.tycon
  val word32Ty: Types.ty

  val stringTycon  : Types.tycon 
  val stringTy : Types.ty

  val charTycon  : Types.tycon 
  val charTy : Types.ty

  val exnTycon : Types.tycon 
  val exnTy : Types.ty

  val contTycon : Types.tycon 
  val ccontTycon : Types.tycon 

  val arrayTycon : Types.tycon 

  val vectorTycon : Types.tycon

  val objectTycon : Types.tycon
  val c_functionTycon : Types.tycon
  val word8arrayTycon : Types.tycon
  val real64arrayTycon : Types.tycon
  val spin_lockTycon : Types.tycon

  val unitTycon : Types.tycon 
  val unitTy : Types.ty

  val recordTy : (Types.label * Types.ty) list -> Types.ty
  val tupleTy : Types.ty list -> Types.ty

  val boolTycon : Types.tycon 
  val boolTy : Types.ty
  val boolsign : Access.consig
  val falseDcon : Types.datacon
  val trueDcon : Types.datacon

  (*  
   *  Unnecessary; removed by appel
   *  val optionTycon : Types.tycon 
   *  val NONEDcon : Types.datacon
   *  val SOMEDcon : Types.datacon
   *)

  val refTycon : Types.tycon 
  val refPatType : Types.ty
  val refDcon : Types.datacon

  val listTycon : Types.tycon 
  val nilDcon : Types.datacon
  val consDcon : Types.datacon

  val ulistTycon : Types.tycon 
  val unilDcon : Types.datacon
  val uconsDcon : Types.datacon

  val fragTycon : Types.tycon
  val ANTIQUOTEDcon : Types.datacon
  val QUOTEDcon : Types.datacon

end (* signature BASICTYPES *)

(*
 * $Log: basictypes.sig,v $
 * Revision 1.1.1.1  1999/12/03 19:59:36  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.2  1997/12/03 21:10:39  dbm
 *   Fix for Word8Array.array equality problem (basis/tests/word8array.sml,
 *   test1).
 *   Added word8arrayTycon, etc.
 *
 * Revision 1.1.1.1  1997/01/14  01:38:09  george
 *   Version 109.24
 *
 *)
