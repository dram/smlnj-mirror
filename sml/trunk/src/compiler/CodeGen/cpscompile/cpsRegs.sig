(* cpsRegs.sig
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * The registers used for CPS compilation.
 *
 *)

signature CPSREGS = sig
  structure T : MLTREE
  val allocptr 	: T.rexp	(* must be a regisiter, - T.REG(r) *)
  val limitptr 	: T.rexp
  val stdlink	: T.rexp
  val stdclos	: T.rexp
  val stdarg 	: T.rexp 
  val stdcont 	: T.rexp 
  val exnptr 	: T.rexp 
  val varptr  	: T.rexp 
  val baseptr	: T.rexp
  val storeptr 	: T.rexp 
  val stackptr 	: T.rexp 
  val maskreg 	: T.rexp 

  val calleesave: T.rexp Array.array
  val exhausted : T.ccexp option

  val miscregs  : T.rexp list
  val floatregs : T.fexp list
  val savedfpregs : T.fexp list

  val dedicatedR : int list
  val availR     : int list
  val dedicatedF : int list
  val availF     : int list
end

(*
 * $Log: cpsRegs.sig,v $
 * Revision 1.3  1998/05/23 14:09:17  george
 *   Fixed RCS keyword syntax
 *
 *)
