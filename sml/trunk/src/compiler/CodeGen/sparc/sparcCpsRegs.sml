(* sparcCpsRegs.sml --- CPS registerS USED ON the Sparc
 *
 * COPYRIGHT (c) 1998 AT&T Bell Laboratories.
 *
 *)

structure SparcCpsRegs : CPSREGS = 
struct
  structure T = SparcMLTree
  structure SL = SortedList
  structure C = SparcCells

  type rexp = (unit, unit, unit, unit) T.rexp
  type fexp = (unit, unit, unit, unit) T.fexp
  type ccexp = (unit, unit, unit, unit) T.ccexp

  val GP = C.GPReg
  val FP = C.FPReg

  fun REG r = T.REG(32,GP r) : rexp
  fun FREG f = T.FREG(64,FP f) : fexp

  val stdarg	= REG(24) (* %i0 *)
  val stdcont	= REG(25) (* %i1 *)
  val stdclos	= REG(26) (* %i2 *)
  val stdlink	= REG(1)  (* %g1 *)
  val baseptr	= REG(27) (* %i3 *)

  val limitptr	= REG(4)  (* %g4 *)
  val varptr	= REG(29) (* %i5 *)
  val exhausted	= SOME(T.CC(T.GTU,C.psr))  (* %psr *) 
  val storeptr	= REG(5)  (* %g5 *)
  val allocptr	= REG(6)  (* %g6 *)
  val exnptr	= REG(7)  (* %g7 *)

  val returnPtr	= GP 15        
  val gcLink	= T.REG(32,returnPtr) : rexp
  val stackptr	= REG(14)

   (* Warning %o2 is used as the asmTmp
    *)    
  val miscregs =
    map REG
              [2, 3,				(* %g2-%g3 *)
	       8, 9,				(* %o0-%o1 *)
	       16, 17, 18, 19, 20, 21, 22, 23,  (* %l0-%l7 *)
	       28, 31,				(* %i4, %i6, %i7 *)
	       11, 12, 13]			(* %o3-%o5 *)
  val calleesave = Array.fromList miscregs

  (* Note: We need at least one register for shuffling purposes. *)
  fun fromto(n, m, inc) = if n>m then [] else n :: fromto(n+inc, m, inc)
  val floatregs = map FREG (fromto(0,31,2))
  val savedfpregs = []

  val allRegs = SL.uniq(fromto(GP 0,GP 31,1))

  val availR = 
    map (fn T.REG(_,r) => r)
        ([stdlink, stdclos, stdarg, stdcont, gcLink] @ miscregs)
  val dedicatedR = SL.remove(SL.uniq availR, allRegs)

  val availF = SL.uniq(fromto(FP 0, FP 30, 2))
  val dedicatedF = []
  val signedGCTest = false
  val addressWidth = 32
end

