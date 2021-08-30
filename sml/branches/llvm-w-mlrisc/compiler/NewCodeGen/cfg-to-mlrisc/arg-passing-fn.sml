(* arg-passing-fn.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

functor ArgPassingFn (

    structure C : CPSREGS
    structure MS : MACH_SPEC

  ) : sig

    structure T : MLTREE

  (* standard convention for functions; returns the register for the call
   * plus the formal argument registers, which includes the function.
   *)
    val stdFun : {argTys : CFG.ty list, vfp : bool} -> T.rexp * T.mlrisc list

  (* standard convention for continuations; returns the register for the call
   * plus the formal argument registers, which includes the function.
   *)
    val stdCont : {argTys : CFG.ty list, vfp : bool} -> T.rexp * T.mlrisc list

  (* fixed calling convention for known functions that require
   * garbage collection on machines that have registers implemented
   * as memory locations.
   *)
    val fixed : {argTys : CFG.ty list, vfp : bool} -> T.mlrisc list

  end  = struct
    structure T : MLTREE = C.T

    fun error msg = ErrorMsg.impossible ("ArgPassing." ^ msg)

    val ncs = MS.numCalleeSaves
    val nfcs = MS.numFloatCalleeSaves

    fun stdlink vfp = T.GPR (C.stdlink vfp)
    fun stdclos vfp = T.GPR (C.stdclos vfp)
    fun stdarg vfp  = T.GPR (C.stdarg vfp)
    fun stdcont vfp = T.GPR (C.stdcont vfp)

    val miscRegs = map T.GPR C.miscregs
    fun gpregs vfp = stdlink vfp :: stdclos vfp :: stdarg vfp :: stdcont vfp :: miscRegs
    val fpregs = map T.FPR (C.savedfpregs @ C.floatregs)

  (* return the elements indexed i..j from the list regs *)
    fun fromto (i, j, regs) = let
	(* NOTE: the `to` function is almost `List.take`, but it does not raise
	 * an exception when ncs >= lenth regs.
	 *)
	  fun to (ncs, []) = []
	    | to (ncs, r::rs) = if ncs > j then [] else r::to(ncs+1, rs)
	  in
	    to (i, List.drop(regs, i))
	  end

    fun gprfromto (i, j, vfp) = fromto(i, j, gpregs vfp)
    fun fprfromto (i, j, vfp) = fromto(i, j, fpregs)
    fun calleesaveregs vfp = List.take(miscRegs, ncs) @ fprfromto(0, nfcs-1, vfp)

    fun isFlt (CFG.FLTt _) = true
      | isFlt _ = false

    fun scan (t::z, gp, fp) = if isFlt t
	  then (case fp
	     of f::fr => f :: scan(z, gp, fr)
	      | [] => error "scan: out of floating-point registers"
	    (* end case *))
	  else (case gp
	     of g::gr => g :: scan(z, gr, fp)
	      | [] => error "scan: out of registers"
	    (* end case *))
      | scan ([], _, _) = []

    fun stdFun {vfp, argTys} = let
	(* for a standard function call, the first three arguments are the
	 * stdLink, stdClos, and stdCont registers; then come callee-save regs,
	 * then come arguments.  The list `rest` will be the arguments.
	 *)
	  val rest = List.drop(argTys, ncs+nfcs+3)
	  val len = length argTys
	  val gpr = stdarg vfp :: gprfromto(ncs+4, len, vfp)
	  val fpr = fprfromto(nfcs, len, vfp)
	  val stdLink = C.stdlink vfp
	  val formals = T.GPR stdLink :: stdclos vfp :: stdcont vfp
		:: calleesaveregs vfp
	        @ scan(rest, gpr, fpr)
	  in
	    (stdLink, formals)
	  end

    fun stdCont {vfp, argTys} = let
	  val rest = if ncs > 0
		then List.drop(argTys, ncs+nfcs+1)
		else List.drop(argTys, 2)
	  val len = length argTys
	  val gpr = stdarg vfp :: gprfromto(ncs+4, 1+len, vfp)
	  val fpr = fprfromto(nfcs, len, vfp)
	  val stdCont = C.stdcont vfp
	  val stdLink = C.stdlink vfp
	  in
	    if ncs > 0
	      then (stdCont, T.GPR stdCont :: calleesaveregs vfp @ scan(rest, gpr, fpr))
	      else (stdLink, T.GPR stdLink :: T.GPR stdCont :: scan(rest, gpr, fpr))
	  end

  (* use an arbitary but fixed set of registers. *)
    fun fixed {vfp, argTys} = let
	  fun iter (CFG.FLTt _::rest, regs, f::fregs) = f::iter(rest, regs, fregs)
	    | iter (_::rest, r::regs, fregs) = r::iter(rest, regs, fregs)
	    | iter ([], _, _) = []
	    | iter _ = error "fixed: out of registers"
          in
	    iter(argTys, gpregs vfp, fpregs)
          end

  end
