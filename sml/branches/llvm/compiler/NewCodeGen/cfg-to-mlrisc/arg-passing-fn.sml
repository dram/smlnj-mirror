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

  (* standard convention for functions  *)
    val stdFun : {argTys : CFG.ty list, vfp : bool} -> T.mlrisc list

  (* standard convention for continuations *)
    val stdCont : {argTys : CFG.ty list, vfp : bool} -> T.mlrisc list

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
	  val rest = List.drop(argTys, ncs+nfcs+3)
	  val len = length argTys
	  val gpr = stdarg vfp :: gprfromto(ncs+4, len, vfp)
	  val fpr = fprfromto(nfcs, len, vfp)
	  in
	    stdlink vfp :: stdclos vfp :: stdcont vfp :: calleesaveregs vfp
	      @ scan(rest, gpr, fpr)
	  end

    fun stdCont {vfp, argTys} = let
	  val rest = if ncs > 0
		then List.drop(argTys, ncs+nfcs+1)
		else List.drop(argTys, 2)
	  val len = length argTys
	  val gpr = stdarg vfp :: gprfromto(ncs+4, 1+len, vfp)
	  val fpr = fprfromto(nfcs, len, vfp)
	  in
	    if ncs > 0
	      then stdcont vfp :: calleesaveregs vfp @ scan(rest, gpr, fpr)
	      else stdlink vfp :: stdcont vfp :: scan(rest, gpr, fpr)
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
