functor Alpha32Arch
  (structure MLTree : MLTREE
     where Constant = Alpha32IrConstant) : TARGET_CONVENTIONS = 
struct
  structure M = MLTree
  structure F = Alpha32Frame
  structure T = Tree
  structure Const = Alpha32IrConstant

  val sp = F.SP

  fun functionPrologue () = let
    val framesize = Const.FRAMESIZE (!F.currFrame)
  in
    [M.MV(F.FP, M.REG sp),
     M.MV(sp, M.SUB(M.REG sp, M.CONST framesize, M.LR))]
  end

  fun functionEpilogue () = let
    val framesize = Const.FRAMESIZE (!F.currFrame)
  in [M.MV(sp, M.ADD(M.REG sp, M.CONST framesize)), M.RET]
  end

  fun mkCall{frame, proc, defs, uses, nArgs} = let
    val SOME ra = F.RA
  in
    [M.MV(F.PV, proc),
     M.CALL(M.REG F.PV, defs, uses),
     M.MV(F.GP, M.REG ra), 
     M.MV(F.FP, M.ADD(M.REG sp, M.CONST(Const.FRAMESIZE frame)))]
  end

  (* deposit memory arguments starting from the ith argument  *)
  val numArgRegs = length F.argRegs
  fun memArgs(i, args) = let
    fun argToMem(slot, e) = 
	T.MOVE(T.MEM(
		 T.BINOP(T.PLUS,
			   T.CONST((slot-numArgRegs) * F.wordSize),
			   T.TEMP sp)),
	       e)
    fun dump(i, [e]) = argToMem(i, e)
      | dump(i, e::el) = T.SEQ(argToMem(i, e), dump(i+1, el))
  in dump(i, args)
  end
end
