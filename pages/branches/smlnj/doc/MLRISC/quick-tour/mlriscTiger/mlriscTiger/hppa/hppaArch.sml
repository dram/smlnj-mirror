functor HppaArch
  (structure MLTree : MLTREE 
      where Constant = HppaIrConstant) : TARGET_CONVENTIONS = 
struct
  structure M = MLTree
  structure F = HppaFrame
  structure T = Tree
  structure Const = HppaIrConstant

  val sp = F.SP

  fun mkCall{frame, proc, defs, uses, nArgs} = 
    [M.CALL(proc, defs, uses),
     M.MV(F.FP, M.SUB(M.REG sp, M.CONST(Const.FRAMESIZE frame), M.LR))]


  fun functionPrologue temps = let
    val framesize = Const.FRAMESIZE (!F.currFrame)
  in
    [M.MV(F.FP, M.REG sp),
     M.MV(sp, M.ADD(M.REG sp, M.CONST framesize))]
  end


  fun functionEpilogue temps = let
    val framesize = Const.FRAMESIZE (!F.currFrame)
  in [M.MV(sp, M.SUB(M.REG sp, M.CONST framesize, M.LR)), M.RET]
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
