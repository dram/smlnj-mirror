functor HppaMillicode
  (structure MLTree : MLTREE
   structure Instr : HPPAINSTR
   structure Frame : FRAME
     sharing MLTree.Constant = Instr.Constant) = 
struct
  structure T = MLTree
  structure I = Instr
  structure C = I.C
  structure LE = LabelExp
  structure Imports = HppaImports

  val milliArg0 = 26
  val milliArg1 = 25
  val milliRet  = 29

  (* The standard register convention is used for millicalls with 
   * a few minor exceptions. 
   *)
  fun doMilli proc {rs, rt, rd} = let
    val cellset = foldr (fn (r, set) => C.addReg(r, set)) C.empty
    val defs = cellset (31 :: milliRet :: Frame.callersaved)
    val uses = cellset [milliArg0, milliArg1]
  in
    Imports.add (Imports.MILLICODE, proc);

    [I.COPY{dst=[milliArg0,milliArg1], src=[rs,rt], impl=ref NONE,
	    tmp=SOME(I.Direct(C.newReg()))},
     I.BL{t=31, x=I.LabExp(LE.LABEL proc, I.T), defs=defs, uses=uses, n=true},
     I.COPY{dst=[rd], src=[milliRet], impl=ref NONE, tmp=NONE}]
  end

  val divu = doMilli Imports.divu
  val divo = doMilli Imports.divo
  val mulo = doMilli Imports.mulo
  val mulu = doMilli Imports.mulu
  fun cvti2d _ = MLRiscErrorMsg.impossible ("HppaMillcode:cvti2d")
end