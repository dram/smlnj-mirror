functor HppaMillicode
  (structure MLTree : MLTREE
   structure Instr : HPPAINSTR
     sharing MLTree.Constant = Instr.Constant) : HPPA_MILLICODE =
struct
  structure T = MLTree
  structure C = Instr.C
  structure I = Instr
  structure Region = I.Region

  val arg1 = 26
  val arg2 = 25
  val ra = 31				(* milli return address *)
  val rv = 29				(* milli return value *)
  val sp = C.stackptrR
  val stack = Region.stack

  val udivOffset = ~16
  val divOffset = ~20
  val mulOffset = ~24
  val muluOffset = ~112
  val cvti2dOffset = ~4
  fun copyTmp() = SOME(I.Direct(C.newReg()))

  fun doMilliCall offset {rs, rt, rd} = let
    fun addList([], cs) = cs
      | addList(r::rs, cs) = addList(rs, C.addReg(r,cs))
    val tmpR = C.newReg()
    val defs = addList([rv,ra], C.empty)
    val uses = C.addReg(arg1, C.addReg(arg2, C.empty))
  in 
    [I.COPY{dst=[arg1, arg2], src=[rs, rt], impl=ref NONE, tmp=copyTmp()},
     I.LOADI{li=I.LDW, r=C.stackptrR, i=I.IMMED offset, t=tmpR, mem=stack},
     I.BLE{t=31, b=tmpR, sr=5, d=I.IMMED 0, defs=defs, uses=uses},
     I.COPY{dst=[rd], src=[rv], impl=ref NONE, tmp=copyTmp()}]
  end

  val divu = doMilliCall udivOffset
  val divo = doMilliCall divOffset
  val mulo = doMilliCall mulOffset
  val mulu = doMilliCall muluOffset

  fun cvti2d{rs, fd} = 
    [I.STORE{st=I.STW, b=sp, d=I.IMMED cvti2dOffset, r=rs, mem=stack},
     I.FLOAD{fl=I.FLDWS, b=sp, d=cvti2dOffset, t=fd, mem=Region.stack},
     I.FUNARY{fu=I.FCNVXF, f=fd, t=fd}]

end

(*
 * $Log: hppaMillicode.sml,v $
 * Revision 1.1.1.1  1998/04/08 18:39:55  george
 * Version 110.5
 *
 *)
