functor Alpha32PseudoInstrs
  (structure Instr : ALPHAINSTR 
     where Region=CPSRegions  
   structure T     : MLTREE
     where Region=CPSRegions
  ) : ALPHA_PSEUDO_INSTR = 
struct
  structure I = Instr
  structure T = T
  structure C = Instr.C

  fun error msg = MLRiscErrorMsg.impossible("Alpha32PsuedoInstrs."^msg)

  type reduceOpnd = I.operand -> C.cell

  val floatTmpOffset = I.IMMop 96	(* runtime system dependent *)
  val floatTmpOffset8 = I.IMMop(96+8)		(* " *)
  val divlOffset = I.IMMop 120			(* " *)
  val divluOffset = I.IMMop 124			(* " *)

  val stack = CPSRegions.stack
  val sp = C.stackptrR
  val zeroR = 31

  val makeCellset = List.foldl C.addReg C.empty 
  val defs = makeCellset (map C.GPReg [0, 23, 24, 25, 26, 28])
  val uses = makeCellset (map C.GPReg [16, 17])
  fun copyTmp() = SOME(I.Direct(C.newReg()))

  val r16 = C.GPReg 16
  val r17 = C.GPReg 17
  val r26 = C.GPReg 26
  val r27 = C.GPReg 27
  val r0  = C.GPReg 0

  fun divlv({ra, rb, rc}, reduceOpnd) = 
    [I.COPY{dst=[r16, r17], src=[ra, reduceOpnd rb], impl=ref NONE, 
	    tmp=copyTmp()},
     I.LOAD{ldOp=I.LDL, r=r27, b=sp, d=divlOffset, mem=stack},
     I.JSR{r=r26, b=r27, d=0, defs=defs, uses=uses, mem=stack},
     I.COPY{dst=[rc], src=[r0], impl=ref NONE, tmp=NONE}]

  fun divlu({ra, rb, rc}, reduceOpnd) = 
    [I.COPY{dst=[r16, r17], src=[ra, reduceOpnd rb], impl=ref NONE, 
	    tmp=copyTmp()},
     I.LOAD{ldOp=I.LDL, r=r27, b=sp, d=divluOffset, mem=stack},
     I.JSR{r=r26, b=r27, d=0, defs=defs, uses=uses, mem=stack},
     I.COPY{dst=[rc], src=[r0], impl=ref NONE, tmp=NONE}]

  fun unimplemented _ = error "unimplemented pseudo-instr"
  val divl  = unimplemented
  val divqv = unimplemented
  val divq  = unimplemented
  val divqu = unimplemented
  val remlv = unimplemented
  val reml  = unimplemented
  val remlu = unimplemented
  val remqv = unimplemented
  val remq  = unimplemented
  val remqu = unimplemented
     
  fun cvtlt({opnd, fd}, reduceOpnd) = 
  let val ra = reduceOpnd opnd
  in  [I.STORE{stOp=I.STQ, r=ra, b=sp, d=floatTmpOffset, mem=stack},
       I.FLOAD{ldOp=I.LDT, r=fd, b=sp, d=floatTmpOffset, mem=stack},
       I.FUNARY{oper=I.CVTQT, fb=fd, fc=fd}]
  end

  val cvtls = unimplemented
  val cvtqt = unimplemented
  val cvtqs = unimplemented
  val cvtsl = unimplemented
  val cvttl = unimplemented
  val cvtsq = unimplemented
  val cvttq = unimplemented
end
