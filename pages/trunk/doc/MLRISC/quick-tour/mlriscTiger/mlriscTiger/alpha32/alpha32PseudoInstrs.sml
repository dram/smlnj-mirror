functor Alpha32PseudoInstrs 
  (structure Instr : ALPHA32INSTR 
     where Region=TigerRegions) : ALPHA32_PSEUDO_INSTR = 
struct
  structure I = Instr
  structure C = Instr.C
  type reduceOpnd = I.operand -> int

  val temps = List.foldl C.addReg C.empty [23, 24, 25, 26, 28]

  fun divl({ra, rb, rc}, _) = 
    [I.PSEUDOARITH{oper=I.DIVL, ra=ra, rb=rb, rc=rc, tmps=temps}]

  fun divlu({ra, rb, rc}, _) = 
    [I.PSEUDOARITH{oper=I.DIVLU, ra=ra, rb=rb, rc=rc, tmps=temps}]

  fun cvti2d _ = MLRiscErrorMsg.impossible "Alpha32PseudoInstrs: cvti2d"
end