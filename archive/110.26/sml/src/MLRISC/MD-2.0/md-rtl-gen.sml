(*
 * This takes a bunch of RTL and build a expbase that can be reused.
 *)
functor MDRTLGen
   (structure Ast  : MD_AST
    structure Term : MD_TERM) : MD_RTL_GEN =
struct

   structure Term = Term
   type cell    = Term.cell
   type exp     = Term.exp
   type cond    = Term.cond
   type operand = Term.operand
   type stm     = Term.stm 
   type addressWidth = unit
   type label   = string
   type address = exp
   type ty      = string * int option

   open Term

   fun rtlReg name   = REG(tyvar(),name)
   fun rtlCCReg name = CCREG(tyvar(),name)
   fun rtlOpnd name  = OPND(tyvar(),name)
   fun rtlLabel name = name

   fun newOp name = 
       let val ty = tyvar()
       in  fn args => OP(ty, name, args) end
   fun newCCOp name = 
       let val ty = tyvar()
       in  fn args => CMP(ty, name, args) end

   fun newStm name = ACTION(name,[])

   infix := ::= :::=
 
   val ?        = UNDEF
   fun ! x      = DEREF(tyvar(),x)
   fun !! x     = CCDEREF(tyvar(),x)
   fun % x      = DEREFOPND(tyvar(),x)
   fun %% x     = LAB(tyvar(),x)
   fun x := y   = ASSIGN(tyvar(),x,y)
   fun x ::= y  = CCASSIGN(tyvar(),x,y)
   fun int i    = LI(tyvar(),i)
   fun word w   = LI32(tyvar(),w)

   (* Integer operators *)
   fun ~ x       = OP(tyvar(),"~",[x])
   fun x + y     = OP(tyvar(),"+",[x,y])
   fun x - y     = OP(tyvar(),"-",[x,y])
   fun muls(x,y) = OP(tyvar(),"muls",[x,y])
   fun mulu(x,y) = OP(tyvar(),"mulu",[x,y])
   fun divs(x,y) = OP(tyvar(),"divs",[x,y])
   fun divu(x,y) = OP(tyvar(),"divu",[x,y])
   fun rems(x,y) = OP(tyvar(),"rems",[x,y])
   fun remu(x,y) = OP(tyvar(),"remu",[x,y])

   fun andb(x,y) = OP(tyvar(),"andb",[x,y])
   fun orb(x,y)  = OP(tyvar(),"orb",[x,y])
   fun xorb(x,y) = OP(tyvar(),"xorb",[x,y])
   fun eqvb(x,y) = OP(tyvar(),"eqvb",[x,y])
   fun notb(x)   = OP(tyvar(),"notb",[x])
   fun <<(x,y)   = OP(tyvar(),"<<",[x,y])
   fun >>(x,y)   = OP(tyvar(),">>",[x,y])
   fun ~>>(x,y)  = OP(tyvar(),"~>>",[x,y])

   (* Trapping operators *)
   fun addt(x,y) = OP(tyvar(),"addt",[x,y])
   fun subt(x,y) = OP(tyvar(),"subt",[x,y])
   fun mult(x,y) = OP(tyvar(),"mult",[x,y])
   fun divt(x,y) = OP(tyvar(),"divt",[x,y])
   fun remt(x,y) = OP(tyvar(),"remt",[x,y])

   fun cond(a,b,c) = COND(tyvar(),a,b,c)

   (* Integer comparisons *)
   fun ==(x,y)  = CMP(tyvar(),"==",[x,y])
   fun x <> y   = CMP(tyvar(),"<>",[x,y])
   fun x > y    = CMP(tyvar(),">",[x,y])
   fun x < y    = CMP(tyvar(),"<",[x,y])
   fun x <= y   = CMP(tyvar(),"<=",[x,y])
   fun x >= y   = CMP(tyvar(),">=",[x,y])
   fun ltu(x,y) = CMP(tyvar(),"ltu",[x,y])
   fun leu(x,y) = CMP(tyvar(),"leu",[x,y])
   fun gtu(x,y) = CMP(tyvar(),"gtu",[x,y])
   fun geu(x,y) = CMP(tyvar(),"geu",[x,y])

   (* Floating point operators *)
   fun fadd(x,y) = OP(tyvar(),"fadd",[x,y]) 
   fun fsub(x,y) = OP(tyvar(),"fsub",[x,y]) 
   fun fmul(x,y) = OP(tyvar(),"fmul",[x,y])
   fun fdiv(x,y) = OP(tyvar(),"fdiv",[x,y])
   fun fabs(x)   = OP(tyvar(),"fabs",[x])
   fun fneg(x)   = OP(tyvar(),"fneg",[x])
   fun fsqrt(x)  = OP(tyvar(),"fsqrt",[x])

   (* Conversions *)
   fun cvti2i x = OP(tyvar(),"cvti2i",[x])
   fun cvti2f x = OP(tyvar(),"cvti2f",[x])
   fun cvtf2i x = OP(tyvar(),"cvtf2i",[x])
   fun cvtf2f x = OP(tyvar(),"cvtf2f",[x])

   (* Floating point comparisons *)
   fun |?|(x,y)    = CMP(tyvar(),"|?|",[x,y])     
   fun |!<=>|(x,y) = CMP(tyvar(),"|!<=>|",[x,y])  
   fun |==|(x,y)   = CMP(tyvar(),"|==|",[x,y])    
   fun |?=|(x,y)   = CMP(tyvar(),"|?=|",[x,y])    
   fun |!<>|(x,y)  = CMP(tyvar(),"|!<>|",[x,y])   
   fun |!?>=|(x,y) = CMP(tyvar(),"|!?>=|",[x,y])  
   fun |<|(x,y)    = CMP(tyvar(),"|<|",[x,y])     
   fun |?<|(x,y)   = CMP(tyvar(),"|?<|",[x,y])    
   fun |!>=|(x,y)  = CMP(tyvar(),"|!>=|",[x,y])   
   fun |!?>|(x,y)  = CMP(tyvar(),"|!?>|",[x,y])   
   fun |<=|(x,y)   = CMP(tyvar(),"|<=|",[x,y])    
   fun |?<=|(x,y)  = CMP(tyvar(),"|?<=|",[x,y])   
   fun |!>|(x,y)   = CMP(tyvar(),"|!>|",[x,y])    
   fun |!?<=|(x,y) = CMP(tyvar(),"|!?<=|",[x,y])  
   fun |>|(x,y)    = CMP(tyvar(),"|>|",[x,y])     
   fun |?>|(x,y)   = CMP(tyvar(),"|?>|",[x,y])    
   fun |!<=|(x,y)  = CMP(tyvar(),"|!<=|",[x,y])   
   fun |!?<|(x,y)  = CMP(tyvar(),"|!?<|",[x,y])   
   fun |>=|(x,y)   = CMP(tyvar(),"|>=|",[x,y])    
   fun |?>=|(x,y)  = CMP(tyvar(),"|?>=|",[x,y])   
   fun |!<|(x,y)   = CMP(tyvar(),"|!<|",[x,y])    
   fun |!?=|(x,y)  = CMP(tyvar(),"|!?=|",[x,y])   
   fun |<>|(x,y)   = CMP(tyvar(),"|<>|",[x,y])    
   fun |!=|(x,y)   = CMP(tyvar(),"|!=|",[x,y])    
   fun |!?|(x,y)   = CMP(tyvar(),"|!?|",[x,y])    
   fun |<=>|(x,y)  = CMP(tyvar(),"|<=>|",[x,y])   
   fun |?<>|(x,y)  = CMP(tyvar(),"|?<>|",[x,y])   

   (* Convert address into a cell *)
   fun $ (k,n) addr = MEM(TYP(SOME k,n),addr)

   fun || (a,b)  = PAR(a,b)
   val Nop       = NOP
   fun Jmp addr  = JMP(addr)
   fun Call addr = CALL(addr)
   val Ret       = RET
   fun If(a,b,c) = IF(a,b,c)

end
