(*
 * This file was automatically generated by MDGen (v2.0)
 * from the machine description file "x86/x86.md".
 *)


signature X86INSTR =
sig
   structure C : X86CELLS
   structure Constant: CONSTANT
   structure LabelExp: LABELEXP
   structure Region : REGION
      sharing Constant = LabelExp.Constant
   datatype operand =
     Immed of Int32.int
   | ImmedLabel of LabelExp.labexp
   | Relative of int
   | LabelEA of LabelExp.labexp
   | Direct of int
   | FDirect of int
   | ST of int
   | MemReg of int
   | Displace of {base:int, disp:operand, mem:Region.region}
   | Indexed of {base:int option, index:int, scale:int, disp:operand, mem:Region.region
     }
   type addressing_mode = operand
   type ea = operand
   datatype cond =
     EQ
   | NE
   | LT
   | LE
   | GT
   | GE
   | B
   | BE
   | A
   | AE
   | C
   | NC
   | P
   | NP
   | O
   | NO
   datatype binaryOp =
     ADDL
   | SUBL
   | ANDL
   | ORL
   | XORL
   | SHLL
   | SARL
   | SHRL
   | ADCL
   | SBBL
   | ADDW
   | SUBW
   | ANDW
   | ORW
   | XORW
   | SHLW
   | SARW
   | SHRW
   | ADDB
   | SUBB
   | ANDB
   | ORB
   | XORB
   | SHLB
   | SARB
   | SHRB
   datatype multDivOp =
     MULL
   | IDIVL
   | DIVL
   datatype unaryOp =
     DECL
   | INCL
   | NEGL
   | NOTL
   | NOTW
   | NOTB
   datatype move =
     MOVL
   | MOVB
   | MOVW
   | MOVSWL
   | MOVZWL
   | MOVSBL
   | MOVZBL
   datatype fbinOp =
     FADDP
   | FADDS
   | FMULP
   | FMULS
   | FCOMS
   | FCOMPS
   | FSUBP
   | FSUBS
   | FSUBRP
   | FSUBRS
   | FDIVP
   | FDIVS
   | FDIVRP
   | FDIVRS
   | FADDL
   | FMULL
   | FCOML
   | FCOMPL
   | FSUBL
   | FSUBRL
   | FDIVL
   | FDIVRL
   datatype fibinOp =
     FIADDS
   | FIMULS
   | FICOMS
   | FICOMPS
   | FISUBS
   | FISUBRS
   | FIDIVS
   | FIDIVRS
   | FIADDL
   | FIMULL
   | FICOML
   | FICOMPL
   | FISUBL
   | FISUBRL
   | FIDIVL
   | FIDIVRL
   datatype funOp =
     FABS
   | FCHS
   | FSIN
   | FCOS
   | FTAN
   | FSCALE
   | FRNDINT
   | FSQRT
   | FTST
   | FXAM
   | FINCSTP
   | FDECSTP
   datatype fenvOp =
     FLDENV
   | FNLDENV
   | FSTENV
   | FNSTENV
   datatype instruction =
     NOP
   | JMP of (operand * Label.label list)
   | JCC of {cond:cond, opnd:operand}
   | CALL of (operand * C.cellset * C.cellset * Region.region)
   | ENTER of {src1:operand, src2:operand}
   | LEAVE
   | RET of operand option
   | MOVE of {mvOp:move, src:operand, dst:operand}
   | LEA of {r32:int, addr:operand}
   | CMPL of {lsrc:operand, rsrc:operand}
   | CMPW of {lsrc:operand, rsrc:operand}
   | CMPB of {lsrc:operand, rsrc:operand}
   | TESTL of {lsrc:operand, rsrc:operand}
   | TESTW of {lsrc:operand, rsrc:operand}
   | TESTB of {lsrc:operand, rsrc:operand}
   | BINARY of {binOp:binaryOp, src:operand, dst:operand}
   | MULTDIV of {multDivOp:multDivOp, src:operand}
   | MUL3 of {dst:int, src2:Int32.int option, src1:operand}
   | UNARY of {unOp:unaryOp, opnd:operand}
   | SET of {cond:cond, opnd:operand}
   | CMOV of {cond:cond, src:operand, dst:int}
   | PUSHL of operand
   | PUSHW of operand
   | PUSHB of operand
   | POP of operand
   | CDQ
   | INTO
   | COPY of {dst:int list, src:int list, tmp:operand option}
   | FCOPY of {dst:int list, src:int list, tmp:operand option}
   | FBINARY of {binOp:fbinOp, src:operand, dst:operand}
   | FIBINARY of {binOp:fibinOp, src:operand}
   | FUNARY of funOp
   | FUCOMPP
   | FCOMPP
   | FXCH of {opnd:int}
   | FSTPL of operand
   | FSTPS of operand
   | FSTPT of operand
   | FSTL of operand
   | FSTS of operand
   | FLD1
   | FLDL2E
   | FLDL2T
   | FLDLG2
   | FLDLN2
   | FLDPI
   | FLDZ
   | FLDL of operand
   | FLDS of operand
   | FLDT of operand
   | FILD of operand
   | FILDL of operand
   | FILDLL of operand
   | FNSTSW
   | FENV of {fenvOp:fenvOp, opnd:operand}
   | SAHF
   | ANNOTATION of {i:instruction, a:Annotations.annotation}
   | SOURCE of {}
   | SINK of {}
   | PHI of {}
end

functor X86Instr(structure LabelExp : LABELEXP
                 structure Region   : REGION
                ) : X86INSTR =
struct
   structure C = X86Cells
   structure Region = Region
   structure LabelExp = LabelExp
   structure Constant = LabelExp.Constant
   datatype operand =
     Immed of Int32.int
   | ImmedLabel of LabelExp.labexp
   | Relative of int
   | LabelEA of LabelExp.labexp
   | Direct of int
   | FDirect of int
   | ST of int
   | MemReg of int
   | Displace of {base:int, disp:operand, mem:Region.region}
   | Indexed of {base:int option, index:int, scale:int, disp:operand, mem:Region.region
     }
   type addressing_mode = operand
   type ea = operand
   datatype cond =
     EQ
   | NE
   | LT
   | LE
   | GT
   | GE
   | B
   | BE
   | A
   | AE
   | C
   | NC
   | P
   | NP
   | O
   | NO
   datatype binaryOp =
     ADDL
   | SUBL
   | ANDL
   | ORL
   | XORL
   | SHLL
   | SARL
   | SHRL
   | ADCL
   | SBBL
   | ADDW
   | SUBW
   | ANDW
   | ORW
   | XORW
   | SHLW
   | SARW
   | SHRW
   | ADDB
   | SUBB
   | ANDB
   | ORB
   | XORB
   | SHLB
   | SARB
   | SHRB
   datatype multDivOp =
     MULL
   | IDIVL
   | DIVL
   datatype unaryOp =
     DECL
   | INCL
   | NEGL
   | NOTL
   | NOTW
   | NOTB
   datatype move =
     MOVL
   | MOVB
   | MOVW
   | MOVSWL
   | MOVZWL
   | MOVSBL
   | MOVZBL
   datatype fbinOp =
     FADDP
   | FADDS
   | FMULP
   | FMULS
   | FCOMS
   | FCOMPS
   | FSUBP
   | FSUBS
   | FSUBRP
   | FSUBRS
   | FDIVP
   | FDIVS
   | FDIVRP
   | FDIVRS
   | FADDL
   | FMULL
   | FCOML
   | FCOMPL
   | FSUBL
   | FSUBRL
   | FDIVL
   | FDIVRL
   datatype fibinOp =
     FIADDS
   | FIMULS
   | FICOMS
   | FICOMPS
   | FISUBS
   | FISUBRS
   | FIDIVS
   | FIDIVRS
   | FIADDL
   | FIMULL
   | FICOML
   | FICOMPL
   | FISUBL
   | FISUBRL
   | FIDIVL
   | FIDIVRL
   datatype funOp =
     FABS
   | FCHS
   | FSIN
   | FCOS
   | FTAN
   | FSCALE
   | FRNDINT
   | FSQRT
   | FTST
   | FXAM
   | FINCSTP
   | FDECSTP
   datatype fenvOp =
     FLDENV
   | FNLDENV
   | FSTENV
   | FNSTENV
   datatype instruction =
     NOP
   | JMP of (operand * Label.label list)
   | JCC of {cond:cond, opnd:operand}
   | CALL of (operand * C.cellset * C.cellset * Region.region)
   | ENTER of {src1:operand, src2:operand}
   | LEAVE
   | RET of operand option
   | MOVE of {mvOp:move, src:operand, dst:operand}
   | LEA of {r32:int, addr:operand}
   | CMPL of {lsrc:operand, rsrc:operand}
   | CMPW of {lsrc:operand, rsrc:operand}
   | CMPB of {lsrc:operand, rsrc:operand}
   | TESTL of {lsrc:operand, rsrc:operand}
   | TESTW of {lsrc:operand, rsrc:operand}
   | TESTB of {lsrc:operand, rsrc:operand}
   | BINARY of {binOp:binaryOp, src:operand, dst:operand}
   | MULTDIV of {multDivOp:multDivOp, src:operand}
   | MUL3 of {dst:int, src2:Int32.int option, src1:operand}
   | UNARY of {unOp:unaryOp, opnd:operand}
   | SET of {cond:cond, opnd:operand}
   | CMOV of {cond:cond, src:operand, dst:int}
   | PUSHL of operand
   | PUSHW of operand
   | PUSHB of operand
   | POP of operand
   | CDQ
   | INTO
   | COPY of {dst:int list, src:int list, tmp:operand option}
   | FCOPY of {dst:int list, src:int list, tmp:operand option}
   | FBINARY of {binOp:fbinOp, src:operand, dst:operand}
   | FIBINARY of {binOp:fibinOp, src:operand}
   | FUNARY of funOp
   | FUCOMPP
   | FCOMPP
   | FXCH of {opnd:int}
   | FSTPL of operand
   | FSTPS of operand
   | FSTPT of operand
   | FSTL of operand
   | FSTS of operand
   | FLD1
   | FLDL2E
   | FLDL2T
   | FLDLG2
   | FLDLN2
   | FLDPI
   | FLDZ
   | FLDL of operand
   | FLDS of operand
   | FLDT of operand
   | FILD of operand
   | FILDL of operand
   | FILDLL of operand
   | FNSTSW
   | FENV of {fenvOp:fenvOp, opnd:operand}
   | SAHF
   | ANNOTATION of {i:instruction, a:Annotations.annotation}
   | SOURCE of {}
   | SINK of {}
   | PHI of {}
end

