(*
 * WARNING: This file was automatically generated by MDLGen (v3.0)
 * from the machine description file "mips/mips.mdl".
 * DO NOT EDIT this file directly
 *)


functor MIPSMCEmitter(structure Instr : MIPSINSTR
                      structure CodeString : CODE_STRING
                     ) : INSTRUCTION_EMITTER =
struct
   structure I = Instr
   structure C = I.C
   structure LabelExp = I.LabelExp
   structure Constant = I.Constant
   structure T = I.T
   structure S = T.Stream
   structure P = S.P
   structure W = Word32
   
   (* MIPS is little endian *)
   
   fun error msg = MLRiscErrorMsg.error("MIPSMC",msg)
   fun makeStream _ =
   let infix && || << >> ~>>
       val op << = W.<<
       val op >> = W.>>
       val op ~>> = W.~>>
       val op || = W.orb
       val op && = W.andb
       val itow = W.fromInt
       fun emit_bool false = 0w0 : W.word
         | emit_bool true = 0w1 : W.word
       val emit_int = itow
       fun emit_word w = w
       fun emit_label l = itow(Label.addrOf l)
       fun emit_labexp le = itow(LabelExp.valueOf le)
       fun emit_const c = itow(Constant.valueOf c)
       val loc = ref 0
   
       (* emit a byte *)
       fun eByte b =
       let val i = !loc in loc := i + 1; CodeString.update(i,b) end
   
       (* emit the low order byte of a word *)
       (* note: fromLargeWord strips the high order bits! *)
       fun eByteW w =
       let val i = !loc
       in loc := i + 1; CodeString.update(i,Word8.fromLargeWord w) end
   
       fun doNothing _ = ()
       fun getAnnotations () = error "getAnnotations"
   
       fun pseudoOp pOp = P.emitValue{pOp=pOp, loc= !loc,emit=eByte}
   
       fun init n = (CodeString.init n; loc := 0)
   
   
   fun eWord32 w = 
       let val b8 = w
           val w = w >> 0wx8
           val b16 = w
           val w = w >> 0wx8
           val b24 = w
           val w = w >> 0wx8
           val b32 = w
       in 
          ( eByteW b8; 
            eByteW b16; 
            eByteW b24; 
            eByteW b32 )
       end
   fun emit_GP r = itow (C.physicalRegisterNum r)
   and emit_FP r = itow (C.physicalRegisterNum r)
   and emit_CC r = itow (C.physicalRegisterNum r)
   and emit_COND r = itow (C.physicalRegisterNum r)
   and emit_HI r = itow (C.physicalRegisterNum r)
   and emit_LO r = itow (C.physicalRegisterNum r)
   and emit_MEM r = itow (C.physicalRegisterNum r)
   and emit_CTRL r = itow (C.physicalRegisterNum r)
   and emit_CELLSET r = itow (C.physicalRegisterNum r)
   fun Load {l, rt, b, offset} = 
       let val rt = emit_GP rt
           val b = emit_GP b
       in eWord32 ((l << 0wx1a) + ((rt << 0wx15) + ((b << 0wx10) + (offset && 0wxffff))))
       end
   and Special {rs, rt, opc} = 
       let val rs = emit_GP rs
           val rt = emit_GP rt
       in eWord32 ((rs << 0wx15) + ((rt << 0wx10) + opc))
       end
       fun emitter instr =
       let
   fun emitInstr (I.NOP) = error "NOP"
     | emitInstr (I.LUI{rt, imm}) = error "LUI"
     | emitInstr (I.LA{rt, b, d}) = error "LA"
     | emitInstr (I.DLA{rt, b, d}) = error "DLA"
     | emitInstr (I.LOAD{l, rt, b, d, mem}) = error "LOAD"
     | emitInstr (I.STORE{s, rs, b, d, mem}) = error "STORE"
     | emitInstr (I.FLOAD{l, ft, b, d, mem}) = error "FLOAD"
     | emitInstr (I.FSTORE{s, fs, b, d, mem}) = error "FSTORE"
     | emitInstr (I.FCMP{fcond, fmt, cc, fs1, fs2}) = error "FCMP"
     | emitInstr (I.TRAP{t, rs, i}) = error "TRAP"
     | emitInstr (I.J{lab, nop}) = error "J"
     | emitInstr (I.JR{rs, labels, nop}) = error "JR"
     | emitInstr (I.JAL{lab, defs, uses, cutsTo, mem, nop}) = error "JAL"
     | emitInstr (I.JALR{rt, rs, defs, uses, cutsTo, mem, nop}) = error "JALR"
     | emitInstr (I.RET{nop}) = error "RET"
     | emitInstr (I.BRANCH{likely, cond, rs, rt, lab, nop}) = error "BRANCH"
     | emitInstr (I.FBRANCH{likely, fbranch, cc, lab, nop}) = error "FBRANCH"
     | emitInstr (I.ARITH{oper, rt, rs, i}) = error "ARITH"
     | emitInstr (I.UNARY{oper, rt, rs}) = error "UNARY"
     | emitInstr (I.MULTIPLY{oper, rt, rs}) = error "MULTIPLY"
     | emitInstr (I.DIVIDE{oper, rt, rs}) = error "DIVIDE"
     | emitInstr (I.MFLO GP) = error "MFLO"
     | emitInstr (I.MTLO GP) = error "MTLO"
     | emitInstr (I.MFHI GP) = error "MFHI"
     | emitInstr (I.MTHI GP) = error "MTHI"
     | emitInstr (I.BREAK int) = error "BREAK"
     | emitInstr (I.FARITH{oper, ft, fs1, fs2}) = error "FARITH"
     | emitInstr (I.FUNARY{oper, ft, fs}) = error "FUNARY"
     | emitInstr (I.FARITH3{oper, ft, fs1, fs2, fs3}) = error "FARITH3"
     | emitInstr (I.FROUND{oper, ft, fs1, rs2}) = error "FROUND"
     | emitInstr (I.CVTI2F{cvt, rs, ft}) = error "CVTI2F"
     | emitInstr (I.CVTF2I{cvt, fs, rt}) = error "CVTF2I"
     | emitInstr (I.COPY{dst, src, impl, tmp}) = error "COPY"
     | emitInstr (I.FCOPY{dst, src, impl, tmp}) = error "FCOPY"
     | emitInstr (I.ANNOTATION{i, a}) = emitInstr i
     | emitInstr (I.PHI{}) = ()
     | emitInstr (I.SOURCE{}) = ()
     | emitInstr (I.SINK{}) = ()
       in
           emitInstr instr
       end
   
   in  S.STREAM{beginCluster=init,
                pseudoOp=pseudoOp,
                emit=emitter,
                endCluster=doNothing,
                defineLabel=doNothing,
                entryLabel=doNothing,
                comment=doNothing,
                exitBlock=doNothing,
                annotation=doNothing,
                getAnnotations=getAnnotations
               }
   end
end

