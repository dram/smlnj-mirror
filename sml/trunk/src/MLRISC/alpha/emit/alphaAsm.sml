(*
 * This file was automatically generated by MDGen (v2.0)
 * from the machine description file "alpha/alpha.md".
 *)


functor AlphaAsmEmitter(structure Instr : ALPHAINSTR
                        structure Stream : INSTRUCTION_STREAM
                        structure Shuffle : ALPHASHUFFLE
                           where I = Instr
                       ) : INSTRUCTION_EMITTER =
struct
   structure I  = Instr
   structure C  = I.C
   structure S  = Stream
   structure P  = S.P
   structure LabelExp = I.LabelExp
   structure Constant = I.Constant
   
   val show_cellset = MLRiscControl.getFlag "asm-show-cellset"
   val show_region  = MLRiscControl.getFlag "asm-show-region"
   val indent_copies = MLRiscControl.getFlag "asm-indent-copies"
   
   fun error msg = MLRiscErrorMsg.error("AlphaAsmEmitter",msg)
   
   fun makeStream formatAnnotations =
   let val stream = !AsmStream.asmOutStream
       fun emit' s = TextIO.output(stream,s)
       val newline = ref true
       val tabs = ref 0
       fun tabbing 0 = ()
         | tabbing n = (emit' "\t"; tabbing(n-1))
       fun emit s = (tabbing(!tabs); tabs := 0; newline := false; emit' s)
       fun nl() = (tabs := 0; if !newline then () else (newline := true; emit' "\n"))
       fun comma() = emit ","
       fun tab() = tabs := 1
       fun indent() = tabs := 2
       fun ms n = let val s = Int.toString n
                  in  if n<0 then "-"^String.substring(s,1,size s-1)
                      else s
                  end
       fun emit_label lab = emit(Label.nameOf lab)
       fun emit_labexp le = emit(LabelExp.toString le)
       fun emit_const c = emit(Constant.toString c)
       fun emit_int i = emit(ms i)
       fun paren f = (emit "("; f(); emit ")")
       fun defineLabel lab = emit(Label.nameOf lab^":\n")
       fun entryLabel lab = defineLabel lab
       fun comment msg = (tab(); emit("/* " ^ msg ^ " */"))
       fun annotation a = (comment(Annotations.toString a); nl())
       fun doNothing _ = ()
       fun emit_region mem = comment(I.Region.toString mem)
       val emit_region = 
          if !show_region then emit_region else doNothing
       fun pseudoOp pOp = emit(P.toString pOp)
       fun init size = (comment("Code Size = " ^ ms size); nl())
       fun emitter regmap =
       let
           val emitRegInfo = AsmFormatUtil.reginfo
                                (emit,regmap,formatAnnotations)
   fun emit_GP r = 
       ( emit (C.showGP (regmap r)); 
       emitRegInfo r )
   and emit_FP r = 
       ( emit (C.showFP (regmap r)); 
       emitRegInfo r )
   and emit_CC r = 
       ( emit (C.showCC (regmap r)); 
       emitRegInfo r )
   and emit_MEM r = 
       ( emit (C.showMEM (regmap r)); 
       emitRegInfo r )
   and emit_CTRL r = 
       ( emit (C.showCTRL (regmap r)); 
       emitRegInfo r )
   
       fun emit_cellset(title,cellset) =
         (nl(); comment(title^C.cellsetToString' regmap cellset))
       val emit_cellset = 
         if !show_cellset then emit_cellset else doNothing
       fun emit_defs cellset = emit_cellset("defs: ",cellset)
       fun emit_uses cellset = emit_cellset("uses: ",cellset)
   fun emit_operand (I.REGop GP) = emit_GP GP
     | emit_operand (I.IMMop int) = emit_int int
     | emit_operand (I.HILABop labexp) = 
       ( emit "hi("; 
       emit_labexp labexp; 
       emit ")" )
     | emit_operand (I.LOLABop labexp) = 
       ( emit "lo("; 
       emit_labexp labexp; 
       emit ")" )
     | emit_operand (I.LABop labexp) = emit_labexp labexp
   and asm_branch (I.BR) = "br"
     | asm_branch (I.BLBC) = "blbc"
     | asm_branch (I.BEQ) = "beq"
     | asm_branch (I.BLT) = "blt"
     | asm_branch (I.BLE) = "ble"
     | asm_branch (I.BLBS) = "blbs"
     | asm_branch (I.BNE) = "bne"
     | asm_branch (I.BGE) = "bge"
     | asm_branch (I.BGT) = "bgt"
   and emit_branch x = emit (asm_branch x)
   and asm_fbranch (I.FBEQ) = "fbeq"
     | asm_fbranch (I.FBLT) = "fblt"
     | asm_fbranch (I.FBLE) = "fble"
     | asm_fbranch (I.FBNE) = "fbne"
     | asm_fbranch (I.FBGE) = "fbge"
     | asm_fbranch (I.FBGT) = "fbgt"
   and emit_fbranch x = emit (asm_fbranch x)
   and asm_load (I.LDB) = "ldb"
     | asm_load (I.LDW) = "ldw"
     | asm_load (I.LDBU) = "ldbu"
     | asm_load (I.LDWU) = "ldwu"
     | asm_load (I.LDL) = "ldl"
     | asm_load (I.LDL_L) = "ldl_l"
     | asm_load (I.LDQ) = "ldq"
     | asm_load (I.LDQ_L) = "ldq_l"
     | asm_load (I.LDQ_U) = "ldq_u"
   and emit_load x = emit (asm_load x)
   and asm_store (I.STB) = "stb"
     | asm_store (I.STW) = "stw"
     | asm_store (I.STL) = "stl"
     | asm_store (I.STQ) = "stq"
     | asm_store (I.STQ_U) = "stq_u"
   and emit_store x = emit (asm_store x)
   and asm_fload (I.LDF) = "ldf"
     | asm_fload (I.LDG) = "ldg"
     | asm_fload (I.LDS) = "lds"
     | asm_fload (I.LDT) = "ldt"
   and emit_fload x = emit (asm_fload x)
   and asm_fstore (I.STF) = "stf"
     | asm_fstore (I.STG) = "stg"
     | asm_fstore (I.STS) = "sts"
     | asm_fstore (I.STT) = "stt"
   and emit_fstore x = emit (asm_fstore x)
   and asm_operate (I.ADDL) = "addl"
     | asm_operate (I.ADDQ) = "addq"
     | asm_operate (I.CMPBGE) = "cmpbge"
     | asm_operate (I.CMPEQ) = "cmpeq"
     | asm_operate (I.CMPLE) = "cmple"
     | asm_operate (I.CMPLT) = "cmplt"
     | asm_operate (I.CMPULE) = "cmpule"
     | asm_operate (I.CMPULT) = "cmpult"
     | asm_operate (I.SUBL) = "subl"
     | asm_operate (I.SUBQ) = "subq"
     | asm_operate (I.S4ADDL) = "s4addl"
     | asm_operate (I.S4ADDQ) = "s4addq"
     | asm_operate (I.S4SUBL) = "s4subl"
     | asm_operate (I.S4SUBQ) = "s4subq"
     | asm_operate (I.S8ADDL) = "s8addl"
     | asm_operate (I.S8ADDQ) = "s8addq"
     | asm_operate (I.S8SUBL) = "s8subl"
     | asm_operate (I.S8SUBQ) = "s8subq"
     | asm_operate (I.AND) = "and"
     | asm_operate (I.BIC) = "bic"
     | asm_operate (I.BIS) = "bis"
     | asm_operate (I.EQV) = "eqv"
     | asm_operate (I.ORNOT) = "ornot"
     | asm_operate (I.XOR) = "xor"
     | asm_operate (I.EXTBL) = "extbl"
     | asm_operate (I.EXTLH) = "extlh"
     | asm_operate (I.EXTLL) = "extll"
     | asm_operate (I.EXTQH) = "extqh"
     | asm_operate (I.EXTQL) = "extql"
     | asm_operate (I.EXTWH) = "extwh"
     | asm_operate (I.EXTWL) = "extwl"
     | asm_operate (I.INSBL) = "insbl"
     | asm_operate (I.INSLH) = "inslh"
     | asm_operate (I.INSLL) = "insll"
     | asm_operate (I.INSQH) = "insqh"
     | asm_operate (I.INSQL) = "insql"
     | asm_operate (I.INSWH) = "inswh"
     | asm_operate (I.INSWL) = "inswl"
     | asm_operate (I.MSKBL) = "mskbl"
     | asm_operate (I.MSKLH) = "msklh"
     | asm_operate (I.MSKLL) = "mskll"
     | asm_operate (I.MSKQH) = "mskqh"
     | asm_operate (I.MSKQL) = "mskql"
     | asm_operate (I.MSKWH) = "mskwh"
     | asm_operate (I.MSKWL) = "mskwl"
     | asm_operate (I.SLL) = "sll"
     | asm_operate (I.SRA) = "sra"
     | asm_operate (I.SRL) = "srl"
     | asm_operate (I.ZAP) = "zap"
     | asm_operate (I.ZAPNOT) = "zapnot"
     | asm_operate (I.MULL) = "mull"
     | asm_operate (I.MULQ) = "mulq"
     | asm_operate (I.UMULH) = "umulh"
     | asm_operate (I.SGNXL) = "addl"
   and emit_operate x = emit (asm_operate x)
   and asm_cmove (I.CMOVEQ) = "cmoveq"
     | asm_cmove (I.CMOVLBC) = "cmovlbc"
     | asm_cmove (I.CMOVLBS) = "cmovlbs"
     | asm_cmove (I.CMOVGE) = "cmovge"
     | asm_cmove (I.CMOVGT) = "cmovgt"
     | asm_cmove (I.CMOVLE) = "cmovle"
     | asm_cmove (I.CMOVLT) = "cmovlt"
     | asm_cmove (I.CMOVNE) = "cmovne"
   and emit_cmove x = emit (asm_cmove x)
   and asm_pseudo_op (I.DIVL) = "divl"
     | asm_pseudo_op (I.DIVLU) = "divlu"
     | asm_pseudo_op (I.DIVQ) = "divq"
     | asm_pseudo_op (I.DIVQU) = "divqu"
     | asm_pseudo_op (I.REML) = "reml"
     | asm_pseudo_op (I.REMLU) = "remlu"
     | asm_pseudo_op (I.REMQ) = "remq"
     | asm_pseudo_op (I.REMQU) = "remqu"
   and emit_pseudo_op x = emit (asm_pseudo_op x)
   and asm_operateV (I.ADDLV) = "addlv"
     | asm_operateV (I.ADDQV) = "addqv"
     | asm_operateV (I.SUBLV) = "sublv"
     | asm_operateV (I.SUBQV) = "subqv"
     | asm_operateV (I.MULLV) = "mullv"
     | asm_operateV (I.MULQV) = "mulqv"
   and emit_operateV x = emit (asm_operateV x)
   and asm_funary (I.CVTLQ) = "cvtlq"
     | asm_funary (I.CVTQL) = "cvtql"
     | asm_funary (I.CVTQLSV) = "cvtqlsv"
     | asm_funary (I.CVTQLV) = "cvtqlv"
     | asm_funary (I.CVTQS) = "cvtqs"
     | asm_funary (I.CVTQSC) = "cvtqsc"
     | asm_funary (I.CVTQT) = "cvtqt"
     | asm_funary (I.CVTQTC) = "cvtqtc"
     | asm_funary (I.CVTTS) = "cvtts"
     | asm_funary (I.CVTTSC) = "cvttsc"
     | asm_funary (I.CVTST) = "cvtst"
     | asm_funary (I.CVTSTS) = "cvtsts"
     | asm_funary (I.CVTTQ) = "cvttq"
     | asm_funary (I.CVTTQC) = "cvttqc"
   and emit_funary x = emit (asm_funary x)
   and asm_foperate (I.CPYS) = "cpys"
     | asm_foperate (I.CPYSE) = "cpyse"
     | asm_foperate (I.CPYSN) = "cpysn"
     | asm_foperate (I.MF_FPCR) = "mf_fpcr"
     | asm_foperate (I.MT_FPCR) = "mt_fpcr"
     | asm_foperate (I.CMPTEQ) = "cmpteq"
     | asm_foperate (I.CMPTLT) = "cmptlt"
     | asm_foperate (I.CMPTLE) = "cmptle"
     | asm_foperate (I.CMPTUN) = "cmptun"
     | asm_foperate (I.CMPTEQSU) = "cmpteqsu"
     | asm_foperate (I.CMPTLTSU) = "cmptltsu"
     | asm_foperate (I.CMPTLESU) = "cmptlesu"
     | asm_foperate (I.CMPTUNSU) = "cmptunsu"
     | asm_foperate (I.ADDS) = "adds"
     | asm_foperate (I.ADDT) = "addt"
     | asm_foperate (I.DIVS) = "divs"
     | asm_foperate (I.DIVT) = "divt"
     | asm_foperate (I.MULS) = "muls"
     | asm_foperate (I.MULT) = "mult"
     | asm_foperate (I.SUBS) = "subs"
     | asm_foperate (I.SUBT) = "subt"
   and emit_foperate x = emit (asm_foperate x)
   and asm_fcmove (I.FCMOVEQ) = "fcmoveq"
     | asm_fcmove (I.FCMOVGE) = "fcmovge"
     | asm_fcmove (I.FCMOVGT) = "fcmovgt"
     | asm_fcmove (I.FCMOVLE) = "fcmovle"
     | asm_fcmove (I.FCMOVLT) = "fcmovlt"
     | asm_fcmove (I.FCMOVNE) = "fcmovne"
   and emit_fcmove x = emit (asm_fcmove x)
   and asm_foperateV (I.ADDSSUD) = "addssud"
     | asm_foperateV (I.ADDSSU) = "addssu"
     | asm_foperateV (I.ADDTSUD) = "addtsud"
     | asm_foperateV (I.ADDTSU) = "addtsu"
     | asm_foperateV (I.DIVSSUD) = "divssud"
     | asm_foperateV (I.DIVSSU) = "divssu"
     | asm_foperateV (I.DIVTSUD) = "divtsud"
     | asm_foperateV (I.DIVTSU) = "divtsu"
     | asm_foperateV (I.MULSSUD) = "mulssud"
     | asm_foperateV (I.MULSSU) = "mulssu"
     | asm_foperateV (I.MULTSUD) = "multsud"
     | asm_foperateV (I.MULTSU) = "multsu"
     | asm_foperateV (I.SUBSSUD) = "subssud"
     | asm_foperateV (I.SUBSSU) = "subssu"
     | asm_foperateV (I.SUBTSUD) = "subtsud"
     | asm_foperateV (I.SUBTSU) = "subtsu"
   and emit_foperateV x = emit (asm_foperateV x)
   and asm_osf_user_palcode (I.BPT) = "bpt"
     | asm_osf_user_palcode (I.BUGCHK) = "bugchk"
     | asm_osf_user_palcode (I.CALLSYS) = "callsys"
     | asm_osf_user_palcode (I.GENTRAP) = "gentrap"
     | asm_osf_user_palcode (I.IMB) = "imb"
     | asm_osf_user_palcode (I.RDUNIQUE) = "rdunique"
     | asm_osf_user_palcode (I.WRUNIQUE) = "wrunique"
   and emit_osf_user_palcode x = emit (asm_osf_user_palcode x)

(*#line 470.7 "alpha/alpha.md"*)
   fun isZero (I.LABop le) = (LabelExp.valueOf le) = 0
     | isZero _ = false
   fun emitInstr' instr = 
       (
        case instr of
        I.DEFFREG FP => 
        ( emit "/* deffreg\t"; 
        emit_FP FP; 
        emit " */" )
      | I.LDA{r, b, d} => (if ((isZero d) andalso (r = b))
           then ()
           else 
           (
           ( emit "lda\t"; 
           emit_GP r; 
           emit ", "; 
           emit_operand d ); 
           (if (b = 31)
              then ()
              else 
              ( emit "("; 
              emit_GP b; 
              emit ")" ))))
      | I.LDAH{r, b, d} => 
        (
        ( emit "ldah\t"; 
        emit_GP r; 
        emit ", "; 
        emit_operand d ); 
        (if (b = 31)
           then ()
           else 
           ( emit "("; 
           emit_GP b; 
           emit ")" )))
      | I.LOAD{ldOp, r, b, d, mem} => 
        ( emit_load ldOp; 
        emit "\t"; 
        emit_GP r; 
        emit ", "; 
        emit_operand d; 
        emit "("; 
        emit_GP b; 
        emit ")"; 
        emit_region mem )
      | I.STORE{stOp, r, b, d, mem} => 
        ( emit_store stOp; 
        emit "\t"; 
        emit_GP r; 
        emit ", "; 
        emit_operand d; 
        emit "("; 
        emit_GP b; 
        emit ")"; 
        emit_region mem )
      | I.FLOAD{ldOp, r, b, d, mem} => 
        ( emit_fload ldOp; 
        emit "\t"; 
        emit_FP r; 
        emit ", "; 
        emit_operand d; 
        emit "("; 
        emit_GP b; 
        emit ")"; 
        emit_region mem )
      | I.FSTORE{stOp, r, b, d, mem} => 
        ( emit_fstore stOp; 
        emit "\t"; 
        emit_FP r; 
        emit ", "; 
        emit_operand d; 
        emit "("; 
        emit_GP b; 
        emit ")"; 
        emit_region mem )
      | I.JMPL({r, b, d}, label) => 
        ( emit "jmp\t"; 
        emit_GP r; 
        emit ", ("; 
        emit_GP b; 
        emit ")" )
      | I.JSR{r, b, d, defs, uses, mem} => 
        ( emit "jsr\t"; 
        emit_GP r; 
        emit ", ("; 
        emit_GP b; 
        emit ")"; 
        emit_region mem; 
        emit_defs defs; 
        emit_uses uses )
      | I.BSR{r, lab, defs, uses, mem} => 
        ( emit "bsr\t"; 
        emit_GP r; 
        emit ", "; 
        emit_label lab; 
        emit_region mem; 
        emit_defs defs; 
        emit_uses uses )
      | I.RET{r, b, d} => 
        ( emit "ret\t"; 
        emit_GP r; 
        emit ", ("; 
        emit_GP b; 
        emit ")" )
      | I.BRANCH{b, r, lab} => 
        ( emit_branch b; 
        emit "\t"; 
        emit_GP r; 
        emit ", "; 
        emit_label lab )
      | I.FBRANCH{b, f, lab} => 
        ( emit_fbranch b; 
        emit "\t"; 
        emit_FP f; 
        emit ", "; 
        emit_label lab )
      | I.OPERATE{oper, ra, rb, rc} => 
        (
         case (oper, ra, rb, rc) of
         (I.BIS, 27, I.REGop 31, 29) => emit "ldgp\t$29, 0($27)"
       | (I.BIS, 26, I.REGop 31, 29) => emit "ldgp\t$29, 0($26)"
       | _ => 
         ( emit_operate oper; 
         emit "\t"; 
         emit_GP ra; 
         emit ", "; 
         emit_operand rb; 
         emit ", "; 
         emit_GP rc )
        )
      | I.OPERATEV{oper, ra, rb, rc} => 
        ( emit_operateV oper; 
        emit "\t"; 
        emit_GP ra; 
        emit ", "; 
        emit_operand rb; 
        emit ", "; 
        emit_GP rc )
      | I.CMOVE{oper, ra, rb, rc} => 
        ( emit_cmove oper; 
        emit "\t"; 
        emit_GP ra; 
        emit ", "; 
        emit_operand rb; 
        emit ", "; 
        emit_GP rc )
      | I.PSEUDOARITH{oper, ra, rb, rc, tmps} => 
        ( emit_pseudo_op oper; 
        emit "\t"; 
        emit_GP ra; 
        emit ", "; 
        emit_operand rb; 
        emit ", "; 
        emit_GP rc; 
        emit_cellset ("tmps", tmps))
      | I.COPY{dst, src, impl, tmp} => emitInstrs (Shuffle.shuffle {regmap=regmap, tmp=tmp, dst=dst, src=src})
      | I.FCOPY{dst, src, impl, tmp} => emitInstrs (Shuffle.shufflefp {regmap=regmap, tmp=tmp, dst=dst, src=src})
      | I.FUNARY{oper, fb, fc} => 
        ( emit_funary oper; 
        emit "\t"; 
        emit_FP fb; 
        emit ", "; 
        emit_FP fc )
      | I.FOPERATE{oper, fa, fb, fc} => 
        ( emit_foperate oper; 
        emit "\t"; 
        emit_FP fa; 
        emit ", "; 
        emit_FP fb; 
        emit ", "; 
        emit_FP fc )
      | I.FOPERATEV{oper, fa, fb, fc} => 
        ( emit_foperateV oper; 
        emit "\t"; 
        emit_FP fa; 
        emit ", "; 
        emit_FP fb; 
        emit ", "; 
        emit_FP fc )
      | I.FCMOVE{oper, fa, fb, fc} => 
        ( emit_fcmove oper; 
        emit "\t"; 
        emit_FP fa; 
        emit ", "; 
        emit_FP fb; 
        emit ", "; 
        emit_FP fc )
      | I.TRAPB => emit "trapb"
      | I.CALL_PAL{code, def, use} => 
        ( emit "call_pal "; 
        emit_osf_user_palcode code )
      | I.ANNOTATION{i, a} => 
        ( comment (Annotations.toString a); 
        nl (); 
        emitInstr i )
      | I.SOURCE{} => emit "source"
      | I.SINK{} => emit "sink"
      | I.PHI{} => emit "phi"
       )
          and emitInstr i = (tab(); emitInstr' i; nl())
          and emitInstrIndented i = (indent(); emitInstr' i; nl())
          and emitInstrs instrs =
           app (if !indent_copies then emitInstrIndented
                else emitInstr) instrs
      in  emitInstr end
   
   in  S.STREAM{beginCluster=init,
                pseudoOp=pseudoOp,
                emit=emitter,
                endCluster=doNothing,
                defineLabel=defineLabel,
                entryLabel=entryLabel,
                comment=comment,
                exitBlock=doNothing,
                annotation=annotation,
                phi=doNothing,
                alias=doNothing
               }
   end
end

