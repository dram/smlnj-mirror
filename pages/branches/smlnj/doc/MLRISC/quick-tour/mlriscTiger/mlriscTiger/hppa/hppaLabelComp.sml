functor HppaLabelComp
  (structure MLTree : MLTREE
   structure Instr : HPPAINSTR
     sharing Instr.Constant = MLTree.Constant) =
struct
  structure T = MLTree
  structure I = Instr
  structure C = I.C
  structure LE = LabelExp
  structure Imports = HppaImports

  type reduce = 
    { stm: T.stm -> unit, 
      rexp: T.rexp -> int, 
      emit: I.instruction -> unit }

  datatype lab_opnd = OPND of I.operand | REG of int 

  fun error msg = MLRiscErrorMsg.impossible("HppaLabelComp." ^ msg)

  val global = Label.newLabel "$global$"

  fun ldLabelOpnd emit {label, pref} = let
    val rd = case pref of NONE => C.newReg() | SOME reg => reg
  in 
    case label
    of LE.LABEL lab => let
         val offset = LE.MINUS(label, LE.LABEL global)
       in
	 Imports.add(Imports.DATA, global);
	 emit(I.ARITHI{ai=I.ADDIL, i=I.HILabExp(offset, I.R), r=27, t=1});
	 emit(I.LDO{i=I.LOLabExp(offset, I.R), b=1, t=rd});
	 REG rd
       end
     | _ => error "ldLabelOpnd"
    (*esac*)
  end

  fun ldLabelEA emit labexp = 
   (case labexp
    of LE.LABEL label => let
         val offset = LE.MINUS(labexp, LE.LABEL global)
       in
	 Imports.add(Imports.DATA, global);
	 emit(I.ARITHI{ai=I.ADDIL, i=I.HILabExp(offset, I.S), r=27, t=1});
	 (1, I.LOLabExp(offset, I.S))
       end
     | _ => error "ldLabelEA"
    (*esac*))

  fun doCall({stm,rexp,emit}:reduce, T.CALL(exp, def, use)) = let
        val addCCreg = C.addCell C.CC
	fun live([], acc) = acc
	  | live(T.GPR(T.REG r)::regs, acc) = live(regs, C.addReg(r, acc))
	  | live(T.FPR(T.FREG f)::regs, acc) = live(regs, C.addFreg(f, acc))
	  | live(T.CCR(T.CC c)::regs, acc) = live(regs, addCCreg(c, acc))
	  | live(_::regs, acc) = live(regs, acc)
	val returnPtr = 2
	val defs = C.addReg(returnPtr, live(def, C.empty))
	val uses = live(use, C.empty)
      in
	case exp
	of T.LABEL(le as LE.LABEL lab) => 
	    (Imports.add(Imports.CODE, lab);
	     emit(I.BL{defs=defs, uses=uses, x=I.LabExp(le,I.T), 
		       t=returnPtr, n=true}))
	 | _ => error "doCall: exp"
	(*esac*)
      end
    | doCall _ = error "doCall"

  fun doJmp({stm,rexp,emit}:reduce, T.JMP(exp, labs)) =
    (case exp
     of T.LABEL(LE.LABEL lab) => emit(I.B{lab=lab, n=true})
      | T.LABEL _ => error "doJmp: LABEL"
      | _ => emit(I.BV{b=rexp(exp), x=0, labs=labs, n=true})
    (*esac*))
end