functor HppaLabelComp
  (structure MLTree : MLTREE
   structure Instr : HPPAINSTR
     sharing Instr.Region = MLTree.Region 
     sharing Instr.LabelExp = MLTree.LabelExp) = 
struct
  structure T = MLTree
  structure I = Instr
  structure C = I.C
  structure LE = I.LabelExp

  type ('s,'r,'f,'c) reduce = 
    { stm: ('s,'r,'f,'c) T.stm -> unit, 
      rexp: ('s,'r,'f,'c) T.rexp -> int, 
      emit: I.instruction -> unit }

  fun error msg = ErrorMsg.impossible("HppaLabelComp." ^ msg)

  fun ldLabelOpnd emit {label, pref} = I.LabExp(label,I.T)
  fun ldLabelEA emit lexp = (0, I.LabExp(lexp, I.T))

  (* should change the return pointer to 2 to follow HPUX conventions *)
  fun doCall({stm,rexp,emit}:('s,'r,'f,'c) reduce, 
             T.CALL(exp, flow, def, use, cdef, cuse, mem)) = 
  let
        val addCCreg = C.addCell C.CC
	fun live([], acc) = acc
	  | live(T.GPR(T.REG(_, r))::regs, acc) = live(regs, C.addReg(r, acc))
	  | live(T.FPR(T.FREG(_, f))::regs, acc) = live(regs, C.addFreg(f, acc))
	  | live(T.CCR(T.CC(_,c))::regs, acc) = live(regs, addCCreg(c, acc))
	  | live(T.CCR(T.FCC(_,c))::regs, acc) = live(regs, addCCreg(c, acc))
	  | live(_::regs, acc) = live(regs, acc)
	val returnPtr = 31
	val defs = C.addReg(returnPtr, live(def, C.empty))
	val uses = live(use, C.empty)
      in emit(I.BLE{b=rexp exp, d=I.IMMED 0, sr=5, t=returnPtr, 
                    defs=defs, uses=uses, mem=mem})
      end
    | doCall _ = error "doCall"

  fun doJmp({stm,rexp,emit}:('s,'r,'f,'c) reduce, T.JMP(ctrl, exp, labs)) =
    (case exp
     of T.LABEL(LE.LABEL lab) => emit(I.B{lab=lab,n=true})
      | T.LABEL _ => error "doJmp: LABEL"
      | _ => emit(I.BV{b=rexp(exp), x=0, labs=labs, n=true})
    (*esac*))

end
		      
