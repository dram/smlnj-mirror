functor SparcDelaySlots
   (structure I : SPARCINSTR
    structure P : INSN_PROPERTIES where I = I
    (* sharing/defn conflict:   sharing P.I = I*)
   ) : DELAY_SLOT_PROPERTIES =
struct
   structure I  = I
   structure C  = I.C
   structure SL = C.SortedCells

   fun error msg = MLRiscErrorMsg.error("SparcDelaySlotProps",msg)

   datatype delay_slot = D_NONE | D_ERROR | D_ALWAYS | D_TAKEN | D_FALLTHRU

   val delaySlotSize = 4

   fun delaySlot{instr, backward} =
     case instr of
       I.CALL{nop,...} => {n=false,nOn=D_ERROR,nOff=D_ALWAYS,nop=nop}
     | I.JMP{nop,...}  => {n=false,nOn=D_ERROR,nOff=D_ALWAYS,nop=nop}
     | I.JMPL{nop,...} => {n=false,nOn=D_ERROR,nOff=D_ALWAYS,nop=nop}
     | I.RET{nop,...}  => {n=false,nOn=D_ERROR,nOff=D_ALWAYS,nop=nop}
     | I.Bicc{b=I.BA,a,nop,...} => {n=false,nOn=D_NONE,nOff=D_ALWAYS,nop=nop}
     | I.Bicc{a,nop,...} => {n=a,nOn=D_TAKEN,nOff=D_ALWAYS,nop=nop}
     | I.FBfcc{a,nop,...} => {n=a,nOn=D_TAKEN,nOff=D_ALWAYS,nop=nop}
     | I.BR{a,nop,...} => {n=a,nOn=D_TAKEN,nOff=D_ALWAYS,nop=nop}
     | I.BP{a,nop,...} => {n=a,nOn=D_TAKEN,nOff=D_ALWAYS,nop=nop}
     | I.FCMP{nop,...} => {n=false,nOn=D_ERROR,nOff=D_ALWAYS,nop=nop}
     | I.ANNOTATION{i,...} => delaySlot{instr=i,backward=backward}
     | _ => {n=false,nOn=D_ERROR,nOff=D_NONE,nop=false}

   fun enableDelaySlot{instr, n, nop} =
       case (instr,n) of
         (I.CALL{defs,uses,label,cutsTo,mem,...},false) => 
	    I.CALL{defs=defs,uses=uses,label=label,cutsTo=cutsTo,
                   nop=nop,mem=mem}
       | (I.JMPL{r,i,d,defs,uses,mem,cutsTo,...},false) => 
	    I.JMPL{r=r,i=i,d=d,defs=defs,uses=uses,cutsTo=cutsTo,
                   nop=nop,mem=mem}
       | (I.JMP{r,i,labs,...},false) => 
	    I.JMP{r=r,i=i,labs=labs,nop=nop}
       | (I.RET{leaf,...},false) => I.RET{leaf=leaf,nop=nop}
       | (I.Bicc{b,a,label,...},_) => I.Bicc{b=b,a=n,nop=nop,label=label}
       | (I.FBfcc{b,a,label,...},_) => I.FBfcc{b=b,a=n,nop=nop,label=label}
       | (I.BR{nop,label,p,r,rcond,...},_) =>
            I.BR{rcond=rcond,r=r,a=n,nop=nop,label=label,p=p}
       | (I.BP{nop,label,p,cc,b,...},_) =>
            I.BP{b=b,cc=cc,a=n,nop=nop,label=label,p=p}
       | (I.FCMP{cmp,r1,r2,...},false) => I.FCMP{cmp=cmp,r1=r1,r2=r2,nop=nop}
       | (I.ANNOTATION{i,a},n) => 
           I.ANNOTATION{i=enableDelaySlot{instr=i,n=n,nop=nop},a=a}
       | _ => error "enableDelaySlot"

    val defUseI = P.defUse C.GP
    val defUseF = P.defUse C.FP
    val psr     = [C.psr] 
    val fsr     = [C.fsr]
    val y       = [C.y]
    val zeroR   = Option.valOf(C.zeroReg C.GP)
    val everything = [C.y,C.psr,C.fsr]
    fun conflict{src=i,dst=j} = 
        let fun cc I.ANDCC  = true
              | cc I.ANDNCC = true
              | cc I.ORCC   = true
              | cc I.ORNCC  = true
              | cc I.XORCC  = true
              | cc I.XNORCC = true
              | cc I.ADDCC  = true
              | cc I.TADDCC  = true
              | cc I.TADDTVCC = true
              | cc I.SUBCC = true
              | cc I.TSUBCC = true
              | cc I.TSUBTVCC= true
              | cc I.UMULCC = true
              | cc I.SMULCC = true
              | cc I.UDIVCC = true
              | cc I.SDIVCC = true
              | cc _ = false
            fun defUseOther(I.Ticc _) = ([],psr)
              | defUseOther(I.ARITH{a,...}) = 
                  if cc a then (psr,[]) else ([],[])
              | defUseOther(I.WRY _) = (y,[])
              | defUseOther(I.RDY _) = ([],y)
              | defUseOther(I.FCMP _) = (fsr,[])
              | defUseOther(I.Bicc{b=I.BA,...}) = ([],[])
              | defUseOther(I.Bicc _) = ([],psr)
              | defUseOther(I.FBfcc _) = ([],fsr)
              | defUseOther(I.MOVicc _) = ([],psr)
              | defUseOther(I.MOVfcc _) = ([],fsr)
              | defUseOther(I.FMOVicc _) = ([],psr)
              | defUseOther(I.FMOVfcc _) = ([],fsr)
              | defUseOther(I.CALL _) = (everything,[])
              | defUseOther(I.JMPL _) = (everything,[])
              | defUseOther(I.ANNOTATION{i,...}) = defUseOther i
              | defUseOther _ = ([],[])
            fun clash(defUse) =
                let val (di,ui) = defUse i
                    val (dj,uj) = defUse j
                in  SL.nonEmptyIntersection(di,uj) orelse
                    SL.nonEmptyIntersection(di,dj) orelse
                    SL.nonEmptyIntersection(ui,dj) 
                end
            fun toSL f i = let val (d,u) = f i
                           in  (SL.uniq d, SL.uniq u) end
            fun defUseInt i = 
                let val (d,u) = defUseI i
                    val d     = SL.uniq d
                    val u     = SL.uniq u
                    (* no dependence on register 0! *) 
                in  (SL.rmv(zeroR,d), SL.rmv(zeroR,u)) end
        in  clash(defUseInt) orelse 
            clash(toSL defUseF) orelse
            clash(toSL defUseOther)
        end

    fun delaySlotCandidate{jmp,delaySlot=
                         (I.CALL _ | I.Bicc _ | I.FBfcc _ | I.Ticc _ | I.BR _ 
                         | I.JMP _ | I.JMPL _ | I.RET _ | I.BP _ )} = false
      | delaySlotCandidate{jmp=I.FCMP _,delaySlot=I.FCMP _} = false
      | delaySlotCandidate{jmp=I.ANNOTATION{i,...},delaySlot} = 
           delaySlotCandidate{jmp=i,delaySlot=delaySlot}
      | delaySlotCandidate{jmp,delaySlot=I.ANNOTATION{i,...}} = 
           delaySlotCandidate{jmp=jmp,delaySlot=i}
      | delaySlotCandidate _ = true

   fun setTarget(I.Bicc{b,a,nop,...},lab) = I.Bicc{b=b,a=a,nop=nop,label=lab}
     | setTarget(I.FBfcc{b,a,nop,...},lab) = I.FBfcc{b=b,a=a,nop=nop,label=lab}
     | setTarget(I.BR{rcond,p,r,a,nop,...},lab) = 
          I.BR{rcond=rcond,p=p,r=r,a=a,nop=nop,label=lab}
     | setTarget(I.BP{b,p,cc,a,nop,...},lab) = 
          I.BP{b=b,p=p,cc=cc,a=a,nop=nop,label=lab}
     | setTarget(I.ANNOTATION{i,a},lab) = I.ANNOTATION{i=setTarget(i,lab),a=a}
     | setTarget _ = error "setTarget"

end
