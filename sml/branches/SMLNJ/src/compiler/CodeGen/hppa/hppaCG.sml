(*
 * Hppa specific backend
 *)
structure HppaCG = 
  MachineGen
  ( structure MachSpec   = HppaSpec
    structure PseudoOps  = HppaPseudoOps
    structure CpsRegs    = HppaCpsRegs
    structure InsnProps  = HppaProps(HppaInstr)
    structure Asm        = HppaAsmEmitter

    structure HppaMillicode =
      HppaMillicode(structure MLTree=HppaMLTree
                    structure Instr=HppaInstr)

    structure HppaLabelComp =
      HppaLabelComp(structure MLTree=HppaMLTree
                    structure Instr=HppaInstr)

    structure MLTreeComp=
       Hppa(structure HppaInstr = HppaInstr
            structure HppaMLTree = HppaMLTree
            structure MilliCode=HppaMillicode
            structure LabelComp=HppaLabelComp
            val costOfMultiply = ref 7
            val costOfDivision = ref 7
           )

    structure HppaJumps =
       HppaJumps(structure Instr=HppaInstr
                 structure Shuffle=HppaShuffle)

    structure BackPatch =
       SpanDependencyResolution
         (structure Flowgraph = HppaFlowGraph
          structure Jumps     = HppaJumps
          structure Emitter   = HppaMCEmitter
          structure DelaySlot = HppaDelaySlots
             (structure I=HppaInstr
              structure P=InsnProps)
          structure Props = InsnProps
         )

    structure RA = 
       RegAlloc
         (structure I         = HppaInstr
          structure MachSpec  = HppaSpec
          structure Flowgraph = HppaFlowGraph
          structure CpsRegs   = HppaCpsRegs
          structure InsnProps = InsnProps 
          structure Rewrite   = HppaRewrite(HppaInstr) 
          structure Asm       = HppaAsmEmitter
          functor Ra = HppaRegAlloc 

          (* NOTE: the spill offset grows backwards on the stack! 
           *)

          val sp        = I.C.stackptrR
          val stack     = I.Region.stack
          val tmpR      = I.C.asmTmpR
          val itow      = Word.fromInt
          val wtoi      = Word.toIntX
          fun low11(n)  = wtoi(Word.andb(itow n, 0wx7ff))
          fun high21(n) = wtoi(Word.~>>(itow n, 0w11))
 
          (* make copy *) 
          fun copyR((rds as [_], rss as [_]), _) =
              I.COPY{dst=rds, src=rss, impl=ref NONE, tmp=NONE}
            | copyR((rds, rss), I.COPY{tmp, ...}) =
              I.COPY{dst=rds, src=rss, impl=ref NONE, tmp=tmp}
          fun copyF((fds as [_], fss as [_]), _) =
              I.FCOPY{dst=fds, src=fss, impl=ref NONE, tmp=NONE}
            | copyF((fds, fss), I.FCOPY{tmp, ...}) =
              I.FCOPY{dst=fds, src=fss, impl=ref NONE, tmp=tmp}

          (* spill copy temp *) 
          fun spillCopyTmp(I.COPY{dst,src,tmp,impl},offset) =
              I.COPY{dst=dst, src=src, impl=impl,
                     tmp=SOME(I.Displace{base=sp, disp= ~offset})}
          fun spillFcopyTmp(I.FCOPY{dst,src,tmp,impl},offset) =
              I.FCOPY{dst=dst, src=src, impl=impl,
                     tmp=SOME(I.Displace{base=sp, disp= ~offset})}

          (* spill register *) 
          fun spillInstrR(r,offset) =
              [I.STORE{st=I.STW, b=sp, d=I.IMMED(~offset), r=r, mem=stack}]
          fun spillInstrF(r,offset) =
              [I.LDIL{i=I.IMMED(high21(~offset)), t=tmpR},
               I.LDO{i=I.IMMED(low11(~offset)), b=tmpR, t=tmpR},
               I.FSTOREX{fstx=I.FSTDX, b=sp, x=tmpR, r=r, mem=stack}
              ]

          (* reload register *) 
          fun reloadInstrR(t,offset,rest) =
              I.LOADI{li=I.LDW, i=I.IMMED(~offset), r=sp, t=t, mem=stack}::rest
          fun reloadInstrF(t,offset,rest) =
              I.LDIL{i=I.IMMED(high21(~offset)), t=tmpR} ::
              I.LDO{i=I.IMMED(low11(~offset)), b=tmpR, t=tmpR} ::
              I.FLOADX{flx=I.FLDDX, b=sp, x=tmpR, t=t, mem=stack} :: rest
         )
  )
