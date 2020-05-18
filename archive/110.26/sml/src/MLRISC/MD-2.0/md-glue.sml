(*
 * This file just links everything together
 *)

structure MDAstUtil = MDAstUtil(MDAst)

structure MDPP = MDPP(MDAstUtil)

structure MDTypeUtils = MDTypeUtils(MDPP)

structure MDEnv = MDEnv(MDTypeUtils)

structure MDTrans = MDTrans(MDAst)

structure MDCompile = MDCompile
   (structure AstPP   = MDPP
    structure Env     = MDEnv
    structure Trans   = MDTrans
    structure AstUtil = MDAstUtil
   )

structure MDTyping = MDTyping
   (structure Env      = MDEnv
    structure TypeUtil = MDTypeUtils
    structure AstUtil  = MDAstUtil
    structure AstPP    = MDPP
    structure Comp     = MDCompile
   )

structure MDRTL = MDRTL
   (structure Comp = MDCompile
    structure Typing = MDTyping
   )

structure MDBuild = MDBuild(MDPP)

structure MDGen = MDGen
(  structure Comp       = MDCompile
   structure Build      = MDBuild
   structure Cells      = MDCells(MDCompile)
   structure Instr      = MDInstr(MDCompile)
   structure Shuffle    = MDDummyGen(MDCompile)
   structure Asm        = MDAsm(MDCompile)
   structure MC         = MDMC(MDCompile)
   structure RTL        = MDRTL
   structure Dasm       = MDDummyGen(MDCompile)
   structure Props      = MDProps(MDCompile)
   structure Jumps      = MDDummyGen(MDCompile)
   structure RTLProps   = MDRTLProps(MDRTL)
   structure SSAProps   = MDSSAProps(MDRTL)
   structure DelaySlots = MDDelaySlots(MDCompile)
   structure Rewrite    = MDRewrite(MDCompile)
   structure SchedProps = MDSchedProps(MDRTL)
)
