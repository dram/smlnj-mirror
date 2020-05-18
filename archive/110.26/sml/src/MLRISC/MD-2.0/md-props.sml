(*
 * Generate the <arch>Props functor.
 * This structure extracts information about the instruction set.
 *)

functor MDProps(Comp : MD_COMPILE) : MD_GEN_MODULE =
struct

   structure Ast  = Comp.Ast
   structure Comp = Comp

   open Ast Comp.Util

   fun gen md =
   let (* name of the structure/signature *)
       val strName = Comp.strname md "Props"  
       val sigName = "INSN_PROPERTIES"

       (* The instructions *)
       val instructions = Comp.instructions md

       (* Arguments to the instruction functor *)
       val args =
           ["I : "^Comp.signame md "INSTR"
           ]

       (* Function that determines the type of an instruction *)
       val instrKind = DUMMYfun "instrKind"

       (* Functions for dealing with parallel copies *)
       val moveInstr = DUMMYfun "moveInstr"
       val moveTmpR  = DUMMYfun "moveTmpR"
       val moveDstSrc = DUMMYfun "moveDstSrc"

       val nop = DUMMYfun "nop"
       val jump = DUMMYfun "jump"

       val immedRange = ()
       val loadImmed  = DUMMYfun "loadImmed"

       val branchTargets = DUMMYfun "branchTargets"
       val setTargets    = DUMMYfun "setTargets"

       val eqOpn         = DUMMYfun "eqOpn"
       val hashOpn       = DUMMYfun "hashOpn"

       val defUse        = DUMMYfun "defUse"

       val getAnnotations = DUMMYfun "getAnnotations"
       val annotate       = DUMMYfun "annotate"

       val getGroup    = DUMMYfun "getGroup"
       val makeGroup   = DUMMYfun "makeGroup"

       (* The functor *)
       val strBody = 
           [$ ["structure I = I",
               "structure C = I.C",
               "structure LE = LabelExp",
               "",
               "exception NegateConditional",
               ""
              ],
            ERRORfun strName,
            $ ["datatype kind = IK_JUMP | IK_NOP | IK_INSTR",
               "              | IK_COPY | IK_CALL", 
               "              | IK_PHI | IK_SOURCE | IK_SINK",
               "datatype target = LABELLED of Label.label | FALLTHROUGH | ESCAPES",
               "",
               "exception NegateCondtional",
               ""
              ],
            instrKind,
            moveInstr,
            moveTmpR,
            moveDstSrc,
            nop,
            jump,
            loadImmed,
            branchTargets,
            setTargets,
            eqOpn,
            hashOpn,
            defUse,
            getAnnotations,
            annotate,
            getGroup,
            makeGroup
           ]

   in  Comp.codegen md "instructions/Props2"
         [Comp.mkFct md "Props" args sigName strBody
         ]
   end
end
