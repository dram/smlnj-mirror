(* sparcMLTree.sml --- customize MLRISC for the Sparc.
 * 
 * COPYRIGHT (c) 1998 AT&T Bell Laboratories.
 *)

(* constants specialised to the sparc *)
structure SparcConst = RegMaskConst(structure RegMask = SparcMask)

(* specialised sparc instruction set *)
structure SparcInstr = 
  SparcInstr(structure Const = SparcConst
	     structure Region = CPSRegions
(*
             structure Annotations = SMLNJAnnotations
*)
            )

structure SparcPseudoInstrs = SparcPseudoInstrs(SparcInstr)

structure SparcShuffle = SparcShuffle(SparcInstr)

structure SparcPseudoOps = PseudoOpsBig(SparcSpec)

(* flowgraph data structure specialized to Sparc instructions *)
structure SparcFlowGraph = 
  FlowGraph(structure I=SparcInstr
	    structure P=SparcPseudoOps
	    structure B=FunctionNames)

structure SparcAsmEmitter = 
  SparcAsmEmitter(structure Instr=SparcInstr
		  structure Shuffle=SparcShuffle
		  structure PseudoOps=SparcPseudoOps)

structure SparcMCEmitter = 
  SparcMCEmitter(structure Instr=SparcInstr
		 structure Assembler=SparcAsmEmitter)

structure SparcMLTree = 
  MLTreeF(structure Const=SparcConst
	  structure R=CPSRegions
	  structure P=SparcPseudoOps
	  structure B=FunctionNames)


(*
 * $Log: sparcMLTree.sml,v $
 * Revision 1.1.1.1  1998/08/05 19:37:50  george
 *   Release 110.7.4
 *
 *)
