(* hppaMLTree.sml --- customize MLRISC for the HPPA.
 * 
 * COPYRIGHT (c) 1996 AT&T Bell Laboratories.
 *
 *)

(* constants specialised to the hppa *)
structure HppaConst = RegMaskConst(structure RegMask = HppaMask)

(* specialised hppa instruction set *)
structure HppaInstr = HppaInstr(structure Const = HppaConst
                                structure Region = CPSRegions
                               )

(* flowgraph data structure specialized to Hppa instructions *)
structure HppaFlowGraph = 
  FlowGraph(structure I=HppaInstr
	    structure P=PseudoOpsBig)

structure HppaAsmEmitter = 
  HppaAsmEmitter(structure Instr=HppaInstr
		 structure FlowGraph=HppaFlowGraph)

structure HppaMCEmitter = 
  HppaMCEmitter(structure Instr=HppaInstr
					structure Assembler=HppaAsmEmitter
		structure FlowGraph=HppaFlowGraph)

structure HppaMLTree = 
  MLTreeF(structure Const=HppaConst
	  structure R=CPSRegions
	  structure P=PseudoOpsBig)


(*
 * $Log: hppaMLTree.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:46  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.4  1997/09/29 20:58:46  george
 *   Propagate region information through instruction set
 *
# Revision 1.3  1997/07/28  20:05:09  george
#   Added support for regions
#
# Revision 1.2  1997/07/17  12:37:39  george
#   The constant type used to specialize MLTrees is now done more compactly.
#
# Revision 1.1  1997/04/19  18:17:48  george
#   Version 109.27
#
 * Revision 1.1.1.1  1997/01/14  01:38:34  george
 *   Version 109.24
 *
 *)
