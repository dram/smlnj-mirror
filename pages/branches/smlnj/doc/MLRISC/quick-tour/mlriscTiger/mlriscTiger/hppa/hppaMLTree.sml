(* The irConstant structure needs more parameterization *)
structure HppaIrConstant = IrConstant(HppaFrame)

structure HppaIrPseudoOps = 
  IrPseudoOps(structure AsmIO=HppaAsmIO structure Imports=HppaImports)

(* mltree data structure *)
structure HppaMLTree =
  MLTreeF(structure Const=HppaIrConstant  
	  structure P=HppaIrPseudoOps
	  structure R=TigerRegions)

(* specialised Hppa instruction set *)
structure HppaInstr = HppaInstr(structure Const=HppaIrConstant
				structure Region=TigerRegions)

(* shuffle parallel copies *)
structure HppaShuffle = HppaShuffle(HppaInstr)

(* Flowgraph data structure specialized to DEC alpha instructions *)
structure HppaFlowGraph = 
  FlowGraph(structure I=HppaInstr  structure P=HppaIrPseudoOps)

(* asm emitter *)
structure HppaAsmEmitter=
  HppaAsmEmitter(structure Instr=HppaInstr
		 structure FlowGraph=HppaFlowGraph
		 structure Shuffle=HppaShuffle)

(* rename tables *)
structure HppaRMapTbls = RMapTbls(structure C=HppaCells) 

