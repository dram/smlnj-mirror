(* The alpha assembler does not require any import declarations. *)
structure Alpha32Imports : IMPORTS = struct
  type import_kind = unit
  fun add _ = ()
  fun output _ = ""
end

structure Alpha32IrConstant = IrConstant(Alpha32Frame)

structure Alpha32IrPseudoOps = 
  IrPseudoOps(structure AsmIO=Alpha32AsmIO
	      structure Imports=Alpha32Imports)

(* mltree data structure *)
structure Alpha32MLTree = 
  MLTreeF(structure Const=Alpha32IrConstant  
	  structure P=Alpha32IrPseudoOps
	  structure R=TigerRegions)

(* specialised alpha32 instruction set *)
structure Alpha32Instr = 
  Alpha32Instr(structure Const=Alpha32IrConstant
	       structure Region=TigerRegions)

(* shuffle of parallel copies *)
structure Alpha32Shuffle = Alpha32Shuffle(Alpha32Instr)

structure Alpha32PseudoInstrs = 
  Alpha32PseudoInstrs(structure Instr=Alpha32Instr)

(* Flowgraph data structure specialized to DEC alpha instructions *)
structure Alpha32FlowGraph = 
  FlowGraph(structure I=Alpha32Instr  structure P=Alpha32IrPseudoOps)

(* asm emitter *)
structure Alpha32AsmEmitter=
  Alpha32AsmEmitter(structure Instr=Alpha32Instr
                    structure FlowGraph=Alpha32FlowGraph
		    structure Shuffle=Alpha32Shuffle)

(* rename tables *)
structure Alpha32RMapTbls = RMapTbls(structure C=Alpha32Cells) 
