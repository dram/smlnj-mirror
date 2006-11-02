signature AMD64REWRITE = sig
  structure I  : AMD64INSTR
  structure CB : CELLS_BASIS = CellsBasis
  val rewriteUse : I.instruction * CB.cell * CB.cell -> I.instruction
  val rewriteDef : I.instruction * CB.cell * CB.cell -> I.instruction
  val frewriteUse : I.instruction * CB.cell * CB.cell -> I.instruction
  val frewriteDef : I.instruction * CB.cell * CB.cell -> I.instruction
end

