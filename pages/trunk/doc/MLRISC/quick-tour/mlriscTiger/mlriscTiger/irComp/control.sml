signature TIGER_CONTROL = sig
  val prTrees : bool ref
  val prAbsyn : bool ref
  val prFlowgraph: bool ref
end

structure TigerControl : TIGER_CONTROL = struct
  val prTrees = ref false
  val prAbsyn = ref false
  val prFlowgraph = ref false
end

