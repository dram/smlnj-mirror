functor ClusterUnit(F:FRAME) = struct
  val dummyCluster = F.newFrame{name=Temp.newlabel(), formals=[]}
  val currCluster = ref dummyCluster
end

