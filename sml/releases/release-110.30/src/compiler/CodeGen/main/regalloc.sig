signature REGALLOC =
sig
   structure F : FLOWGRAPH 
   val ra : F.cluster -> F.cluster
end
