(*
 * This is a simple module for viewing a cluster graph graphically.
 * This is meant to be used only by those of you who don't want to 
 * migrate to the CFG data structure.
 *
 * -- Allen
 *)
functor ClusterViewer
   (structure ClusterGraph : CLUSTER_GRAPH
    structure GraphViewer  : GRAPH_VIEWER
    structure Asm          : INSTRUCTION_EMITTER
       sharing ClusterGraph.F.I = Asm.I
   ) : CLUSTER_VIEWER =
struct

   structure ClusterGraph = ClusterGraph
   structure F = ClusterGraph.F
   structure W = ClusterGraph.W
   structure L = GraphLayout
   structure FMT = FormatInstructionFn(Asm)

   val outline = MLRiscControl.getFlag "view-outline"

   fun view(clusterGraph) = 
   let val F.CLUSTER{regmap,...} = ClusterGraph.cluster clusterGraph 
       val toString = FMT.toString regmap
       fun graph _ = []
       fun edge(_,_,ref w) = [L.LABEL(W.toString w), L.COLOR "red"]
       fun title(blknum,ref freq) = 
           " "^Int.toString blknum^" ("^W.toString freq^")"
       fun node(_,F.ENTRY{blknum,freq,...}) = 
              [L.LABEL("entry"^title(blknum,freq))]
         | node(_,F.EXIT{blknum,freq,...})  = 
              [L.LABEL("exit"^title(blknum,freq))]
         | node(_,F.BBLOCK{blknum,freq,insns,...}) =
              [L.LABEL(title(blknum,freq)^"\n"^
                 (if !outline then "" else
                 List.foldl (fn (i,t) => toString i^"\n"^t) "" (!insns)))]
         | node (_,_) = [L.LABEL "???"]
   in  GraphViewer.view
         (L.makeLayout{graph=graph, edge=edge, node=node} clusterGraph)
   end
          
end
