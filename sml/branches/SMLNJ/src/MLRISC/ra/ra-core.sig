(*
 * Note: This is the core of the new register allocator, i.e. the portion
 * that manipulates only the interference graph and not the flowgraph.
 *
 * -- Allen
 *)

signature RA_CORE = 
sig

   structure G  : RA_GRAPH

   type move_queue
   type freeze_queue

   (*
    * Basic functions
    *)

   (* dump the interference graph to a stream *)
   val dumpGraph : G.interferenceGraph -> TextIO.outstream -> unit
   val show      : G.interferenceGraph -> G.node -> string

   (* add an edge to the interference graph *)
   val addEdge : G.interferenceGraph -> G.node * G.node -> unit

   (* remove an edge from the interference graph *)
   val removeEdge : G.interferenceGraph -> G.node * G.node -> unit

   (*
    * Function to create new nodes 
    *)
   val newNodes : G.interferenceGraph -> 
        {cost:int,pt:G.programPoint,defs:int list,uses:int list} -> 
            G.node list (* defs *)

   (*
    * Update regmap after finishing register allocation or copy propagation
    *)
   val finishRA : G.interferenceGraph -> unit
   val finishCP : G.interferenceGraph -> unit

   (*
    * Create an initial set of worklists from a new interference graph
    * and a list of moves 
    *)
   val initWorkLists : G.interferenceGraph -> 
          { moves : G.move list,
            deadCopyElim : bool
          } -> 
          { simplifyWkl : G.node list, 
            moveWkl     : move_queue, 
            freezeWkl   : freeze_queue, 
            spillWkl    : G.node list   (* high degreee nodes *)
          }

   (*
    * Clear the interference graph but keep the nodes table intact 
    *)
   val clearGraph : G.interferenceGraph -> unit

   (*
    * Remove all adjacency lists from the nodes table
    *)
   val clearNodes : G.interferenceGraph -> unit

   (*
    * Return a regmap function that reflects the current interference graph.
    * Spilled registers are given the special value ~1
    *)
   val regmap      : G.interferenceGraph -> (int -> int)
   val spillRegmap : G.interferenceGraph -> (int -> int)
   val spillLoc    : G.interferenceGraph -> (int -> int)

   (* 
    * Simplify, Coalease and Freeze until the work list is done
    *)
   val iteratedCoalescing : 
        G.interferenceGraph -> 
           { simplifyWkl : G.node list, 
             moveWkl     : move_queue,
             freezeWkl   : freeze_queue,
             stack       : G.node list
           } ->
           { stack : G.node list 
           }

   (* 
    * potentially spill a node.
    *)
   val potentialSpillNode : 
        G.interferenceGraph ->
           { node  : G.node,
             stack : G.node list
           } ->
           { moveWkl   : move_queue,
             freezeWkl : freeze_queue,
             stack     : G.node list
           }

   (*
    * Color nodes on the stack, using Briggs' optimistic spilling.  
    * Return a list of actual spills 
    *)
   val select : 
        G.interferenceGraph -> 
           { biased : bool, (* use biased coloring too? *)
             stack  : G.node list 
           } ->
           { spills : G.node list (* actual spills *)
           }

   (*
    * Spill coalescing 
    *)
    val spillCoalescing : G.interferenceGraph -> G.node list -> unit

   (*
    * Spill coloring 
    *)
    val spillColoring : G.interferenceGraph -> G.node list -> unit

end
