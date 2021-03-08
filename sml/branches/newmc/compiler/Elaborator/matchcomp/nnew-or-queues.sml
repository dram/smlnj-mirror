(* nnew-or-queues.sml *)

(* Extract the OR nodes from the base andor tree and sort them according to the
 * "best choice" heuristic criteria (compare and priority). The sorted sets of
 * OR nodes are represented by priority queues using the SML/NJ Library Util
 * functor LeftPriorityQFn. *)

structure ORQueues =
struct

local
    structure L = Layers
    structure LS = L.Set
    structure LL = LiveLayers
    structure TU = TypesUtil
    structure MU = MCUtil
    open MCTypes (* AND, OR, SINGLE, VARS, LEAF, getId *)
in

fun bug msg = ErrorMsg.impossible ("ORQueues: "^msg)

structure AndorPriority =
struct
  (* priority = (number defaults, number variants) *)
  type priority = int * int

  type item = andor

  (* compare: priority * priority -> order *)
  (* smaller numbers are "better", hence GREATER, priority to the first component *)
  fun compare ((x1,y1),(x2,y2)) =
      (case Int.compare(x1, x2)
         of LESS => GREATER            (* fewer defaults *)
	  | EQUAL =>
	      (case Int.compare(y1,y2)
		 of LESS => GREATER    (* fewer variants *)
		  | EQUAL => EQUAL
		  | GREATER => LESS)
	  | GREATER => LESS)

  (* priority: andor -> goodness *)
  (* priority applies only to OR nodes *)
  fun priority (OR{info={typ,...}, variants, live, ...}) : priority =
      let val numVariants = Variants.numItems variants (* the number of keys actually occuring *)
	  val maxVariants = TU.typeVariants typ (* how many variants could potentially occur *)
	  val numDefaults = LS.numItems (LL.defaults live)
	  val numBranches = (* then number of branches, including a possible default branch *)
	        if numVariants < maxVariants andalso numDefaults > 0
		then numVariants + 1  (* adds default branch *)
		else numVariants (* ?? even if defaults exist too? *)
       in (numBranches, numDefaults)  (* banching trumps number of defaults *)
      end
    | priority _ = bug "priority: not OR node"

end (* structure AndorPriority *)

(* APQ: priority queues of OR nodes *)
structure APQ = LeftPriorityQFn(AndorPriority)

(* toList : APQ.queue -> APQ.item list *)
fun toList (q : APQ.queue) =
    case (APQ.next q)
     of SOME (andor, q') => andor :: toList q'
      | NONE => nil

(* toString : APQ.queue -> string *)
fun toString q =
    PrintUtil.listToString ("[", ",","]") (fn andor => Int.toString (getId andor)) (toList q) 

(* findAndRemove: (APQ.item -> bool) -> APQ.queue -> (APQ.item * APQ.queue) option *)
(* This function will be added to the Priority Queue interface, at which point it
 * will not need to be defined here. This version is specific to APQ. *)
fun findAndRemove (pred: APQ.item -> bool) (queue: APQ.queue) =
    let fun find(queue: APQ.queue, prefix: APQ.item list) =
	    case APQ.next queue
	     of SOME(item, queue') =>
		  if pred item
		  then SOME(item, foldl APQ.insert queue' (rev prefix))
		  else find(queue', item::prefix)
	      | NONE => NONE
     in find(queue, nil)
    end

(* DEFN: two (OR) nodes are compatible with one another if their paths do not
 * diverge at an OR node (i.e. the first key difference in their paths is not
 * a choice key). *)

(* accessible : andor -> APQ.queue *)
(* collect the "accessible" OR nodes, i.e. all OR nodes that are reachable from
 * the root without passing through another OR node. If there are no OR nodes,
 * accessible returns nil. All accessible nodes are independent (incomparable
 * with respect to path prefix ordering), because we stop at the first OR node
 * reachable from the root. Singleton datatypes produce SINGLE nodes, not OR nodes,
 * and SINGLE nodes are treated as "transparent" (like AND nodes) wrt collecting OR nodes.
 * The result is a priority queue of nodes (APQ.queue) sorted by AndorPriority.compare.
 * the node with the greatest priority (wrt compare) is returned first. *)
(* CLAIM: all the OR nodes in the result queue are compatible with one another. *)
fun accessible andor =
    (case andor
       of AND{children,...} => accessibleList children
	| OR _ => APQ.singleton andor       (* non-degenerate OR node is opaque *)
	| SINGLE{variant = (_,arg),...} => accessible arg
	    (* SINGLE nodes are "transparent" *)
	| _ => APQ.empty)

(* accessibleList : andor list -> APQ.queue *)
and accessibleList andors =
    foldl (fn (andor,queue) => APQ.merge(accessible andor, queue)) APQ.empty andors

(* selectBestRelevant : APQ.queue * ruleno -> (andor * APQ.queue) option *)
(* CLAIM: the compatibility test is probably redundant, because queues will always
 * contain only mutually compatible OR nodes. *)
fun selectBestRelevant (orNodes: APQ.queue, leastLive: L.layer) =
    let fun relevant (OR{live,...}) =
	      not(LS.member(LL.defaults live, leastLive))
	  | relevant _ = bug "selectBestRelevant..relevant: not OR node"
     in findAndRemove relevant orNodes
    end

end (* local *)
end (* structure OrderedOrNodes *)
