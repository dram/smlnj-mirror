(* ordered-or-nodes.sml *)

(* Extract the OR nodes from the base andor tree and sort them according to the
 * "best choice" heuristic criteria (compare and priority). The sorted sets of
 * OR nodes are represented by priority queues using the SML/NJ Library Util
 * functor LeftPriorityQFn. *)

structure OrderedOrNodes =
struct

local
    structure R = Rules
    open MCTypes (* AND, OR, SINGLE, VARS, LEAF *)
in

structure AndorPriority =
struct
  (* priority = (number defaults, number variants) *)
  type priority = int * int

  type item = andor

  (* compare: priority * priority -> order *)
  (* smaller numbers are "better", hence GREATER *)
  fun compare ((d1,b1),(d2,b2)) =
      case compare(d1, d2)
       of LESS => GREATER
	| EQUAL =>
	  (case compare(w1,w2)
	    of LESS => GREATER
	     | EQUAL => EQUAL
	     | GREATER => LESS)
	| GREATER => LESS
  
  (* priority: andor -> goodness *)
  (* priority intended only for OR nodes *)
  fun priority (OR{defaults,variants,...}) : priority =
	(length defaults, length variants)
    | priority _ = (10000,10000)
	(* "infinitely low" priority for non-OR nodes, which won't occur in queues *)

end (* structure AndorPriority *)
    
(* APQ: priority queues of OR nodes *)
structure APQ = LeftPriorityQFn(AndorPriority)

(* findAndRemove: (APQ.item -> bool) -> APQ.queue -> (APQ.item * APQ.queue) option *)
(* This function will be added to the Priority Queue interface, at which point it
 * will not need to be defined here. This version is specific to APQ. *)
fun findAndRemove pred queue =
    let fun find(queue: APQ.queue, prefix: item list) =
	    case APQ.next
	     of SOME(item, queue') =>
		  if pred item
		  then SOME(item, foldl APQ.insert (rev prefix) queue')
		  else find(queue', item::prefix)
	      | NONE => NONE
     in find(queue, nil)
    end

(* accessible : andor -> APQ.queue *)
(* collect the "accessible" OR nodes, i.e. all OR nodes that are reachable from 
 * the root without passing through another OR node. If there are no OR nodes, 
 * accessible returns nil. All accessible nodes are independent (incomparable
 * with respect to path prefix ordering), because we stop at the first OR node
 * reachable from the root. Singleton datatypes produce SINGLE nodes, not OR nodes,
 * and SINGLE nodes are treated as "transparent" (like AND nodes) wrt collecting OR nodes.
 * The result is a priority queue of nodes (APQ.queue) sorted by AndorPriority.compare.
 * the node with the greatest priority (wrt compare) is returned first. *)
fun accessible andor =
    case andor
     of AND{children,...} =>
	  foldl (fn (andor,queue) = APQ.merge(accessible andor, queue)) APQ.empty children
      | SINGLE{arg,...} = accessible arg (* SINGLE nodes are "transparent" *)
      | (andor as OR _) = APQ.singleton andor (* non-degenerate OR node is opaque *)
      | LEAF _ = APQ.empty
      | VARS _ = APQ.empty

(* selectBestRelevant : APQ.queue * ruleno -> (andor * APQ.queue) option *)
fun selectBestRelevant (orNodes: APQ.queue, leastLive: ruleno, oldpath) =
    let fun relevant (OR{path,defaults,...}) =>
	    not(R.member(leastLive, defaults)) andalso
	    not(incompatible(oldpath,path)
	      (* OR node is relevant *)
	  | relevant _ => bug "relevant"
     in findAndRemove relevant orNodes
    end

end (* structure OrderedOrNodes *)

