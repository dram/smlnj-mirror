(* ordered-or-nodes.sml *)

(* Extract the OR nodes from the base andor tree and sort them according to the
 * "best choice" heuristic criteria. *)

structure OrderedOrNodes =
struct

local
    structure TU = TypesUtil
    structure R = Rules
    open MCCommon
in

(* datatypeWidth : T.datacon -> int *)
(* number of distince datacons for the datatype to which the datacon belongs *)
fun datatypeWidth dcon = 
    TU.dataconWidth dcon   (* dataconCount in TypesUtil *)

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
fun priority (OR{defaults,variants,...}) : goodness =
    (length defaults, length variants)
  | priority _ = (10000,10000)  (* "infinite" metric for non-OR nodes *)

end (* structure AndorPriority *)
    
(* priority queues of OR nodes *)
structure APQ = LeftPriorityQFn(AndorPriority)

(* accessible : andor -> APQ.queue *)
(* collect the "accessible" OR nodes, i.e. all OR nodes that are reachable from 
 * the root without passing through another OR node. If there are no OR nodes, 
 * accessibleOrNodes returns nil. All accessible nodes are independent (incomparable
 * with respect to path prefix ordering), because we stop at the first OR node
 * reachable from the root. Singleton datatypes produce SINGLE nodes, not OR nodes,
 * and SINGLE nodes are treated as "transparent" (like AND nodes) wrt collecting OR nodes.
 * The resulting node list is sorted in ascending order wrt nodeGt, i.e.the "best" OR
 * node (least metric) is at the beginning. *)
fun accessible andor =
    case andor
     of AND{children,...} =>
	  foldl (fn (andor,queue) = APQ.merge(accessible andor, queue)) APQ.empty children
      | SINGLE{arg,...} = accessible arg (* SINGLE nodes are "transparent" *)
      | (andor as OR _) = APQ.singleton andor (* non-degenerate OR node is opaque *)
      | LEAF _ = APQ.empty
      | VARS _ = APQ.empty

(* selectBestRelevant : APQ.queue * ruleno -> (andor * APQ.queue) option *)
fun selectBestRelevant (orNodes, leastLive) =
    let fun search(queue, prefix)
              (case APQ.next orNodes
		 of SOME((andor as OR{defaults,...}), orNodes') =>
		    if not(R.member(leastLive, defaults)) (* OR node is relevant *)
		    then SOME(andor,
			      foldl APQ.insert (rev prefix) orNodes')
		    else search(orNodes', andor::prefix)
		  | NONE => NONE
		  | _ => bug "selectBestRelevant")
    in search(orNodes, nil)
    end

end (* structure OrderedOrNodes *)

