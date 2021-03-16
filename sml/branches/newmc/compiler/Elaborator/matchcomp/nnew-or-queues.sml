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

  val debugging = ElabControl.mcdebugging
  val say = Control_Print.say
  fun newline () = say "\n"
  fun saynl msg = (Control_Print.say msg; newline())
  fun dbsay msg =
      if !debugging
      then (say msg; newline())
      else ()
  fun dbsays msgs = dbsay (concat msgs)
  fun says strings = saynl (concat strings)
  fun bug msg = ErrorMsg.impossible ("ORQueues: "^msg)
in

structure AndorPriority =
struct

  (* priority = (weight, #bindLayers #branches, #defaults) 
   * weighted in that order, and inversely ordered for #branches and #defaults *)
  type priority = int * int * int * int

  type item = andor

  (* compare: priority * priority -> order *)
  (* priority to the first component, order on branching and defaults is reversed *)
  fun compare ((w1,bl1,b1,d1),(w2,bl2,b2,d2)) =
      (case Int.compare (w1,w2)               (* greater weight is better *)
	 of LESS => LESS
	  | GREATER => GREATER
	  | EQUAL =>
	    (case Int.compare (bl1, bl2)
	       of LESS => LESS
		| GREATER => GREATER
		| EQUAL =>
		  (case Int.compare (b1, b2)        (* or fewer variants is better *)
		     of LESS => GREATER
		      | GREATER => LESS
		      | EQUAL =>
			(case Int.compare (d1,d2)  (* or fewer defaults is better *)
			   of LESS => GREATER
			    | GREATER => LESS
			    | EQUAL => EQUAL))))

  (* priority: andor -> goodness *)
  (* priority applies only to OR nodes *)
  fun priority (OR{info={id, typ, ...}, variants, live, ...}) : priority =
      let val weight = ORinfo.getWeight id
	  val numBindingLayers = ORinfo.getNumBindingLayers id
	  val numDefaults = LS.numItems (LL.defaults live)
	  val numVariants = Variants.numItems variants
	      (* the number of keys actually occuring *)
	  val maxVariants = TU.typeVariants typ
	      (* how many variants could potentially occur *)
	  val numBranches =
	      (* number of branches, including a possible default branch *)
	      if numVariants < maxVariants andalso numDefaults > 0
	      then numVariants + 1  (* adds default branch *)
	      else numVariants (* ?? even if defaults exist too? *)
       in (weight, numBindingLayers, numBranches, numDefaults)
      end
    | priority _ = bug "priority: not OR node"

end (* structure AndorPriority *)

(* APQ: priority queues of OR nodes *)
structure APQ = LeftPriorityQFn(AndorPriority)

(* pqToList : APQ.queue -> APQ.item list *)
fun pqToList (q : APQ.queue) =
    case (APQ.next q)
     of SOME (andor, qrest) => andor :: pqToList qrest
      | NONE => nil

(* pqToString : APQ.queue -> string *)
fun pqToString q =
    PrintUtil.listToString ("[", ",","]") andorToString (pqToList q) 

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
	| OR _ => APQ.singleton andor       (* non-degenerate OR node is "opaque" *)
	| SINGLE{variant = (_,arg),...} => accessible arg
	    (* SINGLE nodes are "transparent" *)
	| _ => APQ.empty)

(* accessibleList : andor list -> APQ.queue *)
and accessibleList andors =
    foldl (fn (andor,queue) => APQ.merge(accessible andor, queue)) APQ.empty andors

(* selection based on "relevance" *)

(* CLAIM: layer \in directs  =>  layer not-\in defaults
 *   but layer not-\in defaults does not impliy layer \in direct (? example? )
 *)

(* relevantDisc : L.layer -> andor -> bool *)
(* "discriminant relevant": layer could be either confirmed or rejected by matching (or not)
 * one of the variants of the OR node *)
fun relevantDisc layer (OR{info = {id,...}, live, ...}) =
    let val directs = LL.directs live
	val disc = LS.member (directs, layer)
	fun dbprint (result) =
	    (dbsays ["<< relevantDisc: layer = ", L.layerToString layer,
		     ", node id = ", Int.toString id,
		     ", directs = ", LS.layerSetToString directs,
		     ", result = ", Bool.toString result];
	     result)
    in if disc  (* OR node can "discriminate" on this layer (i.e. confirm or reject) *)
       then if LS.member(LL.defaults live, layer) (* sanity check *)
	    then bug ("relevantDisc: layer is in both directs and defaults sets: " ^
		      L.layerToString layer)
	    else dbprint true
       else dbprint false
    end
  | relevantDisc _ _ = bug "relevantDisc: OR expected"

(* relevantBind : L.layer -> andor -> bool *)
(* the layer "involves" a variable binding associated with the OR node (??) *)
fun relevantBind layer (OR{info={id,...},...}) =
    LS.member (ORinfo.getBindingLayers id, layer)
  | relevantBind _ _ = bug "relevantBind: OR expected"

(* selectBestRelevant : APQ.queue * ruleno -> (andor * APQ.queue) option *)
(* CLAIM: the compatibility test is probably redundant, because queues will always
 * contain only mutually compatible OR nodes. *)
fun selectBestRelevant (orNodes: APQ.queue, leastLive: L.layer) =
    let fun relevant andor =
	    relevantDisc leastLive andor orelse
	    relevantBind leastLive andor
	val result = findAndRemove relevant orNodes
    in case result
	of NONE => 
	   (dbsay "<< selectBestRelevant: NONE"; NONE)
	 | SOME (chosen, rest) =>
	   (dbsays ["<< selectBestRelevant: chosen = ", andorToString chosen, ", Leaving: ", pqToString rest];
	    result)
    end

end (* local *)
end (* structure ORQueues *)

(* Notes:

(1) Need and example where a layer is "bind-relevant" but not "discriminant relevant",
    and visa versa to show that these two relevance tests are independent.

*)
