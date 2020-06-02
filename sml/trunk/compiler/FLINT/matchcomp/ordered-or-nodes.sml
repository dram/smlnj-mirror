(* ordered-or-nodes.sml *)

(* Extract the OR nodes from the base andor tree and sort them according to the
 * "best choice" criteria. *)

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
    TU.dataconCount dcon = 1   (* dataconCount in TypesUtil *)

(* orBreadth : orKind -> int *)
(* number of children of an OR node. These cound the number of alternatives actually
 * appearing in the pattern space, not the number of possible alternives, which are
 * effectively infinite in the case of constant and vector patterns.  *)
fun orBreadth (ORdata children) = length children
  | orBreadth (ORvec children) = length children
  | orBreadth (ORconst children) = length children

(* metric: andor -> int * int *)
(* metric intended only for OR nodes *)
fun metric (OR{defaults,variants,...}) : (int * int) =
    (length defaults, orBreadth variants)
  | metric _ = (10000,10000)  (* "infinite" metric for non-OR nodes *)

fun metricGt ((d1,w1),(d2,w2)) =
    d1 > d2 orelse (d1 = d2) and w1 > w2

fun nodeGt (node1,node2) = metricGt(metric node1, metric node2)

(* sortNodes : andor list -> andor list *)
(* Sorts a list of nodes in ascending order by metric
 * Meant to be applied only to lists of OR nodes.
 * Does not merge "duplicates", i.e. distinct nodes with the same
 * metric remain distinct in result *)
fun sortNodes andors =
    ListMergeSort metricGt andors

(* insertNode : andor * andor list -> andor list
 * inserts a node into a list of nodes, maintaining nodeGt ascending order.
 * INVARIANT: The node being inserted should always be a new node, so the
 * eqNode test should always fail. *)
fun insertNode (andor, nil) = [andor]
  | insertNode (andor, andors as andor1::rest) =
      if eqNode(andor, andor1) then andors  (* don't insert another copy of an existing node *)
      else if nodeGt (andor1, andor) then andor :: andors
      else andor1 :: insertNode(andor, rest)
			       
(* compareOrNodes : andor * andor -> bool *)
(* compareOrNodes (node1, node2) returns true if node1 is "better" than node2.
 * This is where the heuristics for match optimization come into play.
 * The heuristics are (Aitken version, 0.93? -- if defaults defn understood):
 *   (1) fewer defaults is better (more discriminating power? more rules exposed to elimination?
 *   (2) if same number of defaults, fewer children is better (less du\plication of switch code)
 * The role of compareOrNodes is similar to MCOpt.opt in the 0.33 ond 0.44 compilers. 
 * ASSERT: the orNode arguments are not degenerate (i.e. single constructor datatype nodes).
fun compareOrNodes (OR{path = path1, defaults=defaults1,children=children1,...},
		    OR{path = path2, defaults=defaults2,children=children2,...}) =
    if eqPath(path1, path2) then EQUAL
    else let val l1 = length defaults1  (* how many rules defaulted in first OR node *)
	     val l2 = length defaults2  (* ditto for second *)
	  in case compare(l1,l2)
	     of LESS => LESS
	      | EQUAL => compare(orBreadth children1, orBreadth children2)
	      | GREATER => GREATER
	 end
  | compareOrNodes _ = bug "compareOrNodes applied to non OR node(s)"
*)

(* binary set of nodes doesn't work because different nodes will compare EQUAL and
 * thus be "merged" into one
stucture NodeKey : ORD_KEY =
struct
  type ord_key = AndOr.andor	   
  val compare = compareOrNode
end (* structure NodeKey *)

structure NodeSet = BinarySetFn(NodeKey)

(* only OR nodes will be collected in NodeSet.sets. The "best" OR node will be
 * the minimum item in the set. *)
 *)
			   
(* accessibleOrNodes : andor -> andor list *)
(* collect the "accessible" OR nodes, i.e. all OR nodes that are reachable from 
 * the root without passing through another OR node. If there are no OR nodes, 
 * accessibleOrNodes returns nil. All accessible nodes are independent (incomparable
 * with respect to path prefix ordering), because we stop at the first OR node
 * reachable from the root. Singleton datatypes produce SINGLE nodes, not OR nodes,
 * and SINGLE nodes are treated as "transparent" (like AND nodes) wrt collecting OR nodes.
 * The resulting node list is sorted in ascending order wrt nodeGt, i.e.the "best" OR
 * node (least metric) is at the beginning. *)
fun accessibleOrNodes andor =
    let fun collect (AND{children,...}) = List.concat (map collect children)
	  | collect (SINGLE{arg,...}) = collect arg (* SINGLE nodes are "transparent" *)
	  | collect {andor as OR _) = [andor] (* non-degenerate OR node is opaque *)
	  | collect (LEAF _) = nil
	  | collect (VAR _) = nil
     in collect andor
    end

fun sortedAccessibleOrNodes andor = sortNodes(accessibleOrNodes(andor))
    let fun collect (AND{children,...}) = List.concat (map collect children)
	  | collect (SINGLE{arg,...}) = collect arg (* SINGLE nodes are "transparent" *)
	  | collect {andor as OR _) = [andor] (* non-degenerate OR node is opaque *)
	  | collect (LEAF _) = nil
	  | collect (VAR _) = nil
    in sortNodes(collect andor)
    end
	fun 

(* relevant : andor * ruleno -> bool *)
(* andor arg must be an OR node, ruleno will be least element of live ruleset *)
(* What does relevant(OR _ , n) mean? Is n expected to be live for this OR node?
 * If n in defaults, then the OR node doesn't have a chance of eliminating rule n?
 * i.e. there must be a child of the OR node with n _not_ in live or defaults for
 * that child, so a choice of that child will eliminate rule n from consideration
 * as the first matching rule. Other children may be consistent with rule n.
 * What if the ruleno is not live for the OR node?  Then it is incompatible with
 * some choice in the OR-node's path. Can the OR node be "relevant" to such a ruleno? *)
(* ASSERT: if n not in defaults for an OR node, then there exists a child of the node
 * such that n is not live for that child. *)
fun relevant (OR{defaults,...}, rulenum) =
      not (R.member(rulenum, defaults))
  | relevant _ 
      bug "relevant - unexpected andor"

(* (first version) selectBest : andor list -> (andor * andor list) option *)
(* the argument is a list or OR nodes, and selectBest returns the "best" OR node
 * and the remainer of the OR nodes in the argument supplemented with the OR nodes
 * that are "accessible" from the chosen best OR node *)
(* This version does not deal with how a context live set influences the choice via
 * "relevance" criterion.
fun selectBest (nil) = NONE  (* only happens if there are no accessible OR nodes *)
  | selectBest [orNode] = SOME(orNode, nil)  (* only one choice *)
  | selectBest (orNode::rest) =
    let fun select (node, node' :: rest, remainder) =
	    if compareOrNodes(node, node')  (* node is better than node' *)
	    then select(node, rest, node'::remainder) (* keep node as potential best *)
	    else select(node', rest, node::remainder) (* otherwise, node' become the contender *)
	  | select (node, [], remainder) = SOME(node, rev remainder)
     in select (orNode, rest, nil)
    end
*)
      
(* selectBest : andor list * ruleset -> (andor * andor list) option *)
(* a version selecting only "relevant" nodes. It has to scan the filtered OR node list
 * for the heuristically best node on each call. *)
(* could assume that the OR node list is sorted wrt compareOrNodes, then you just have
 * to fileter by relevance and then take the first element as best *)
(* or perhaps the filtering for relevance could be performed on the node list argument
 * before select is applied to it, in which case the live argument could be omitted.
fun selectBest (orNodes, live) = (* only happens if there are no accessible OR nodes *)
    let val firstLive = R.minItem live
	val relevantNodes = List.filter (fn node => relevant(node, firstLive)) orNodes
	fun select nil = NONE  (* only happens if there are no accessible OR nodes *)
	  | select [orNode] = SOME (orNode, nil)  (* only one choice *)
	  | select (orNode::rest) =
	    let fun sel (node, node' :: rest, remainder) =
		    if compareOrNodes(node, node')  (* node is better than node' *)
		    then sel(node, rest, node'::remainder)
		    else sel(node', rest, node::remainder)
		  | sel (node, [], remainder) = (SOME node, rev remainder)
	     in sel (orNode, rest, nil)
	    end
     in select orNodes
    end
 *)
    
(* selectBest : andor list -> (andor * andor list) option
 * assuming that andor list is sorted with respect to compareOrNodes and is prefiltered
 * with respect to relevance to some live ruleset. Now selection becomes trivial.
fun selectBest (set: NodeSet.set) =
    if NodeSet.empty set then NONE else NodeSet.minItem set
 *)
	
(* selectBest : andor list -> (andor * andor list) option
 * argument andor list is assumed to be sorted wrt nodeGt *)
fun selectBest nil = NONE
  | selectBest (andor::andors) = SOME(andor, andors)

(* orderedOrNodes : andor -> andor list *)
(* orderedOrNodes iterates selectBest until the OR nodes are exhausted, returning a
 * list of all OR nodes in the argument ordered by the compareOrNodes relation.
 * What to do with OR nodes for single datacon types (i.e. ORdata with no real "choice")?
 * discard these and don't regard them as real OR nodes. They will be stripped during
 * code generation (assuming tagged representation).
fun orderedOrNodes andor =
    let val accessible = accessibleOrNodes andor
    in  case accessible
	 of nil => (* no OR nodes *)
	  | [orNode] => accessible (* only one OR node to choose from *)
    end
 *)

end (* structure OrderedOrNodes *)

