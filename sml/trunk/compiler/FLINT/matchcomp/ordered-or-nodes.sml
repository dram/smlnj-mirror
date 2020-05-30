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

fun singletonDcon dcon = 
    TU.dataconCount dcon = 1   (* dataconCount in TypesUtil *)

(* accessibleOrNodes : andor -> andor list *)
(* collect the "accessible" OR nodes, i.e. all OR nodes that are reachable from 
 * the root without passing through another OR node. If there are no OR nodes, 
 * accessibleOrNodes returns nil. All accessible nodes are independent (incomparable
 * with respect to path prefix ordering), because we stop at the first OR node
 * reachable from the root. *)
(* don't include singleton datatype OR nodes in result. In other words,
 * treat such OR nodes as degenerate, or fake choices. *)
fun accessibleOrNodes (AND{children,...}) =
      List.concat (map accessibleOrNodes children)
  | accessibleOrNodes (andor as SINGLE{arg,...}) =
      accessibleOrNodes arg (* traverse argument of singleton datacon *)
  | accessibleOrNodes {andor as OR _) = [andor] (* non-degenerate OR node *)
  | accessibleOrNodes (LEAF _) = []
  | accessibleOrNodes (VAR _) = []

(* breadth : orKind -> int *)
(* number of children of an OR node. These cound the number of alternatives actually
 * appearing in the pattern space, not the number of possible alternives, which are
 * effectively infinite in the case of constant and vector patterns.  *)
fun breadth (ORdata children) = length children
  | breadth (ORvec children) = length children
  | breadth (ORconst children) = length children

(* compareOrNodes : andor * andor -> bool *)
(* compareOrNodes (node1, node2) returns true if node1 is "better" than node2.
 * This is where the heuristics for match optimization come into play.
 * The heuristics are (Aitken version, 0.93? -- if defaults defn understood):
 *   (1) fewer defaults is better (more discriminating power? more rule exposed to elimination?
 *   (2) if same number of defaults, fewer children is better (less duplication of switch code)
 * The role of compareOrNodes is similar to MCOpt.opt in the 0.33 ond 0.44 compilers. 
 * ASSERT: the orNode arguments are not degenerate (i.e. single constructor datatype nodes). *)
fun compareOrNodes (OR{defaults=defaults1,children=children1,...},
		    OR{defaults=defaults2,children=children2,...})
    let val l1 = length defaults1  (* how many rules defaulted in first OR node *)
        val l2 = length defaults2  (* ditto for second *)
    in l1 < l2 orelse l1 = l2 and breadth children1 < breadth children2
    end
  | compareOrNodes _ = bug "compareOrNodes applied to non OR node(s)"
			   
(* relevant : andor * ruleno -> bool *)
(* andor arg must be an OR node, ruleno will be least element of live ruleset *)
(* What does relevant(OR _ , n) mean? Is n expected to be live for this OR node?
 * If n in defaults, then the OR node doesn't have a chance of eliminating rule n?
 * i.e. there must be a child of the OR node with n _not_ in live or defaults for
 * that child, so a choice of that child will eliminate rule n from consideration
 * as the first matching rule. Other children may be consistent with rule n. *)
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
 * before select is applied to it, in which case the live argument could be omitted. *)
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

(* selectBest : andor list -> (andor * andor list) option
 * assuming that andor list is sorted with respect to compareOrNodes and is prefiltered
 * with respect to relevance to some live ruleset. Now selection becomes trivial. *)
fun selectBest nil = NONE
  | selectBest (node::rest) = SOME(node, rest)

(* orderedOrNodes : andor -> andor list *)
(* orderedOrNodes iterates selectBest until the OR nodes are exhausted, returning a
 * list of all OR nodes in the argument ordered by the compareOrNodes relation.
 * What to do with OR nodes for single datacon types (i.e. ORdata with no real "choice")?
 * discard these and don't regard them as real OR nodes. They will be stripped during
 * code generation (assuming tagged representation). *)
fun orderedOrNodes andor =
    let val accessible = accessibleOrNodes andor
    in  case accessible
	 of nil => (* no OR nodes *)
	  | [orNode] => accessible (* only one OR node to choose from *)
    end

(* sortedOrNodes: andor -> andor list *)
(* an alternative to orderOrNodes above. *)
fun sortedOrNodes andor = Sort.sort(accessibleOrNodes andor, compareOrNodes)

end (* structure OrderedOrNodes *)

