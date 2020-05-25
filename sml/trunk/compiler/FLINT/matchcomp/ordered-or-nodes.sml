(* ordered-or-nodes.sml *)

(* Extract the OR nodes from the base andor tree and sort them according to the
 * "best choice" criteria. *)

structure OrderedOrNodes =
struct

local
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
  | accessibleOrNodes (andor as OR{children=ORdata((dcon,andorOp,_)::_),...}) =
    if singletonDcon dcon (* degenerate OR node -- no choice involved *)
    then case andorOp
	  of NONE => []  (* single datacon is a constant *)
	  | SOME andor => accessibleOrNodes andor (* traverse argument of single datacon *)
    else [andor]
  | accessibleOrNodes {andor as OR _) = [andor] (* non-degenerate OR node *)
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
 * Its role is similar to MCOpt.opt in the 0.33, 0.44 compilers. 
 * Assume the orNode arguments are not degenerate (i.e. single constructor datatype nodes). *)
fun compareOrNodes (OR{live=live1,defaults=defaults1,children=children1,...},
		    OR{live=live2,defaults=defaults2,children=children2,...})
    let val l1 = length defaults1  (* how many rules defaulted in first OR node *)
        val l2 = length defaults2  (* ditto for second *)
    in l1 < l2 orelse l1 = l2 and breadth children1 < breadth children2
    end
    
(* relevant : andor * ruleno -> bool *)
(* andor arg must be an OR node, ruleno will be least element of live ruleset *)
(* What does relevant(OR _ , n) mean? Is n expected to be live for this OR node?
 * If n in defaults, then the OR node doesn't have a chance of eliminating rule n?
 * i.e. there must be a child of the OR node with n _not_ in live for that child,
 * so a choice of that child will eliminate rule n from consideration as the first
 * matching rule. Other children may be consistent with rule n. *)
fun relevant (OR{defaults,...}, rulenum) =
      not (R.member(rulenum, defaults))
  | relevant _ 
      bug "relevant - unexpected andor"

(* selectBest : andor list -> (andor * andor list) option *)
(* the argument is a list or OR nodes, and selectBest returns the "best" OR node
 * and the remainer of the OR nodes in the argument supplemented with the OR nodes
 * that are "accessible" from the chosen best OR node *)
(* This version does not deal with how a context live set influences the choice via
 * "relevance" criterion. *)
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

(* selectBest : andor list -> (andor * andor list) option *)
(* a version selecting only "relevant" nodes *)
(* could keep the OR node list sorted wrt compareOrNodes, then just have to fileter
 * by relevance and take the first element as best *)
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

fun sortedOrNodes andor = Sort.sort(accessibleOrNodes andor, compareOrNodes)


(* ================================================================================ *)
(* decision tree construction (possibly a separate module)*)

(* the decision tree is simply a structure ordering OR nodes according to the combination
 * of the "best" (compareOrNodes) ordering and the path suffix relation.  Note that OR
 * nodes can have multiple  occurrences in the decision tree (being duplicated in multiple
 * branches under an earlier choice node. Note that the idea is to refer to the OR nodes
 * from the original andor tree rather than creating new, redundant representations of these
 * choices. *)
	
datatype decisionTree
  = LEAF of ruleno    (* was RHS *)
     (* if you get to this node, bind variables along branch and execute ruleno RHS *)
  | BRANCH of
    {node : andor,  (* an OR node used for dispatching *)
     children : decisionTree}

(* need to recover and traverse AND structure for selecting values to test. Can
 * recover from the original andor tree? *)

(* makeDecisionTree : andor * andor list -> decisionTree *)
(* makeDecisionTree takes the original andor tree and the ordered list of OR nodes
 * and constructs a tree of OR nodes based on the path dependencies among the OR nodes. *)
fun makeDecisionTree 

				    
(* ================================================================================ *)
(* generate match "code" (probably a separate module) *)

(* The "code" for the match is generated from the decision tree and information from
 * the original andor (about record deconstruction and variable bindings (and types).
 * This code performs pattern dispatching and deconstruction and then invokes the appropriate
 * RHS expressions with the necessary variable bindings.
 *
 * The "code" (at least initially) will be a neutral minimal abstract code (perhaps similar
 * to the Lambda IR in the early compiler (FN, APP, SELECT, SWITCH, etc.).  Types and type
 * variables are taken into account.  Goal is to see if the "code" language can be replaced by
 * (or translated into) Absyn (perhaps modified appropriately.
 *)

end (* structure OrderedOrNodes *)
