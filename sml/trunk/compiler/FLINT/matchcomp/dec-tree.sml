(* dec-tree.sml *)
(* construction of a decision tree *)

(* decision tree construction (possibly a separate module)*)

(* the decision tree is simply a structure ordering OR nodes according to the combination
 * of the "best" (compareOrNodes) ordering and the path suffix relation.  Note that OR
 * nodes can have multiple  occurrences in the decision tree (being duplicated in multiple
 * branches under an earlier choice node. Note that the idea is to refer to the OR nodes
 * from the original andor tree rather than creating new, redundant representations of these
 * choices. *)
	
datatype decisionTree
  = LEAF of ruleno    (* was RHS *)
     (* if you get to this node, bind variables along branch and dispatch to RHS(ruleno) *)
  | RAISEMATCH  (* probably redundant -- remove when sure *)
  | CHOICE of
    {node : andor,  (* an OR node used for dispatching *)
     choices : (key * decisionTree) list  (with ORdata', etc)
     default = decisionTree}
       (* one child for each variant of the node, + a default if node is partial *)

(* need to recover and traverse AND structure for selecting values to test. Can
 * recover from the original andor tree? *)

(* makeDecisionTree : andor * andor list -> decisionTree *)
(* makeDecisionTree takes the original andor tree and the ordered list of OR nodes
 * and constructs a tree of OR nodes based on the path dependencies among the OR nodes. *)
fun makeDecisionTree 


(* Questions:
   (1) is there an operational difference between live and defaults rules?
   (2) is "relevant" relevant
*)


(* selectBestRelevant : andor list * ruleno -> (andor * andor list) option *)
fun selectBestRelevant (orNodes, leastLive) =
    let fun search((andor as OR{defaults,...}) :: andors, prefix) =
	    if not(R.member(leastLive, defaults)) (* andor is relevant *)
	    then SOME(andor, List.revAppend(prefix,andors))
	    else search(andors, andor::prefix)
	  | search (nil, _) = NONE  (* no relevant OR nodes *)
    in search(orNodes, nil)
    end
 
fun partial (OR{variants,...}) =
    case variants
     of ORdata (children as (dcon,_)::_) =>
	let val branching = length children
	    val breadth = TU.dataconWidth dcon
	 in branching < breadth
	end
     | ORvec _ => true  (* unbounded number of possible vector lengths *)
     | ORconst _ => true  (* approximately right *)
        (* what about complete match over (128?, 64k?) characters *)

(* makeDecisionTree : (andor list * ruleset -> decisionTree * andor list *)
(* orNodes is a nodeGt sorted list of OR nodes
 * oldlive is a ruleset containing rulenos of rules that are live on this branch,
 * i.e. have survived earlier decisions on the branch 
 * -- keyDts processes each variant of an OR node.
 * -- can fix the key type problem by unifying the key types (see choiceKey in mctypes.sml *)
*)
fun makeDecisionTree(orNodes, oldLive) =
      (case selectBestRelevant(orNodes, R.minItem oldLive)
        of SOME (node as OR{path, live, defaults, variants, ...}, oldOrNodes) =>
	   (* best relevant OR node, remainder is rest of orNodes, still sorted *)
	   let val children =  (* OOPS! ill-typed because of different key types *)
		   case variants  (* have to unify key types somehow *)
		    of ORdata children => children
		     | ORvec (ty,children) => children
		     | ORconst children => children
	       let val dependentNodes = map #2 children
		   val newOrNodes = List.concat(map accessibleOrNodes dependentNodes) 
		   val newCandidates = foldl insertNode oldOrNodes newOrNodes
		       (* add the newly accessible OR nodes to oldOrNodes *)
		   (* keyDts: (key?? * andor) list * (key?? * dtree) list * andor list)
                              -> (key?? * andor) list * andor list *)
		   fun keyDts ((key,(node as OR{live,defaults,...}))::rest,
				children, candidates) =
		       let val newLive = R.intersect(R.union(live,defaults), oldLive)
		       in if R.isEmpty newlive  (* can never happen with default rule? *)
			  then keyDts((key, RAISEMATCH)::children, candidates)
 			  else if R.numItems newlive = 1 (* could be default rule only *)
			    (* may need further tests, leading to possible RAISEMATCH *)
			  then ((key, LEAF(R.minItem newlive))::children, candidates)
			  else let val (dtree, rem_ornodes) =
			               makeDecisionTree(candidates, R.union(live,defaults))
			       in keyDts(rest, (key, dtree) :: newchildren,
					       rem_ornodes)
			       end
		       end
		     | keyDts(nil, children, ornodes) = (rev children, ornodes)
		   val (childDecisions, remainder') = keyDts(children, nil, newCandidates)
		   val (defaultChild, remainder'') =
		       if partial node
		       then let val (dt, remainer'') =
				    makeDecisionTree(remainder', R.interset(oldLive, defaults))
			    in (SOME dt, remainder'')
			    end
		       else (NONE, remainder')
		   val childBranches =  (* unify the children types *)
		       case variants
			 of ORdata _ => DECdata childDecisions
			  | ORvec _ => DECvec childDecisions
			  | ORconst _ => DECconst childDecisions
	       in (CHOICE{node = node, variants = childBranches, default = defaultChild},
		   remainder'')
	       end
