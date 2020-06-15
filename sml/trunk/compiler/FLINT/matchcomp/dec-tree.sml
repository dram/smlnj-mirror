(* dec-tree.sml *)
(* construction of a decision tree *)

(* decision tree construction (possibly a separate module)*)

(* the decision tree is simply a structure ordering OR nodes according to the combination
 * of the "best" (APQ.compare) ordering and the path suffix relation.
 *  -- OR nodes can have multiple occurrences in the decision tree (being duplicated in multiple
 *     branches under an earlier choice node.
 *  -- Note that the idea is to refer to the OR nodes from the original andor tree
 *     rather than creating new, redundant representations of these choices. *)

structure DecisionTree =
struct

local
    structure R = Rules
    structure T = Types
    structure TU = TypesUtil
    structure OO = OrderedOrNodes
    structure APQ = OO.APQ
    open MCTypes
in

(* type decTree: decision trees *)	
datatype decTree
  = DLEAF of ruleno    (* old version: RHS *)
     (* if you get to this node, bind variables along branch and dispatch to RHS(ruleno) *)
  | RAISEMATCH  (* probably redundant -- remove when sure *)
  | CHOICE of
    {node : andor,  (* an OR node used for dispatching *)
     choices : decVariant list,
     default : decTree option}
       (* one child for each variant of the node, + a default if node is partial *)
withtype decVariant = key * decTree

(* need to recover and traverse AND structure for selecting values to test. Can
 * recover from the original andor tree? *)

val numberChars = 256  (* this should be a basic configuration parameter in Target? *)

(* partial : andor -> bool *)
(* should this check for defaults? i.e. presence of a variable covering the node
 * and providing a default for missing keys? *)
fun partial (OR{variants as (key,andor)::_,...}) =
    (case key
       of D (dcon,_) => length variants < TU.dataconWidth dcon
        | C _ => length variants < numberChars
        | _ => true)
  | partial _ =  bug "parial"

(* makeDecisionTree : (APQ.queue * ruleset * path -> (decTree * APQ.queue) option *)
(* orNodes is a nodeGt sorted list of OR nodes
 * -- oldlive is a ruleset containing rules that are live on this branch,
 *    i.e. have survived earlier decisions on this branch
 * -- path is the path of the parent, candidate OR nodes must be compatible with this path
 * -- variantDecTrees processes each variant of the selected OR node.
 * -- keys all have type choiceKey, making it easier to iterate over variants *)
fun makeDecisionTree(orNodes, oldLive, oldPath) =
      (case OO.selectBestRelevant(orNodes, R.minItem oldLive, oldPath)
        of SOME (node as OR{path, live, defaults, variants, ...}, oldOrNodes) =>
	   (* best relevant OR node, oldOrNodes is rest of orNodes, still sorted *)
	   let val newOrNodes = APQ.merge(oldOrNodes, OO.accessibleList(map #2 variants))
		   (* add the newly accessible OR nodes to oldOrNodes *)
	       (* variantDecTrees: variant list * decVariant list * APQ.queue
                                   -> decVariant list * APQ.queue *)
	       fun variantDecTrees ((key,andor)::rest, decvariants, candidates) =
		   let val live = getLive andor
		       val defaults = getDefaults andor
		       val variantLive = R.intersection(R.union(live,defaults), oldLive)
		    in if R.isEmpty variantLive  (* should never happen with final default rule *)
		       then variantDecTrees(rest, (key, RAISEMATCH)::decvariants, candidates)
 		       else (case makeDecisionTree(candidates, R.union(variantLive,defaults), path)
			      of SOME(dtree, remaining) =>
				   variantDecTrees(rest, (key, dtree) :: decvariants, candidates)
			       | NONE =>  (* no relevant rules for this OR node *)
				 variantDecTrees(rest,
						 (key, DLEAF(R.minItem variantLive))::decvariants,
						 candidates))
		   end
		 | variantDecTrees(nil, decvariants, ornodes) = (rev decvariants, ornodes)
	       val (decvariants, remainder') = variantDecTrees(variants, nil, newOrNodes)
	       val (defaultOp, remainder'') =
		   if partial node
		   then case (makeDecisionTree(remainder', R.intersection(oldLive, defaults), path))
			 of SOME(dt, remainder'') => (SOME dt, remainder'')
			 | NONE => (NONE, remainder')
		   else (NONE, remainder')
	       in SOME(CHOICE{node = node, choices = decvariants, default = defaultOp},
		       remainder'')
	   end
	 | NONE => NONE  (* no relevant OR nodes in orNodes *)
	 | _ => bug "makeDecisionTree")

(* What to do when there are no relevant OR nodes in the queue? In this case, is the
 * match degenerate (only one pattern/rule)? Produce degenerate CHOICE{andor,DLEAF,NONE}? 
 * Or possibly DLEAF(andor)? *)

(* decistionTree: andor -> decTree *)
fun decisionTree andor =
    let val orNodes = OO.accessible andor
	val rules = R.union(getLive andor, getDefaults andor)
    in case makeDecisionTree(orNodes, rules, rootPath)
	of SOME (dectree,_) => SOME dectree
         | NONE => NONE
    end

end (* local *)
end (* structure DecisionTree *)
