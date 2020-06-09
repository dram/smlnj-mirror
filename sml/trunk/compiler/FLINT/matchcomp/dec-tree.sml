(* dec-tree.sml *)
(* construction of a decision tree *)

(* decision tree construction (possibly a separate module)*)

(* the decision tree is simply a structure ordering OR nodes according to the combination
 * of the "best" (APQ.compare) ordering and the path suffix relation.
 *  -- OR nodes can have multiple occurrences in the decision tree (being duplicated in multiple
 *     branches under an earlier choice node.
 *  -- Note that the idea is to refer to the OR nodes from the original andor tree
 *     rather than creating new, redundant representations of these choices. *)

(* type decTree: decision trees *)	
datatype decTree
  = LEAF of ruleno    (* old version: RHS *)
     (* if you get to this node, bind variables along branch and dispatch to RHS(ruleno) *)
  | RAISEMATCH  (* probably redundant -- remove when sure *)
  | CHOICE of
    {node : andor,  (* an OR node used for dispatching *)
     choices : decVariant list
     default = decTree option}
       (* one child for each variant of the node, + a default if node is partial *)
withtype decVariant = key * decTree

(* need to recover and traverse AND structure for selecting values to test. Can
 * recover from the original andor tree? *)

val numberChars = 128  (* should be a basic configuration parameter? *)

(* partial : andor -> bool *)
(* should this check for defaults? i.e. presence of a variable covering the node? *)
fun partial (OR{variants as (key,node)::_,...}) =
    (case key
       of Dkey (dcon,_) => length variants < TU.dataconWidth dcon
        | Ckey _ => length variants < numberChars
        | _ => true)
  | partial _ =  bug "parial"

(* makeDecisionTree : (APQ.queue * ruleset -> (decTree * APQ.queue) option *)
(* orNodes is a nodeGt sorted list of OR nodes
 * -- oldlive is a ruleset containing rules that are live on this branch,
 *    i.e. have survived earlier decisions on this branch
 * -- keyDts processes each variant of an OR node.
 * -- keys all have type choiceKey, making it easier to iterate over variants *)
fun makeDecisionTree(orNodes, oldLive, oldPath) =
      (case selectBestRelevant(orNodes, R.minItem oldLive, oldPath)
        of SOME (node as OR{path, live, defaults, variants, ...}, oldOrNodes) =>
	   (* best relevant OR node, oldOrNodes is rest of orNodes, still sorted *)
	   let val newOrNodes = APQ.merge(oldOrNodes, accessible(AND(map #2 variants)))
		   (* add the newly accessible OR nodes to oldOrNodes *)
	       (* variantDecTrees: variant list * decVariant list * APQ.queue
                         -> decVariant list * APQ.queue *)
	       fun variantDecTrees ((key,andor)::rest, decvariants, candidates) =
		   let val live = andorLive andor
		       val defaults = andorDefaults andor
		       val variantLive = R.intersect(R.union(live,defaults), oldLive)
		    in if R.isEmpty variantLive  (* should never happen with final default rule *)
		       then variantDecTrees(rest, (key, RAISEMATCH)::decvariants, candidates)
 		       else (case makeDecisionTree(candidates, R.union(live,defaults), path)
			      of SOME(dtree, remaining) =>
				   variantDecTrees(rest, (key, dtree) :: decvariants, remaining)
			       | NONE =>  (* no relevant rules for this OR node *)
				   variantDecTrees(rest, (key, LEAF(R.minItem newLive))::decvariants,
					  remaining))
		 | variantDecTrees(nil, decvariants, ornodes) = (rev decvariants, ornodes)
	       val (decvariants, remainder') = variantDecTrees(variants, nil, newOrNodes)
	       val (defaultChild, remainder'') =
		   if partial node
		   then let val (dt, remainer'') =
				makeDecisionTree(remainder', R.intersect(oldLive, defaults))
			in (SOME dt, remainder'')
			end
		   else (NONE, remainder')
	       in SOME(CHOICE{node = node, branches = decvariants, default = defaultChild},
		       remainder'')
	   end
	| NONE => NONE)  (* no relevant OR nodes in orNodes *)

(*  What to do when there are no relevant OR nodes in the queue? *)
