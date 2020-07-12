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

(* need to recover and traverse AND structure for selecting values to test. Can
 * recover from the original andor tree? *)

val numberChars = 256  (* this should be a basic configuration parameter in Target? *)

(* partial : andor -> bool *)
(* should this check for defaults? i.e. presence of a variable covering the node
 * and providing a default for missing keys? *)
fun partial (OR{variants as (key,andor)::_,...}) =
    (case key
       of D dcon => length variants < TU.dataconWidth dcon  (* missing constructor keys *)
        | C _ => length variants < numberChars
        | _ => true)
  | partial _ =  bug "parial"

(* decistionTree: andor -> decTree * int Vector.vector *)
fun decisionTree andor =
    let val orNodes = OO.accessible andor
	val rules = R.union(getLive andor, getDefaults andor)
		(* this should = allRules for the top andor *)
	val ruleCounts = Array.tabulate (R.numItems rules, (fn i => 0))
	fun incrementRuleCount r =
	    Array.update(ruleCounts, r, Array.sub(ruleCounts,r)+1)
		
(* makeDecisionTree : (APQ.queue * ruleset * path -> decTree *)
(* orNodes is a priority queue (APQ.queue) of OR nodes
 * -- oldlive is a ruleset containing rules that are live on this branch,
 *    i.e. have survived earlier decisions on this branch
 * -- oldPath is the path of the parent decision, which is the path of its OR node,
 *    candidate OR nodes must be compatible with this path
 * -- variantDecTrees processes each variant of the selected OR node.
 * -- keys all have type choiceKey, making it easier to iterate over variants
 * -- if survivors is empty, returns RAISEMATCH.
 * CLAIM: The orNodes queue argument will always be internally compatible. *)
fun makeDecisionTree(orNodes, survivors, thisPath) =
    if R.isEmpty survivors then DMATCH else
      (case OO.selectBestRelevant(orNodes, R.minItem survivors, thisPath)
        of SOME (node as OR{path, direct, defaults, variants, ...}, candidates) =>
	   (* best relevant OR node, remainder is queue of remaining OR nodes *)
	   let (* val _ =
		  (print "makeDecisionTree: \n";
		   print "  thisPath: "; MCPrint.tppPath thisPath;
	           print "  survivors: "; MCPrint.tppRules survivors;
		   print "  path: "; MCPrint.tppPath path) *)
	       (* variantDecTrees: variant list * decVariant list * APQ.queue
                                   -> decVariant list * APQ.queue *)
	       fun variantDecTrees ((key,andor)::rest, decvariants) =
		   let val variantPath = getPath andor
		       val variantLive = getLive andor
		       val variantSurvivors = R.intersection(variantLive, survivors)
		       val variantCandidates = APQ.merge(candidates, OO.accessible andor)
			    (* add newly accessible OR nodes only under this variant,
			     * OR nodes under other variants will be incompatible *)
		       val dectree =
			   makeDecisionTree(variantCandidates, variantSurvivors, variantPath)
		    in variantDecTrees(rest, (key, dectree) :: decvariants)
		   end
		 | variantDecTrees(nil, decvariants) = (rev decvariants)
	       val decvariants = variantDecTrees(variants, nil)
	       val defaultOp =
		   if partial node andalso not(R.isEmpty defaults)
		   then let val defaultSurvivors = R.intersection(survivors, defaults)
			in if R.isEmpty defaultSurvivors
			   then (print "Default: no survivors\n";
				 print "survivors: ";
				 MCPrint.tppRules survivors;
				 print "local live: ";
				 MCPrint.tppRules defaults)
			   else ();
			   SOME(makeDecisionTree(candidates, defaultSurvivors, thisPath))
			end
		   else NONE  (* no default clause *)
	       in CHOICE{node = node, choices = decvariants, default = defaultOp}
	   end
	 | NONE =>
	   (* no relevant OR nodes; pick minimum rule *)
	   let val rule = R.minItem survivors
	    in incrementRuleCount rule;
	       DLEAF rule
	   end
	 | _ => bug "makeDecisionTree")

(* What to do when there are no relevant OR nodes in the queue? In this case, is the
 * match degenerate (only one pattern/rule)? Produce degenerate CHOICE{andor,DLEAF,NONE}? 
 * Or possibly DLEAF(andor)? Or a new decTree constructor? Examples? *)

     in (makeDecisionTree(orNodes, rules, rootPath), Array.vector ruleCounts)
    end  (* function decistionTree *)

end (* local *)
end (* structure DecisionTree *)
