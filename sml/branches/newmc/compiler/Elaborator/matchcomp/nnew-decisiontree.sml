(* decisiontree.sml *)
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
  structure R = Rules  (* sets of rule numbers *)
  structure T = Types
  structure TU = TypesUtil
  structure L = Layers
  structure LS = Layers.Set
  structure LL = LiveLayers
  structure Q = ORQueues
  structure APQ = Q.APQ
  open MCTypes

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

  fun bug msg = ErrorMsg.impossible ("MatchComp: " ^ msg)

in

(* need to recover and traverse AND structure for selecting values to test. Can
 * recover from the original andor tree? *)

(* Terminology: a _trace_ is a list of andor paths recording the decision
 * points in a complete/maximal branch of a decision tree. Traces are needed
 * for (1) determining "scoping" of multiple versions of a variable produced by
 * OR patterns, and (2) as a source for producing match counterexamples for 
 * decision tree branches terminating with DMATCH. *)

val numberChars = 256  (* this should be a basic configuration parameter in Target? *)

(* NEED: typeVariants function *)

(* partial : andor -> bool *)
(* should this check for defaults? i.e. presence of a variable covering the node
 * and providing a default for missing keys? *)
fun partial (OR{info={typ,...}, variants, ...}) =
    let val numVariants = Variants.numItems variants
	val numTypeVariants = TU.typeVariants typ  (* "width" of the type of the OR node *)
     in numVariants < numTypeVariants
    end
(*    (case key
       of D (dcon,_) => length variants < TU.dataconWidth dcon  (* missing constructor keys *)
        | C _ => length variants < numberChars
        | _ => true)
*)
  | partial _ =  bug "partial"

(* decistionTree: andor -> decTree * int Vector.vector *)
(* translates an andor tree into a decision tree and also returns a vector of use counts for
 * the rules.  If uses(r) > 1 then the rhs of rule r needs to be abstracted for reuse.
 * If uses(r) = 0, then rule r is redundant, i.e. the match has redundant rules. *)
fun decisionTree (andor, numRules) =
let val _ = dbsays [">> decisionTree: andor ID = ", Int.toString (getId andor),
			", numRules = ", Int.toString numRules]
    val initialLayers = getLive andor
          (* this should be normally be all rule-layers, developed by makeAndor *)
    val ruleCounts = Array.tabulate (numRules, (fn i => 0))
    fun incrementRuleCount r =
	let val newCount = Array.sub(ruleCounts,r) + 1
	 in Array.update(ruleCounts, r, newCount);
	    dbsays ["<< incrementRuleCount: ", Int.toString r, " to ", Int.toString(Array.sub(ruleCounts,r))]
	end

    val initialOrNodes = Q.accessible andor
    val _ = dbsays["** decisionTree: intialOrNodes = ", ORQueues.pqToString initialOrNodes]

    (* makeDecisionTree : APQ.queue * LS.set * trace -> decTree *)
    (* orNodes is a priority queue (APQ.queue) of OR nodes
     * -- oldlive is a ruleset containing rules that are live on this branch,
     *    i.e. have survived earlier decisions on this branch
     * -- oldPath is the path of the parent decision, which is the path of its OR node,
     *    candidate OR nodes must be compatible with this path
     * -- variantDecTrees processes each variant of the selected OR node.
     * -- keys all have type choiceKey, making it easier to iterate over variants
     * -- if survivors is empty, returns RAISEMATCH.
     * CLAIM: The orNodes queue argument will always be internally compatible. *)
    fun makeDecisionTree(orNodes: APQ.queue, survivors: LS.set, dtrace) =
	(dbsays [">> makeDecisionTree: survivors = ", LS.layerSetToString survivors,
		 "; orNodes = ", ORQueues.pqToString orNodes];
	 case LS.minItem survivors
	   of NONE => DMATCH (rev dtrace)  (* no survivors, match failure *)
	    | SOME minSurvivor => 
	      (case Q.selectBestRelevant(orNodes, minSurvivor)
	         of SOME (node as OR{info = {id, path,...}, live, variants, ...},
			  candidates) =>
		  (* best relevant OR node, candidates is queue containing remainder of OR nodes *)
		    let val _ = dbsays ["** makeDecisionTree[SOME(OR)]: best relevant = ",
					Int.toString id]
		      (* (print "makeDecisionTree: \n";
			  print "  thisPath: "; MCPrint.tppPath thisPath;
			  print "  survivors: "; MCPrint.tppRules survivors;
			  print "  path: "; MCPrint.tppPath path) *)
		      (* andorToDecTree: andor -> decisionTree
		       * the andor of each variant is a child of the parent OR node, but it may be
		       * a LEAF node!!! *)
		      fun andorToDecTree andor =
			  let val variantPath = getPath andor
			      val variantLive = getLive andor
			      val variantSurvivors = LS.intersect(variantLive, survivors)
			      val variantCandidates = APQ.merge(candidates, Q.accessible andor)
				   (* add newly accessible OR nodes only under this variant,
				    * OR nodes under other variants will be incompatible *)
			   in dbsays [">> andorToDecTree: id = ", Int.toString (getId andor),
				      "\n   path = ", pathToString variantPath,
				      "\n   live = ", LS.layerSetToString variantLive,
				      "\n   variantSurvivors = ", LS.layerSetToString variantSurvivors,
				      "\n   variantCandidates = ", ORQueues.pqToString variantCandidates];
			      makeDecisionTree(variantCandidates, variantSurvivors,
					       variantPath::dtrace)
			  end
		      val decvariants = Variants.map andorToDecTree variants
		      val defaultOp =
			  if partial node
			  then let val defaultSurvivors = LS.intersect(survivors, LL.defaults live)
			        in (* if LS.isEmpty defaultSurvivors
				     then (print "Default: no survivors\n";
					   print "survivors: "; MCPrint.tppLayers survivors;
					   print "live: "; MCPrint.tppLayers live)
				     else (); *)
				    dbsays ["** makeDecisionTree: defaultOp: id = ", Int.toString id,
					    ", defaultSurvivors = ", LS.layerSetToString defaultSurvivors];
				   SOME(makeDecisionTree(candidates, defaultSurvivors, path::dtrace))
				   (* BUG? path added to dtrace does not reflect default _choice_.
				    * Could this cause wrong svar choice in MCCode.genRHS? *)
			       end
			  else NONE  (* no default clause *)
		   in ORinfo.incrementUse id;
		      dbsays ["<< makeDecisionTree: ", Int.toString id, " relevant"];
		      CHOICE{node = node, choices = decvariants, default = defaultOp}
		  end

		| NONE =>
		  (* no relevant OR nodes; pick minimum rule *)
		    (incrementRuleCount (L.toRule minSurvivor);
		     dbsays ["<< makeDecisionTree: NO relevant, DLEAF ", L.layerToString minSurvivor];
		     DLEAF (minSurvivor, rev dtrace))

		| _ => bug "makeDecisionTree"))
	    (* end makeDecisionTree *)

    (* What to do when there are no relevant OR nodes in the queue? In this case,
     * the match will be degenerate (only one irrefutable pattern, in rule 0)? 
     * In this case, return DLEAF 0, which code will translate to rhs0 
     * (the right-hand-side of rule 0), prefaced with required pattern destruction 
     * code. *)

    val dectree = makeDecisionTree (initialOrNodes, initialLayers, nil)
    val ruleCountsVector = Array.vector ruleCounts
    val (maxId, maxCount) = ORinfo.maxUseCount ()
 in dbsays ["<< decisionTree: ruleCounts = ",
	    PrintUtil.listToString ("[", ",", "]") Int.toString
				   (Vector.foldr (op ::) nil ruleCountsVector),
	    "\n  Total Choice Nodes = ", Int.toString (ORinfo.getTotalUses()),
	    ", Max Use Count = (", Int.toString maxId, ", ", Int.toString maxCount, ")"];
    (dectree, ruleCountsVector)
end (* function decisionTree *)

end (* local *)
end (* structure DecisionTree *)
