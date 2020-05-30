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
     (* if you get to this node, bind variables along branch and execute ruleno RHS *)
  | BRANCH of
    {node : andor,  (* an OR node used for dispatching *)
     children : decisionTree list}

(* need to recover and traverse AND structure for selecting values to test. Can
 * recover from the original andor tree? *)

(* makeDecisionTree : andor * andor list -> decisionTree *)
(* makeDecisionTree takes the original andor tree and the ordered list of OR nodes
 * and constructs a tree of OR nodes based on the path dependencies among the OR nodes. *)
fun makeDecisionTree 


(* makeDecisionTree : (choice list * ruleset -> decisionTree *)
fun makeDecisionTree(orNodes, live) =
      (case selectBest(orNodes, live)
        of SOME (OR{path, defaults, children}, rest) =>
	     (* case orNodes
                  of DATAorNodes [(dcon,_,guarded)] =>
                       if singleDcon dcon
                       then genDecisionTree(rest@guarded, delayed), live)
		       else		       
             *)
           let fun isLive(OR{ruleset,...}) = not(R.isEmpty(R.intersect(ruleset, live)))
		 | isLive _ = false (* ??? *)
                 val activeOrNodes = List.filter isLive orNodes
                 val caseTrees =
                   gencases(activeOrNodes, rest, delayed, defaults, live)
                 val defActive = R.intersect(live, defaults)
		 val branching =
		     case orNodes
		      of  DATAorNodes((dcon,_,_)::_) =>
			  dataconBranching dcon
		       |  _  =>  0
		 val defTreeOp =
                     if length activeOrNodes = branching then NONE
                     else SOME (genDecisionTree((rest, delayed), defActive))
              in DCHOICE{path=path, caseTrees, default=defTreeOp)
             end
	  | NONE =>  (RHS (R.minItem live)))
  | genDecisionTree (_,nil) = bug "genDecisionTree - nil active"

and gencases (nil, decs, defaults, active) = nil
  | gencases ((pcon,rules,guarded)::rest,decs,defaults,active)=
      let val rActive = R.intersect(R.union(defaults, rules), active)
       in (pcon, genDecisionTree((decs@guarded),rActive))
          :: (gencases(rest,decs,defaults,active))
      end


(* ================================================================================ *)

(* old version of genDecisionTree *)
(* genDecisionTree : (choice list * (path list * choice list) list) * ruleset
 *                   -> dectree *)
fun genDecisionTree((orNodes, delayed), live) =
      ((case extractNth(pickBest(orNodes, live), orNodes)
         of (BND(path, _), rest) =>
	      genDecisionTree(fireConstraint(path,delayed,rest,nil),live)
          | (CHOICE{path, defaults, orNodes}, rest) =>
	     (* case orNodes
                  of DATAorNodes [(dcon,_,guarded)] =>
                       if singleDcon dcon
                       then genDecisionTree(rest@guarded, delayed), live)
		       else		       
             *)
            let fun isLive(OR{ruleset,...}) = not(R.isEmpty(R.intersect(ruleset, live)))
		  | isLive _ = false (* ??? *)
                 val liveOrNodes = List.filter isLive orNodes
                 val caseTrees =
                   gencases(activeOrNodes, rest, delayed, defaults, live)
                 val defActive = R.intersect(live, defaults)
		 val branching =
		     case orNodes
		      of  DATAorNodes((dcon,_,_)::_) =>
			  dataconBranching dcon
		       |  _  =>  0
		 val defTreeOp =
                     if length activeOrNodes = branching then NONE
                     else SOME (genDecisionTree((rest, delayed), defActive))
              in DCHOICE{path=path, caseTrees, default=defTreeOp)
             end
       handle PickBest => (RHS (R.minItem live))
  | genDecisionTree (_,nil) = bug "genDecisionTree - nil active"

and gencases (nil, decs, delayed, defaults, active) = nil
  | gencases ((pcon,rules,guarded)::rest,decs,delayed,defaults,active)=
      let val rActive = R.intersect(R.union(defaults, rules), active)
       in (pcon, genDecisionTree((decs@guarded, delayed),rActive))
          :: (gencases(rest,decs,delayed,defaults,active))
      end


