(* flatten-andor.sml *)

(*
Translate an andor tree into a flattened list of "decision" or "choice" trees (CHOICE) and
binding nodes (BND)

  -- What is the role of defaults ruleset?  It indicates what other rules might match if
     this particular rule fails?
  -- What is the "live" ruleset?  The rules (& hence patterns) which still have a chance 
     to match, given the "choices" that have been made in traveling to this node,
     i.e rules/patterns that haven't been ruled out by earlier commitments (like choosing
     a cons branch rather than a nil branch when matching a list at a location in a pattern.
  -- Paths designate points in the "pattern space" generated (e.g. by AND-OR analysis),
     but points that are not meaningful for all patterns -- they are dependent on certain
     choices that have been made in the branching choice structure.

"Flattening" consists of:
  (1) flattening nested products (records and tuples) -- any top-level nested ANDS
      are collapsed into lists
  (2) interspercing BND values representing variable bindings (both AS and VARS),
      so AS and VARS are both translated into a "leading" BND element added to
      the front of a decision list for the other, non-variable patterns at this
      position
  (3) translating the OR nodes (DATA, VEC, CONST) into CHOICE nodes (with three
      variants of "choice" fields for the three types of OR nodes.

The BND nodes and CHOICE nodes are the basic elements of the flattened products,
which are lists of BND and CHOICE nodes.  The BND and choice nodes are annotated
with

  (a) paths, which capture their possition in the pattern space
  (b) rulesets

The rulesets for BND nodes contain the rules in which the bound variables occur (?).

The rulesets for CHOICE nodes, called "defaults" capture (what?).

The "live" ruleset parameter in these functions represent the rules that might
still be matched at this point, i.e. the rules that are still "in play".  The live
rules can be narrowed down when we go down a dcon branch, eliminating rules inconsistent
with that particular dcon.

Record/tupling structure is being lost in the flattening, but paths are added that
provide an alternative, indirect record of the original structure.

Q: Would it be possible to translate back from a decision list to the AND-OR tree?
No, because information is discarded in the flattening process.  For instance, the
actual variables (and their attributes like lvars) are not included in the BND nodes
in the decision structure.  This information is availabe in VARS nodes of AND-OR trees,
but it is dropped in the flattening translation.

Q: Is this loss of variables a good idea?  We have to work to recover it later.

 *)

structure FlattenAndOrs =
struct

local
    structure R = Rules
    open MCCommon
in

(* flatten : andor -> andor list *)
(* collect the "accessible" OR nodes, i.e. all OR nodes that are reachable from 
 * the root without passing through another OR node. If there are no OR nodes, 
 * flatten returns nil. *)
fun flatten (AND{children,...}) =
    List.concat (map flatten children)
  | flatten {andor as OR _) = [andor]
  | flatten (VAR _) = []

(* compareOrNodes : andor * andor -> bool *)
(* compareOrNodes (node1, node2) returns true if node1 is "better" than node2.
 * This is where the heuristics for match optimization come into play.
 * Its role is similar to MCOpt.opt in the 0.33, 0.44 compilers. *)
fun compareOrNodes (orNode1, orNode2) = ...

(* selectBestOr : andor list -> andor option * andor list *)
(* the argument is a list or OR nodes, and selectBest returns the "best" OR node
 * and the remainer of the OR nodes in the argument supplemented with the OR nodes
 * that are "accessible" from the chosen best OR node *)
fun selectBestOr (nil) = (NONE, nil)  (* only happens if there are no accessible OR nodes *)
  | selectBestOr [orNode] = (SOME orNode, nil)  (* only one choice *)
  | selectBestOr (orNode::rest) = ...

(* orderedOrNodes : andor -> andor list *)
(* orderedOrNodes iterates selectBestOr	until the OR nodes are exhausted, returning a
 * list of all OR nodes in the argument ordered by the compareOrNodes relation *)
fun orderedOrNodes andor =
    let val accessible = flatten andor
    in  case accessible
	 of nil => (* no OR nodes *)
	  | [orNode] => accessible (* only one OR node to choose from *)
    end

(* ================================================================================ *)
(* decision tree construction (possibly a separate module)*)

(* the decision tree is simply a structure ordering OR nodes according to the combination
 * of the "best" (compareOrNodes) ordering and the path suffix relation.  Note that OR
 * nodes can have multiple  occurrences in the decision tree (being duplicated in multiple
 * branches under an earlier choice node. Note that the idea is to refer to the OR nodes
 * from the original andor tree rather than creating new, redundant representations of these
 * choices. *)
	
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


(* ================================================================================ *)
(* old flatten-andor.sml code *)
	
(* bindingRules : binding list * ruleset -> ruleset *)
fun bindingRules (bindings, live) = 
    let val range = foldr (fn ((_,r),set) => R.add(set,r)) R.empty bindings
     in R.intersect(range,live)
    end

(* flattenAndor : andor * path * ruleset -> decision list *)
and flattenAndor (AND andors, path, live) =
      let fun doAndors (i, nil) = nil
	    | doAndors (i, andor::rest) =
	        flattenAndor(andor,SELR(i,path),live) @ doAndors(i + 1, rest)
      in  doAndors (0, andors)
      end
  | flattenAndor (DATA dataBranches, path, live) =
      let val defaults = R.difference(live, R.unionList (map #3 dataBranches))
	  val choices = DATAchoice(map (fn db => flattenDATA(db,path,live,defaults)) dataBranches)
      in  [CHOICE{choices = choices, path = path, defaults = defaults}]
      end
  | flattenAndor (VEC vecBranches, path, live) =
      let val defaults = R.difference(live, R.unionList (map #3 vecBranches))
	  val choice = VECchoices(map (fn vb => flattenVEC(vb,path,live,defaults)) vecBranches)
      in  [CHOICE{choices = choices, path = path, defaults = defaults}]
      end
  | flattenAndor (CONST constBranches, path, live) =
      let val defaults = R.difference(live, R.unionList (map #2 constBranches))
	  val choice = CONSTchoice(map (fn cb => flattenCONST(cb,path,live,defaults)) constBranches)
      in  [CHOICE{choices = choices, path = path, defaults = defaults}]
      end
  | flattenAndor ((AS(andor,bindings) | VARS(andor,bindings)), path, live) = 
      let val rules = bindingRules(bindings, live)
	    (* rules containing some variable binding at this path *)
	  val choices = flattenAndor(andor,path,live)
       in if R.empty rules
	  then choices
	  else BND(path,rules) :: choices
      end
  | flattenAndor (VAR, path, live) = []  (* ? *)


(* flattenVEC : vecBranch -> vecAlt
 * flattenVEC : flatten one vector branch
 * andor in the vecBranch is an AND, representing the list of vector elements *)
and flattenVEC ((veclength, AND andors, ruleset), path, live, defaults) =
      let val stillLive = R.intersect(R.union(ruleset, defaults), live)
	  val ruleLive = R.intersect(ruleset, live)
	  fun flattenVecElements (i, nil) = nil
	    | flattenVecElements (i, andor::rest) = (* flattenAndor andor *)
	      flattenAndor(andor, SELV(i,VECP(veclength,path)), stillLive)
	      @ flattenVecElements(i + 1, rest)
          (* alternatively, if we don't need to distinguish between records and vectors:
          val decisions = flattenAndor(andor, CHOICEV(n,path), stillLive) 
           -- this would use SELR instead of SELV in the paths *)
       in (veclength, ruleLive, flattenVecElements(0, andors))
      end

(* flattenDATA : dataBranch -> dataAlt
 * flatten one data branch *)
and flattenDATA (dcon, (*tvs, *) ruleset, andorOp), path, live, defaults) =
      let val stillLive = R.intersect(R.union(ruleset, defaults), live)
	  val ruleLive = R.intersect(ruleset, live)
      in case andorOp (* depends on whether dcon is constant *)
	   of SOME andor => (* dcon has argument, represented by andor *)
	        (dcon,ruleLive,flattenAndor(andor,DATAP(dcon,path),stillLive))
	    | NONE => (dcon, ruleLive, []) (* constant dcon, no descendents *)
      end

(* flattenCONST : constBranch -> constAlt *)
and flattenCONST ((constCon,ruleset), path, live, defaults) =
      (constCon, R.intersect(ruleset, live))  (* no path, no use of CHOICEC *)

(* flattenAndors : (path * andor) list * ruleset -> (path * decision list) list *)
fun flattenAndors (path_andors, rules) =
    map (fn ((path,andor) => (path, flattenAndor(andor, path, rules))) path_andors

end (* local open ... *)
end (* structure FlattenAndOrs *)
