(* matchcompile.sml *)

(*** building a decision tree from a matchTree ***)
(* 
1. incrementally build a set (or priority queue) of choice-points, starting
   with the top matchTree node.
   - if node is a DataCP (a choice-point), add case split immediately with
     this choice-point
   - if a tuple, scan elements adding choice-points to the set, then pick
     "best" choice-point (DataCP node) according to sort order.

2. at each alternative under the choice, the choice-points used
   along this decision tree branch are encoded by a path (list of lvars).
   These are excluded from being reused in the decision subtrees 
   that are children of this node.
   
3. at each point, the matchTree designates a fresh lvar to be
   bound to the matched value component for further reference at the
   leaves where pattern variables are to be bound. At a tuple node,
   fresh lvars were assigned to each component. Some of these may be
   bound to pattern variables at terminal decision nodes, some may be
   used as subjects for case discrimination [with "as" layered patterns, you
   could have both].

4. the sorting order on matchTrees is as follows:
   a. minimum element of relevance set
   b. minimum overlap of live sets on branches (fewest rules in multiple livesets)
  [c. minimum "width" of branch (i.e. no. of alternatives) - push wide branches down]
  [d. maximum number in relevance set?]

5. a decision branch "terminates" when remaining available choice
   points are not relevant to the minimum live rule number, and that
   minimum rule number is the chosen rule for this leaf node of the
   decision tree.

6. At a terminal point, which selects the earliest live rule, increment the
   rule count and produce the bind pattern variables of selected rule.

7. Any rules that appear in more than one terminal node must have their
   rhs expression abstracted as a function that is then called at that
   point in the case tree.
*)

structure MatchCompile =
struct

structure A = Absyn
open MatchTree

structure Key =
struct
  type ord_key = LVar.uid * LVar.uid
  fun compare ((x1,y1), (x2,y2)) =
      case LVar.compare(x1,x2) 
       of LESS => LESS
	| GREATER => GREATER
	| EQUAL => LVar.compare(y1,y2)
end (* structure Key *)

(* dTree - decision trees *)
datatype dTree
 = Choice of   (* or-node, case analysis on dcon or constants *)
   {name: LVar.uid,   (* name of tested value *)
    cases: branches,
    default : dTree option}
 | Dtuple of   (* tuple destruction *)
   {name: LVar.uid,   (* name of destructed value *)
    childNames: LVar.uid list,   (* bind components to these vars *)
    body: dTree}
 | SingCon of  (* "strip" singular dcon *)
   {name : LVar.uid,
    dcon : dcon,
    dtree : dTree}
 | Terminal of  (* unique rule chosen at a leaf of the decision tree *)
   {rule: int}

and branches
 = DconBr of (dcon * LVar.uid list * dTree) list
 | IntBr of (int * dTree) list
 | StrBr of (string * dTree) list

fun getName (Choice{name,...}) = SOME name
  | getName (Dtuple{name,...}) = SOME name
  | getName (SingCon{name,...}) = SOME name
  | getName _ = NONE

(* precision : matchTree -> int *)
(* number of rules that appear in more than one branch of a choice node *)
fun precision (mtree) = 0   (* dummy implementation *)

(* better : matchTree * matchTree -> bool *)
(* ordering of choice points. 
 * Should this be done relative to a current set of live rules for the point
 * in the decision tree where we are trying to choose a choice point? *)
fun better (mtree1, mtree2) =
    let val rel1 as (rule1::_) = getRelev mtree1    (* not(null(rel1)) *)
	val rel2 as (rule2::_) = getRelev mtree2    (* not(null(rel1)) *)
     in case Int.compare(rule1,rule2) (* compare initial relevant rule *)
	 of LESS => true
	  | GREATER => false
	  | EQUAL =>
	    (case Int.compare(length rel1, length rel2)
	      of LESS => false
	       | GREATER => true
	       | EQUAL =>
		 (case Int.compare(precision mtree1, precision mtree2)
		   of LESS => true
		    | GREATER => false
		    | EQUAL => false))
    end


fun relevant(mt,ruleNo) =
    List.exists (fn r => (r = ruleNo)) (getRelev mt)

(*
fun usedOnPath(mt, path) =
    let val lvar = lvar mt
     in List.exists (fn lv => LVar.eq(lv,lvar)) path
    end
*)

(* selectChoice : int * matchTree list -> (matchTree * matchTree list) option *)
(* find a choice that is relevant to rule in choiceset *)
fun selectChoice (rule, choiceset) =
    let fun findCP (nil, prefix) = NONE
	  | findCP (mt::mts, prefix) =
	    if relevant(mt,rule)
	    then SOME (mt, List.revAppend(prefix,mts))
	    else findCP(mts, mt::prefix)
     in findCP(choiceset,nil)
    end

(* insertChoice : matchTree * matchTree list -> matchTree list *)
(* add a new choice-point to choiceset, maintaining best-first ordering *)
fun insertChoice (mtree, choiceset) =
    let fun ins (cps as mtree'::rest) = 
	      if better(mtree, mtree')
	      then mtree :: cps
	      else mtree' :: ins rest
	  | ins nil = [mtree]
     in ins choiceset
    end

(* intersect two lists of ints (rule numbers) assumed to be in
 * ascending order *)
fun intersect (xs: int list, ys: int list) : int list =
    case (xs,ys)
      of (nil,ys) => nil
       | (xs,nil) => nil
       | (xs' as (x::xs), ys' as (y::ys)) =>
	 (case Int.compare(x,y)
	   of EQUAL => x ::intersect(xs,ys)
	    | LESS => intersect(xs, ys')
	    | GREATER => intersect(xs', ys))

val wildLvar = LVar.new()
val wildVar = A.Var{lvar = wildLvar,
		    name = Symbol.mkSymbol "wild",
		    ty = ref(Type.MONO Basis.errorTy)}


(* matchCompile : (Absyn.rulepat * Absyn.exp) list -> dTree * int list * lvenv *)
(* the toplevel match compile function, taking the Absyn rules making up a case
 * match, and returning a decision tree, a list of counts of how many time each
 * rule rhs is selected by a branch in the decision tree, and the lvenv produced
 * during the construction of the matchtree for the rule patterns. *)
fun matchCompile (rules: (A.rulepat * A.exp) list) =
let val nRules = length rules + 1
    val defaultPat = BindPat(AtomPat(VarPat(wildVar)))
    val pats = map #1 rules @ [defaultPat]
    val (mtree, lvenv) = mkMatchTree pats

(* record how many terminal nodes select for each rule. If 0,
 * then rule is not selected in decision tree and is redundant.
 * If > 0, then there are muliple decision paths to that rule,
 * and rhs has to be eta-expanded into a function, which is called
 * at each terminal node for that rule. *)
val ruleCounts = Array.array(0, length rules)
fun incRuleCount ruleNo = 
    Array.update(ruleCounts, ruleNo, Array.sub(ruleCounts, ruleNo) + 1)

(* mkDtree : int list * LVar.uid list * matchTree -> dTree *)
(* mkDtree(rules, path, mtree): top level function for constructing decision tree 
 *   rules : list of live rules at this point in decision tree (initially all)
 *   path : inherited path to this dtree node (initially empty)
 *   mtree : starting matchTree node for translation (initially full matchTree)
 * Build a decision tree after a choice, or for the top-level matchTree.
 * ASSERT: rules intersect live(mTree) should be nonempty. *)
fun mkDtree (rules: int list, path : LVar.uid list, available: matchTree list,
	     mTree: matchTree) : dTree =
    case mTree 
     of (DataCP _ | IntCP _ | StrCP _) => (* a choice point node *)
        (* make this choice-point available and dispatch *)
	let val available' = insertChoice(mTree,available)
	 in dispatch (rules, path, available')
	end
      | Tuple {children, lvar, vars, ...} =>
	let val childNames = map getLvar children
            (* this version does not deal with nested tuples *)
	    val available' = foldl insertChoice available children
	 in Dtuple{name = lvar, childNames = childNames,
		   body = dispatch(rules, insert(lvar,path), available')}
	end
      | Single{dcon,lvar,vars,tree} => 
	SingCon{dcon = dcon, name = lvar,
		dtree = mkDtree(rules, insert(lvar,path), available, tree)}
      | Var{live,lvar,...} => dispatch(rules, path, available)
         (* rules = live ? *)
      | Leaf{live} => dispatch(rules, path, available)
      | _ => bug "mkDtree"

(* dispatch : int list * LVar.uid list -> dTree
 * dispatch (rules, path)
 *  rules: the set of live rules at this point
 *  path : the decision tree path recording what decisions have been made
 *         up to this point.
 * Use the best of the available choice points to extend branches of
 * the decision tree for each choice *)
and dispatch (rules: int list, path: LVar.uid list, available: matchTree list)
             : dTree  =
    case selectChoice (hd rules, available)
     of SOME (mtree, available') =>
	(* mtree is a choice point: DataCP, IntCP, or StrCP *)
        (case mtree
	  of DataCP{lvar, vars, variants, remainder, defaults, ...} =>
	       let val path' = insert(lvar,path)
		   fun mkbranch(dcon, mtree) =
		       let val rules' = intersect(rules,getLive(mtree))
			   val lvars = getLvars mtree
		        in (dcon, lvars, mkDtree(rules',path', available', mtree))
		       end
	       in Choice{name=lvar,
			 cases = DconBr(map mkbranch variants),
			 default =
			   (case defaults
			      of nil => NONE
			       | _ => SOME(dispatch(defaults,path',available')))}
	      end
	  | IntCP{lvar, vars, variants, defaults,...} =>
	      let val path' = insert(lvar,path)
		   fun mkbranch(n, rules') =
		       let val rules'' = intersect(rules,rules')
		        in (n, dispatch(rules'',path',available'))
		       end
	       in Choice{name=lvar,
		         cases = IntBr(map mkbranch variants),
		         default =
			   (case defaults
			      of nil => NONE
			       | _ => SOME(dispatch(defaults,path',available')))}
	      end
	  | StrCP{lvar, vars, variants, defaults,...} =>
	      let val path' = insert(lvar, path)
		   fun mkbranch(s, rules') =
		       let val rules'' = intersect(rules,rules')
		        in (s, dispatch(rules'',path',available'))
		       end
	       in Choice{name=lvar,
		         cases = StrBr(map mkbranch variants),
		         default =
			   (case defaults
			      of nil => NONE
			       | _ => SOME(dispatch(defaults,path',available')))}
	      end
	  | _ => bug "dispatch 1")
      | NONE => (* no choices relevant to top rule => Terminal *)
	  (case rules
	    of rule::_ => 
	         (incRuleCount rule;
	          Terminal{rule = rule})
	     | nil => bug "dispatch 2")

val dtree = mkDtree(getLive(mtree), nil, nil, mtree)
val ruleCounts' = Array.foldr (op ::) nil ruleCounts

 in (dtree, ruleCounts', lvenv)
end (* fun matchCompile *)

end (* structure MatchCompile *)

(* Notes:

1. Choice-points (D,I,S mtree nodes) can be used multiple
times, but only once on a particular dtree branch. Thus we pass a
path parameter that records which choice points have been used in
the decision tree branch above this point.

2. On a branch, choice-points must obey their ordering as derived from
their dominance relations on the mtree. I.e., if cp1 < cp2 (appears
higher in mtree), then cp1 < cp2 on any branch of the dtree (cp2
applies to structure only accessible after cp1 has been dispatched).

*)

