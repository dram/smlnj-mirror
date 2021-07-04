(* FLINT/trans/decisiontree.sml *)

(* building decision trees *)

structure DecisionTree =
struct

local

  structure DA = Access
  open MCCommon
  structure RS = RuleSet

  val debugging = FLINT_Control.mcdebugging

  val say = Control_Print.say
  fun newline () = say "\n"
  fun saynl msg = (say msg; newline())
  fun says strings = saynl (concat strings)

  fun dbsay msg =
      if !debugging
      then (say msg; newline())
      else ()
  fun dbsays msgs = dbsay (concat msgs)

  fun bug msg = ErrorMsg.impossible ("DecisionTree: " ^ msg)

  fun signToWidth (DA.CSIG(i,j)) = i+j
    | signToWidth DA.CNIL = Types.infinity

in

(* relevant : andor * ruleno -> bool *)
(*   the andor node is "relevant" to the rule/pattern indexed by ruleno *)
fun relevant (OR{defaults,...}, ruleno) = not (RS.member(defaults, ruleno))
  | relevant _ = bug "relevant - ORd expected"

(* metric : andor -> int (>= 0) -- the metric for the ORd node *)
fun metric (OR {cases, defaults,...}) = 1000 * (RS.numItems defaults) + length cases
  | metric _ = bug "metric - OR expected"

(* Node metric (int):
 * Better: smaller number of defaults, or equal number of defaults and smaller number
 * of cases; returns true if first arg is (strictly) better than second *)
(* fun better((a:int,b:int),(c,d)) = a < c orelse (a = c andalso b < d) *)
fun better (m1, m2) = Int.< (m1, m2)

(* --------------------------------------------------------------------------- *)
(* ORnode queues ordered by metric *)

(* nodeQueue: a list of (metric, node), where node: andor[OR] and metric is metric(node),
   ordered by "better" order on the metric, i.e. best is first element of queue. *)
type nodeQueue = (int * andor) list

(* emptyQ : nodeQueue *)
val emptyQ : nodeQueue = nil

(* insertQ : andor[OR] * nodeQueue -> nodeQueue *)
fun insertQ (andor, nodeQ) =
    let val m0 = metric andor
	fun ins (queue as ((m1, node)::queue')) =
	    if better (m0, m1)
	    then (m0, andor) :: queue
	    else (m1, node) :: ins queue'
	  | ins nil = [(m0, andor)]
     in ins nodeQ
    end

(* insertListQ : andor[OR] list * nodeQueue -> nodeQueue *)
fun insertListQ (nodes, nodeQ) = foldr insertQ nodeQ nodes

(* selectBest : (andor -> bool) * nodeQueue -> (andor * nodeQueue) option *)
(* returns the node with the best metric that satisfies the predicate pred *)
fun selectBest (pred, nodeQ) =
    let fun scan (nil, _) = NONE
	  | scan ((head as (_, node)) :: nodeQ, passed) = 
	    if pred node
	    then SOME (node, List.revAppend (passed, nodeQ))
	    else scan (nodeQ, head :: passed)
    in scan (nodeQ, nil)
    end

(* bestOrNode : nodeQueue * ruleset = (andor * nodeQueue) option *)
fun bestOrNode (nodeQ, active) = 
    let val minActive = RS.minItem active
	fun pred node = relevant (node, RS.minItem active)
     in selectBest (pred, nodeQ)
    end
 
(* accessible OR nodes *)
(* accessible : andor * nodeQueue -> nodeQueue *)
(* inserts the "accessible" OR nodes in the argument andor tree into the nodeQ *)
fun accessible (andor, nodeQ) =
    (case andor
       of LEAF _ => nodeQ
        | AND {children, ...} =>
	    accessibleList (children, nodeQ)
        | OR _ => insertQ(andor, nodeQ))

(* accessibleList : andor[OR] list * nodeQueue -> nodeQueue *)
and accessibleList (andors, nodeQ) = foldl accessible nodeQ andors

(* mkDecisionTree : (nodeQueue * ruleset -> dectree *)
(* "active" rules = rules still "in play" = rules surviving earlier choices" ?? *)
fun mkDecisionTree (nodeQ: nodeQueue, active: ruleset) =
      if RS.isEmpty active then bug "mkDecisionTree: active empty" else
      (case bestOrNode(nodeQ, active)
         of SOME (OR{path, sign, cases, defaults, ...}, remainder) =>
             let fun isActive(_,rules,_) = not (RS.isEmpty (RS.intersection(rules, active)))
                 val activeCases = List.filter isActive cases
                 val caseTrees = gencases(activeCases, remainder, defaults, active)
                 val defActive = RS.intersection (active, defaults)
		 val defTree =
                     if length activeCases = signToWidth sign
		     then NONE (* cases are "saturated" -- cover all possible cons *)
                     else SOME (mkDecisionTree (remainder, defActive))
			  (* default needed to cover cons not in any variant *)
              in CHOICE {path=path, sign=sign, cases=caseTrees, default=defTree}
             end
	    | NONE => RHS (RS.minItem active))

(* gencases : caseD list * nodeQueue * ruleset * ruleset -> (con * dectree) list *)
and gencases (cases: variant list, nodeQ: nodeQueue, defaults: ruleset, active: ruleset) =
    let fun genSubCase (con, rules, subcase) =
	    let val caseActive = RS.intersection (RS.union (defaults, rules), active)
	        val dectree =
		    case subcase
		      of CONST => mkDecisionTree (nodeQ, caseActive)
		       | DCON node => mkDecisionTree (accessible (node, nodeQ), caseActive)
		       | VEC velements => 
			   mkDecisionTree (accessibleList (velements, nodeQ), caseActive)
	     in (con, dectree)
	    end
     in map genSubCase cases
    end

(* genDecisionTree: andor * ruleset -> dectree *)
	  (* initially no delayed: delayed = nil *)
fun genDecisionTree (andor, active) =
    mkDecisionTree (accessible (andor, emptyQ), active)

end (* local *)
end (* structure DecisionTree *)
