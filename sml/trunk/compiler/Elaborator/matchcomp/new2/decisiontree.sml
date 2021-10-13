(* FLINT/trans/decisiontree.sml *)
(* revised "old" match compiler *)

(* building decision trees *)

structure DecisionTree =
struct

local

  structure DA = Access
  open MCCommon
  structure RS = RuleSet

  val debugging = MCControl.mcdebugging

  fun bug msg = ErrorMsg.impossible ("DecisionTree: " ^ msg)

  val say = Control_Print.say
  fun newline () = say "\n"
  fun saynl msg = (say msg; newline())
  fun says strings = saynl (concat strings)

  fun dbsay msg =
      if !debugging
      then (say msg; newline())
      else ()
  fun dbsays msgs = dbsay (concat msgs)

  (* signToWidth : DA.sign -> int
   *  calculate the number of datacons in a datatype ("width") from the
   *  DA.sign value (a component of a datacon).
   *  NOTE: This belongs in TypesUtil. *)
  fun signToWidth (DA.CSIG (i, j)) = i+j
    | signToWidth DA.CNIL = Types.infinity

in

(* relevant : andor * ruleno -> bool *)
(*   the andor node is "relevant" to the rule/pattern indexed by ruleno *)
fun relevant (OR{defaults,...}, ruleno) = not (RS.member(defaults, ruleno))
  | relevant _ = bug "relevant - ORd expected"

(* metric : andor -> int (>= 0)
 *   The "quality" metric for the OR node. Only defined for OR nodes. *)
fun metric (OR {cases, defaults,...}) = 10000 * (RS.numItems defaults) + length cases
  | metric _ = bug "metric - OR expected"

(* Node metric (int):
 *  Better: smaller number of defaults, or equal number of defaults and smaller number
 *  of cases; returns true if first arg is (strictly) better than second. Pair of
 *  metrics encoded as a single int.
 *    The intuition is that (1) more defaults makes an OR node less discriminating,
 *    and (2) favoring fewer number of cases makes the dectree less "bushy".
 *  *)

(* better: int * int -> bool *)
(* fun better((a:int,b:int),(c,d)) = a < c orelse (a = c andalso b < d) *)
fun better (m1, m2) = Int.< (m1, m2)

(* --------------------------------------------------------------------------- *)
(* ORnode queues ordered by metric -- simple version of a priority queue *)

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
fun bestOrNode (nodeQ, live) = 
    let val minLive = RS.minItem live
	fun pred node = relevant (node, RS.minItem live)
     in selectBest (pred, nodeQ)
    end
 
(* accessible OR nodes *)
(* accessible : andor * nodeQueue -> nodeQueue *)
(* inserts the "accessible" OR nodes in this andor tree into the nodeQ *)
fun accessible (andor, nodeQ) =
    (case andor
       of VAR _ => nodeQ  (* no OR nodes here *)
        | AND {children, ...} =>
	    accessibleList (children, nodeQ)  (* add OR nodes from children *)
        | OR _ => insertQ(andor, nodeQ))  (* insert this OR node into queue *)

(* accessibleList : andor[OR] list * nodeQueue -> nodeQueue *)
and accessibleList (andors, nodeQ) = foldl accessible nodeQ andors

(* mkDecTree : (nodeQueue * ruleset -> dectree *)
(* "live" rules = rules still "in play" = rules surviving earlier choices" ?? *)
fun mkDecTree (nodeQ: nodeQueue, live: ruleset) =
      if RS.isEmpty live then FAIL else  (* no live rules, raise MATCH/BIND/unhandled exception *)
      (case bestOrNode(nodeQ, live)
         of (SOME(node as (OR{id, typ, sign, cases, defaults}), remainder)) =>
             let val dtCases = mkcases (cases, remainder, defaults, live)
                 val liveDefaults = RS.intersection (live, defaults)
		 val defTree =
                     if length cases = signToWidth sign
		     then NONE (* cases are "saturated" -- cover all possible cons *)
                     else SOME (mkDecTree (remainder, liveDefaults))
			  (* default needed to cover cons not in any variant *)
              in CHOICE {andor=node, sign=sign, cases=dtCases, default=defTree}
             end
	  | NONE => RHS (RS.minItem live))
             (* no further choices => no more rules can be eliminated *)

(* mkcases : variant list * nodeQueue * (defaults: ruleset) * (live: ruleset) -> (con * dectree) list
 *  ASSERT: (1) length of result (decTree) cases = length of input (OR-node) cases, and
 *          (2) corresponding elements of cases and result have the same con element. *)
and mkcases (cases: variant list, nodeQ: nodeQueue, defaults: ruleset, live: ruleset) =
    let fun genSubCase (con, rules, subcase) =
	    let val caseLive = RS.intersection (rules, live)
		val caseLiveWithDefaults = RS.intersection (RS.union (defaults, rules), live)
                    (* ASSERT: not (empty caseLive) => not (empty caseLiveWithDefaults, because
                       caseLive \subset caseLiveWithDefaults *)
	        val dectree =
		    if RS.isEmpty caseLiveWithDefaults
		       (* caseLive *) (* if this con is matched, the match has failed. defaults? *)
		    then FAIL
		    else case subcase
			  of CONST => mkDecTree (nodeQ, caseLiveWithDefaults)
			       (* no destruct; go to next CHOICE *)
			   | DCON andor => mkDecTree (accessible (andor, nodeQ), caseLiveWithDefaults)
			   | VEC velements => 
			     mkDecTree (accessibleList (velements, nodeQ), caseLiveWithDefaults)
	     in (con, dectree)
	    end
     in map genSubCase cases
    end

(* genDecisionTree: andor * ruleset -> dectree
 *  initially: live = allRules *)
fun genDecisionTree (andor, live) =
    mkDecTree (accessible (andor, emptyQ), live)

(* =========================================================================== *)
(* collecting decision tree stats *)
	      
structure NodeMap = IntRedBlackMap

type decTreeStats =
     {rulesUsed : RS.set,
      failures : int,
      choiceTotal : int,
      choiceDist : int NodeMap.map}

(* decTreeStats : dectree * int -> decTreeStats
 *  returns the set of all rules used in the dectree, maintaining ordering
 *  (because union operation does). The boolean value indicates presence of FAIL,
 *  signalling that the rules are non-exhaustive. *)
fun decTreeStats (dectree: dectree): decTreeStats =
    let val rules = ref RS.empty
	val failures = ref 0
	val choiceTotal = ref 0
	val choiceDist = ref NodeMap.empty
	fun scanTree (RHS n) = (rules := RS.add (!rules, n)) 
	  | scanTree FAIL = (failures := !failures + 1)
	  | scanTree (CHOICE {andor, cases, default, ...}) =
	    (choiceTotal := !choiceTotal + 1;
	     choiceDist :=
		let val nmap = !choiceDist
		    val nodeId = getId andor
		    val newcount =
			case NodeMap.find (nmap, nodeId)
			 of NONE => 1
			  | SOME k => k+1
		in NodeMap.insert(nmap, nodeId, newcount)
		end;
	     app (fn (_, dt) => scanTree dt) cases;
	     Option.app scanTree default)
    in scanTree dectree;
       {rulesUsed = !rules,
	failures = !failures,
	choiceTotal = !choiceTotal,
	choiceDist = !choiceDist}
    end

end (* local *)
end (* structure DecisionTree *)
