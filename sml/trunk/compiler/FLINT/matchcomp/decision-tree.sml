(* decision-tree.sml *)
(* constructing a decision tree from choices *)

structure DecisionTree =
struct

exception PickBest

fun relevant (CHOICE{defaults,...}, rulenum) =
      not (R.member(rulenum, defaults))
  | relevant (BND _, _) =
      bug "relevant - unexpected BINDDEC arg"

(* numberChoices : choiceKind -> int *)
fun numberChoices (DATAchoices l) = length l
  | numberChoices (VECchoices l = length l
  | numberChoices (CONSTchoices l) = length l

(* metric : choice -> int * int *)
fun metric (CHOICE{choices, defaults)) = (length defaults, numberChoices choices)
  | metric (BND _) = bug "metric - unexpected BINDDEC arg"

fun metricBetter((a:int,b:int),(c,d)) = a < c orelse (a = c andalso b < d)

fun singleDcon (CHOICE{choices=DATAchoices((dcon, _, _)::_) =
    TU.dataconCount dcon = 1   (* dataconCount in TypesUtil *)
  | singleDcon _ = false

(* CSIG(0,1) means 0 dcons tagged, 1 dcon untagged => single dcon in datatype
   CSIG(1,0) is the reverse: 1 dcon tagged, 0 dcon untaged => single dcon in datatype
   -- assuming in CSIG(n,m) the total number of dcons is n+m, i.e. all dcons are either
      tagged or untagged *)
(* pickBest0 : choice list * ruleset * int * (int * int) option * ruleno option *) 
fun pickBest0(nil, _, _, _, NONE) = raise PickBest
  | pickBest0(nil, _, _, _, SOME n) = n
  | pickBest0((BND _)::rest, _, n, _, _) = n
  | pickBest0(choice::rest, live, n, NONE, NONE) =
    if singleDcon choice then n
    else if relevant (choice, R.minItem live)
    then pickBest0(rest, live, n + 1, SOME(metric choice), SOME n)
    else pickBest0(rest, live, n + 1, NONE, NONE)
  | pickBest0(choice::rest, live, n, SOME m, SOME i) =
    if singleDcon choice then n
    else if relevant (choice, R.minItem live)
    then let val myMetric = metric choice
	  in if metricBetter(myMetric, m)
	     then pickBest0(rest, live, n + 1, SOME(myMetric), SOME n)
	     else pickBest0(rest, live, n + 1, SOME m, SOME i)
	 end
    else pickBest0(rest, live, n + 1, SOME m, SOME i)
  | pickBest0 _ = bug "pickBest0 - unexpected arg"

(* pickBest : choice list * rules -> ruleno *)
fun pickBest (choices, live) = pickBest0(choices, live, 0, NONE, NONE)

(* extractNth : int * 'a list -> 'a * 'a list *)
fun extractNth(0, a::b) = (a, b)
  | extractNth(n, a::b) =
      let val (c,d) = extractNth(n - 1, b) in (c, a::d) end
  | extractNth _ = bug "extractNth - n too big"

(* what does fireConstraint do?  constraints have disappeared! *)
(* fireConstraint : path
 *                  * (path list * decision list) list     -- (needPaths, decisions) list
 *                  * decision list                        -- ready list
 *                  * (path list * decision list) list     -- delayed list
 *                  -> decision list * (path list * decision list) list *)
fun fireConstraint (path, (needPaths, decisions)::rest, ready, delayed) =
      (case removePath(path, needPaths)
         of nil => fireConstraint(path, rest, decisions@ready, delayed)
          | x => fireConstraint(path, rest, ready, (x,decisions)::delayed))
  | fireConstraint (path, nil, ready, delayed) =
      (ready, delayed)

(* genDecisionTree : (choice list * (path list * choice list) list) * ruleset
 *                   -> dectree *)
fun genDecisionTree((choices, delayed), live) =
      ((case extractNth(pickBest(choices, live), decisions)
         of (BND(path, _), rest) =>
	      genDecisionTree(fireConstraint(path,delayed,rest,nil),live)
          | (CHOICE{path, defaults, choices}, rest) =>
	     (* case choices  (* disregard degenerate (single constructor) DATA choices *)
                  of DATAchoices [(dcon,_,guarded)] =>
                       if singleDcon dcon
                       then genDecisionTree(rest@guarded, delayed), live)
		       else		       
             *)
            let fun isLive(CHOICE{ruleset,...}) = not(R.isEmpty(R.intersect(ruleset, live)))
		  | isLive _ = false (* ??? *)
                 val activeChoices = List.filter isLive choices
                 val caseTrees =
                   gencases(activeChoices, rest, delayed, defaults, live)
                 val defActive = R.intersect(live, defaults)
		 val branching =
		     case choices
		      of  DATAchoices((dcon,_,_)::_) =>
			  dataconBranching dcon
		       |  _  =>  0
		 val defTreeOp =
                     if length activeChoices = branching then NONE
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

(* 0.93 version of genDecisionTree *)
fun genDecisionTree((decisions, delayed), active as active1::_) =
      ((case extractNth(pickBest(decisions, active), decisions)
	 of (BINDDEC(path, _), rest) =>
	      genDecisionTree(fireConstraint(path,delayed,rest,nil),active)
	  | (CASEDEC(path, [sign], [(_,_,_,guarded)], defaults), rest) => 
	      genDecisionTree((rest@guarded, delayed), active)
	  | (CASEDEC(path, sign, cases, defaults), rest) =>
	      let 
		fun isActive(_,_,rules,_) = intersect(rules, active) <> []
		val activeCases = filter(isActive, cases)
		val caseTrees = 
		     gencases(activeCases, rest, delayed, defaults, active)
		val defActive = intersect(active, defaults)
		val defTree = 
		      if length activeCases = length sign then NONE 
		      else
			SOME (genDecisionTree((rest, delayed), defActive))
	      in 
		CASETEST(path, sign, caseTrees, defTree)
	      end
	  | (ABSCONDEC(path, con, yes, guarded, defaults), rest) =>
	      let 
		val yesActive = intersect(active, union(yes, defaults))
		val noActive = intersect(active,defaults)
		val yesTree = 
		      genDecisionTree((rest@guarded, delayed), yesActive)
		val defTree = genDecisionTree((rest, delayed), noActive)
	      in 
		if unary con then ABSTEST1(path, con, yesTree, defTree)
		else ABSTEST0(path, con, yesTree, defTree)
	      end)
       handle PickBest => (RHS active1))
  | genDecisionTree (_,active) = impossible "nothing active"

and gencases (nil, decs, delayed, defaults, active) = nil
  | gencases ((const,t,rules,guarded)::rest,decs,delayed,defaults,active)= 
      let 
	  val rActive = intersect(union(defaults, rules), active)
      in 
	(const, t, genDecisionTree((decs@guarded, delayed),rActive))
	  :: (gencases(rest,decs,delayed,defaults,active))
      end



end (* structure DecisionTree *)
