(* match.sml *)

structure MatchComp = 
struct

(*** match trees ***)
(* This datastructure summarizes the information in an ordered set of patterns. 
 * It allows us to determine the test points, and which rules each test point is
 * relevant to.  A test point is a point where the focus is on a datatype value
 * and a case dispatch can be made on the dataconstructors.
 *)

datatype matchTree
  = Data of  (* datatype dcons choice *)
    {live : int list,   (* rule patterns matchable at this point *)
     relev : int list,  (* rules for which this choice point is "relevant"; not nil *)
     path : lvar list,
     vars : (var * int) list,  (* variables bound to this point, with rule no. *)
     variants: (dcon * matchTree) list, (* the branches -- an OR node *)
     remainder: dcon list} (* datacons not covered by variants *)
  | IntCase of  (* integer liternal choice *)
    {live : int list,           (* rules live at this point *)
     relev: int list,           (* rules for which relevant; not nil *)
     path : lvar list,
     vars : (var * int) list,   (* variables bound at this point *)
     consts: (int * int list) list}  (* int literals, with matching rules *)
  | StrCase of  (* string literal patterns *)
    {live : int list,           (* rules live at this point *)
     relev: int list,             (* rules for which relevant; not nil *)
     path : lvar list,
     vars : (var * int) list,     (* variables bound at this point *)
     consts: (string * int list) list} (* string literals, with matching rules *)
  | Single of  (* singular datacon app *)
    {dcon: dcon,  (* the singleton dcon of the datatype for this node *)
     path : lvar list,
     vars: (var * int) list,  (* variables bound to this point *)
     tree: matchTree}  (* arg of the dcon, Leaf if nullary *)
  | Tuple of   (* tuple pattern *)
    {live : int list,           (* live rules *)
     path : lvar list,
     vars : (var * int) list,   (* variables bound at this point *)
     children: matchTree list}  (* tuple components as children -- AND node *)
  | Var of
    {live: int list,           (* rules live at this point *)
     path : lvar list,
     vars: (var * int) list}   (* redundant? live = map #2 vars? *)
  | Open   (* initial empty matchTree into which patterns are merged *)
  | Leaf of
    {live: int list,           (* leaf, with live rules *)
     path : lvar list}

(* relev: matchTree -> int list
 * ASSERT: not(null(relev mt))  *)
fun relev (Data{relev,...}) = relev
  | relev (IntCase{relev,...}) = relev
  | relev (StrCase{relev,...}) = relev
  | relev _ = bug "relev"

(* live: matchTree -> int list
 * ASSERT: not(null(live mt))  *)
fun live (Data{live,...}) = live
  | live (IntCase{live,...}) = live
  | live (StrCase{live,...}) = live
  | live (Tuple{live,...}) = live
  | live (Var{live,...}) = live
  | live (Leaf live) = live
  | live (Single{tree,...}) = live mtree
  | live Open = bug "relev"

(* path : matchTree -> lvar list *)
fun path (Data{path,...}) = path
  | path (IntCase{path,...}) = path
  | path (StrCase{path,...}) = path
  | path (Tuple{path,...}) = path
  | path (Var{path,...}) = path
  | path (Leaf path) = path
  | path (Single{tree,...}) = path mtree
  | path Open = bug "relev"

(* precision : matchTree -> int *)
(* number of rules that appear in more than one branch of a choice node *)
fun precision (mtree) = 0   (* dummy implementation *)

fun mergePat (pat, ruleNo, matchTree) : matchTree =
    case pat
      of AppPat(dcon, bindpat) =  (* assume dcon not singular *)
	 (case matchTree
	   of Open  =>
	      if DC.singleton dcon then Single(dcon, mergePat(bindPat, Open)) else
	      let val dcons = DC.allDcons(dcon)
		  val path = [LVar.new()]
	       in Data{live = [ruleNo], relev = [ruleNo], path = path, vars = nil,
		       variants = [(dcon, mergePat(bindpat, ruleNo, Open})],
		       remainder = delete(dcon,dcons)}
	      end
	    | Data{live,relev,path,vars,variants,remainder} =>
	      let val live' = insert(ruleNo,live)
		  val relev' = insert(ruleNo,relev)
		  val variants' = mergeVariant(dcon, SOME bindpat, ruleNo, variants)
		  val remainder' = delete(dcon,remainder)}
	       in Data{live=live', relev=relev', vars=vars, variants=variants',
		       remainder=remainder'}
	      end
	    | Single{dcon, vars, tree} =>  (* a unique dcon *)
	      Single{dcon=dcon, vars=vars, tree=mergePat(bindPat, ruleNo, tree)}
	    | Var {live,vars} =>
	      let val dcons = DC.allDcons(dcon)
	       in Data{live = insert(ruleNo,live), relev = [ruleNo],
		       vars = vars,
		       variants = [(dcon, mergePat(bindpat, ruleNo, Open})],
		       remainder = delete(dcon,dcons)}
	    | _ => bug "matchTree[AppPat]")
       | BindPat(TuplePat pats) =>
	 (case matchTree
	   of Open =>
	      Tuple{live=[ruleNo],vars = nil,
		    children = map (fn p => mergePat(p,ruleNo,Open)) pats}
	    | Var{live, vars} =>
	      Tuple{live=insert(ruleNo,live), vars = (var,rule)::vars,
		    children = map (fn p => mergePat(p,ruleNo,Open)) pats}
	    | Tuple{live, vars, children} =>
	      Tuple{live=insert(ruleNo,live), vars = vars, 
		    children = ListPair.map (fn (p,t) => mergePat(p, ruleNo, t))
					    (pats, children)}
	    | _ => bug "matchTree[TuplePat]")
       | BindPat(AtomPat(VarPat v)) =>
	 (case matchTree
	   of Open => Var {live = [ruleNo], var = [(v,ruleNo)]}
	    | Var {live,vars} => Var{live=insert(ruleNo, live), vars = (v,ruleNo)::vars}
	    | Data{live, relev, vars, variants, remainder} =>
	      Data{live=insert(ruleNo, live), relev=relev, 
		   vars = (v, ruleNo)::vars, variants = variants,
		   remainder = remainder}}
	    | Single{live, vars, tree} =>
	      Single{live=insert(ruleNo, live), vars=(v,ruleNo)::vars, tree=tree}
	    | Tuple{live, vars, children} =>
	      Tuple{live=insert(ruleNo, live), vars=(v,ruleNo)::vars,
		    children=children}
	    | IntCase{live, consts, vars} =>
	      IntCase{live=insert(ruleNo,live), consts=consts, vars=(v,ruleNo)::vars})
       | BindPat(AtomPat(ConPat dcon)) =>
	 (case matchTree
	   of Open  =>
	      if DC.singleton dcon then Single{dcon=dcon,vars=nil,arg=Leaf[ruleNo]} else
	      let val dcons = DC.allDcons(dcon)
	       in Data{live = [ruleNo], relev = [ruleNo], vars = nil,
		       variants = [(dcon,Leaf[ruleNo])]
		       remainder = delete(dcon,dcons)}
	      end
	    | Var {live,vars} =>
	      if DC.singleton dcon then Single{dcon=dcon,vars=vars,arg=Leaf[ruleNo]} else
	      let val dcons = DC.allDcons(dcon)
	       in Data{live = insert(ruleNo,live) relev = [ruleNo],
		       vars = vars,
		       variants = [(dcon, Leaf [ruleNo])],
		       remainder = delete(dcon,dcons)}
	      end
	    | Data{live,relev,vars,variants,remainder} =>
	      let val live' = insert(ruleNo,live)
		  val relev' = insert(ruleNo,relev)
		  val variants' = mergeVariant(dcon, NONE, ruleNo, variants)
		  val remainder' = delete(dcon,remainder)
	       in Data{live=live', relev=relev', vars=vars, variants=variants',
		       remainder=remainder'}
	      end
	    | Single{dcon, vars, tree=Leaf live} => 
	      Single{dcon=dcon, vars=vars, tree=Leaf(insert(ruleNo,live)}
	    | _ => bug "matchTree[AppPat]")
       | BindPat(AtomPat(IntPat n)) =>
	 (case matchTree
	   of Open => IntCase{live=[ruleNo], consts=[(n, ruleNo)], vars=nil}
	    | Var{live,vars} => IntCase{live=insert(ruleNo,live),
					 consts=[(n, ruleNo)], vars=vars}
	    | IntCase{live, consts, vars} =>
	      IntCase{live=insert(ruleNo,live), consts=(n,ruleNo)::consts, vars=vars}
	    | _ => bug "mergePat[IntPat]")
       | BindPat(AtomPat(StrPat s)) =>
	 (case matchTree
	   of Open => StrCase{live=[ruleNo], consts=[(s, ruleNo)], vars=nil}
	    | Var{live,vars} => StrCase{live=insert(ruleNo,live),
					 consts=[(s, ruleNo)], vars=vars}
	    | StrCase{live, consts, vars} =>
	      IntCase{live=insert(ruleNo,live), consts=(s,ruleNo)::consts, vars=vars}
	    | _ => bug "mergePat[StrPat]")

and mergeVariant(dcon, argOp, ruleNo, variants) =
    let fun insert (nil) = (* adding a new dcon variant *)
	    [(dcon,
	      case argOp
	        of NONE => Leaf [ruleNo]
	         | SOME pat => mergePat(pat, ruleNo, Open))]
	  | insert ((dcon', tree):: rest) =
	    if DC.same (dcon,dcon') then  (* merging under an existing variant *)
		(dcon, case (argOp,tree)
			of (NONE, Leaf live) => Leaf(insert(ruleNo,live))
			 | (SOME pat,_) => mergePat(pat, ruleNo, tree)):: rest
	    else (dcon',tree)::insert rest
     in insert variants
    end


(*** build a decision tree ***)
(*
(1) Start at top of match tree, find a choice point relevant to pattern 1. If none found
then pattern 1 is irrefutable, and we are done (any further rules are redundant) -- commit
to rule 1.
(2) Case on head constructor of choice point, with default if variants are not complete
for the datatype.  
(3) For case consistent with pattern 1, search arg matchTree for another choice-point
relevant to rule 1. Generate next case, and proceed until there are no more choice-points
relevant to rule 1. Commit to rule 1.

(4) find highest choice point for pattern 2.
   (a) if same as first choice point for pattern 1, either go down same branch or different
       branch (which ever is appropriate).  If same branch, search for next pattern 2
       choice point within the arg matchTree.  If different branch, search for further
       pattern 2 relevant choice points below that branch, proceeding as for pattern 1.

*)

datatype path 
  = Dcase of dcon * path
  | Tpaths of (path * int) list
  | ICase of int
  | SCase of string
  | Irrelevant

fun live (Data{live,...}) = live
  | live (Single{vars, tree,...}) = map #2 vars @ live tree
  | live (Tuple{live,...}) = live
  | live (IntCase{live,...}) = live
  | live (StrCase{live,...}) = live
  | live (Var vars) = map #2 vars
  | live (Leaf l) = l

fun paths (n, tree) =
    case tree
      of Data{live,relev,variants,vars,remainder} => 
         if member(n, relev)
	 then (case find (fn (v as (dcon,tree)) => member(n,live(tree))) variants
		of SOME (dcon,tree) => DCase(dcon, paths(n, tree))
		 | NONE => bug "paths")
	 else Irrelevant
       | Tuple{live,vars,children} => (* assert member(n,live) *)
	 let fun getPath (tree, (i,paths)) =
		 case paths(n,tree)
		  of Irrelevant => (i+1, paths)
		   | p => (i+1, (p,i)::paths)
	  in case foldr getPath (0, nil) children
	      of nil =>  Irrelevant
	       | ps => Tpaths(ps)
	 end
       | Single{dcon, vars, tree} = 
	 (case paths tree
	    of Irrelevant => Irrelevant
             | p => SG(dcon,p))
       | IntCase{relev,consts,...} =>
	 if member(n,relev)
	 then (case find (fn (c,n') => n=n') consts
		of SOME (c,n') => SOME(ICase(c))
		 | NONE => bug "paths")
	 else Irrelevant
       | StrCase{relev,consts,...} =>
	 if member(n,relev)
	 then (case find (fn (c,n') => n=n') consts
		of SOME (c,n') => SOME(SCase(c))
		 | NONE => bug "paths")
	 else Irrelevant
       | _ => Irrelevant

fun mergePaths (path, n, dtree) = 
    case (path, dtree)
      of (Dcase (dcon, p), DDcase(cases, default)) =>
	 merge (dcon,p) cases
           -- dcon new to cases -- add a branch, elaborate p into a skeletal dtree
           -- dcon in cases - merge below the common dcon


(*
1. develop a set (or priority queue) of choice-points, starting with the
   top matchTree node.
   - if node is a Data (a choice-point), add case split immediately with
     this choice-point
   - if a tuple, scan elements adding choice-points to the set, then pick
     "best" choice-point (Data node) according to sort order.

2. at each alternative under the choice, inherit unused choice-points from
   above, and carry on the analysis on the constructor argument
   
3. at each point, generate a fresh variable to be bound to the matched
   value component for further reference at the leaves where pattern
   variables are to be bound. At a tuple node, introduce fresh variables
   for each component. Some of these may be bound to pattern variables
   at terminal decision nodes, some may be used as subject for case discr.
   (with "as" layered patterns, you could have both).

4. the sorting order on matchTrees is as follows:
   a. minimum element of relevance set
   b. minimum overlap of live sets on branches (fewest rules in multiple livesets)
  [c. minimum "width" of branch (i.e. no. of alternatives) - push wide branches down]
  [d. maximum number in relevance set?]

5. a decision branch "terminates" when remaining choice points are not relevant
   to lead pattern (relevant choice-points are exhausted).

6. At a terminal point, bind pattern variables of selected rule.
*)

type choiceSet = list matchTree

datatype dTree (* decision tree *)
 = Case of   (* or-node *)
   {name: lvar,   (* name of tested value *)
    cases: (dcon * (lvar option * dTree ref))) list, (* lvar names dcon arg *)
    default : dtree option}
 | Dtuple of   (* tuple destruction *)
   {name: lvar,   (* name of destructed value *)
    childNames: lvar list,   (* bind children to these vars *)
    body: dTree ref}
 | SingCon of
   {dcon : dcon,
    dtree : dTree ref}
 | Terminal of  (* unique rule chosen *)
   {rule: int,
    bindings: var * lvar list}
 | Indirect
 | Pending of lvar * matchTree   

val varbindings = Array.array(nRules, nil)

(* local lvars and variable assignments could be made during
 * pattern merge phase. *)

fun recordVars (lv, var_rules) = 
    app (fn (v,r) => update(varbindings, r, (v, lv))) var_rules

fun getChoices ((lv,k as Data{vars,...}) :: rest) = 
    (recordVars (lv, vars);
     (n,ref(Pending k)) :: getChoices rest)
  | getChoices ((n,k as IntCase _) :: rest) =
    (recordVars (lv, vars);
     (n,ref(Pending k)) :: getChoices rest)
  | getChoices ((n,k as StrCase _) :: rest) =
    (recordVars (lv, vars);
     (n,ref(Pending k)) :: getChoices rest)
  | getChoices ((n,k as IntCase _) :: rest) =
    (recordVars (lv, vars);
     (n,ref(Pending d)) :: getChoices rest)
  | getChoices (mt :: rest) =
    (recordVars (lv, mTreevars mt); getChoices rest)
  | getChoices nil = nil

(* choiceset: set of available choice points.
 * dtree contents of refs are Pending *)
val choiceset = list dTree ref

(* getChoice : unit -> dTree ref *)
fun getChoice () =
    (case !choiceset
      of nil => NONE
       | c :: cs => (choiceset := cs; SOME c))

(* ordering of choice points *)
fun better (mtree1, mtree2) =
    let val rel1 as (rule1::_) = relev mtree1    (* not(null(rel1)) *)
	val rel2 as (rule2::_) = relev mtree2    (* not(null(rel1)) *)
     in case Int.compare(rule1,rule2)
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

(* insertChoice : dTree ref -> unit *)
(* add a new choice-point to choiceset *)
fun insertChoice dtreeRef =
    (case !dtreeRef
      of Pending(lvar, mtree) =>
	 let val choices = !choiceset
	     fun ins (cps as (old as ref(Pending mtree'))::rest) = 
		 if better(mtree, mtree')
		 then dtreeRef :: cps
		 else old :: ins rest
	       | ins nil = [dtreeRef]
	  in choiceset := ins choices
	 end
       | _ => bug "insert")

(* mkDtree : matchTree -> dTree *)
fun mkDtree (mTree: matchTree) =
    case mTree 
     of Data _ => 
	let val choice = ref (Pending(freshLvar(), mTree))
	 in insertChoice choice;
	    dispatch rules;
	    choice
	end
      | Tuple {children, vars, ...} =>
	let val name = freshLvar ()
	    val childNames = map freshLvar children
	    val namedChildren = ListPair.zip(childNames,children)
	    fun checkChild (lv, mt) =
		(case mt
		  of Data _ | IntCase _ | StrCase _ =>
	             insertChoice(ref(Pending(lv,mt)))
                   | _ => ())
	    val _ = app checkChild namedChildren
	    val dtree = dispatch rules
	 in Dtuple{name = name, childNames = childNames,
		   body = dTree}
	end

(* dispatch : int list -> dTree *)
fun dispatch rules  : dTree  =
    case selectChoice (rules, choiceset)
     of SOME((lvar, mtreeRef), rest) =>
	(* !mtreeRef is a Data, IntCase, or StrCase *)
        (case !mtreeRef
	  of Data{vars, variants, remainder,...} =>
	     (* collect all new choicepoints for variants *)
	     let fun mkbranch(dcon, mtree) =
		     (case mtree
		       of Data cp => 
			  let val name = freshLvar()
			      val choice = ref(Pending(name, mtree))
			   in insertChoice choice;
			      (dcon, (SOME name, choice))
			  end
			| Tuple {children,...} =>
			  app (fn cp => insert(cp,choiceset)) cps


	let val (cases, default) = splitCases(!mtreeRef)
	 in Case{lvar=lvar, cases = cases, default = default}
	end
      | NONE => (* no choices relevant to top rule => Terminal *)
	let val rule = #1 rules
	 in (Terminal{rule = rule, 
		      bindings = sub(varbindings, rule)},
	     choiceset)   (* pass back unused choicepoints *) 
	end

(* WRONG! still.  choice-points (D,I,S mtree nodes) can be used multiple
times, but only once on a particular dtree branch.

On a branch, choice-points must obey their ordering as derived from their
dominance relations on the mtree. I.e., if cp1 < cp2 (appears higher in mtree),
then cp1 < cp2 on any branch of the dtree (cp2 applies to structure only accessible
after cp1 has been dispatched).
*)

end (* structure MatchComp *)
