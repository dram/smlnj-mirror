(* matchtree.sml *)

structure MatchTree = 
struct

(*** match trees ***)
(* This datastructure summarizes the information in an ordered set of patterns. 
 * It allows us to determine the test points, and which rules each test point is
 * relevant to.  A test point is a either a point where the focus is on a datatype
 * value and a case dispatch can be made on the dataconstructors (DataCP), or a
 * similar dispatch on constant patterns (IntCP, StrCP).

 * Nodes in a matchTree specify which rules are "live" at that point
 * (consistent with choices along the path to that node). Choice point
 * nodes (DataCP,IntCP,StrCP) also list the rules for which the choice point
 * is relevant, i.e. rules that can be refuted by the choice.
 * relev subset live?  relev disjoint from defaults?

 * The lvar attribute for a node is a fresh lvar that names the value component
 * examined by that node.  It can also serve as a unique identifier for choice
 * points, which is used to ensure that a choice point is only used once on any
 * branch in the decision tree.
 *)

structure DC = DataConUtil
structure LV = LVar
open Type Absyn

fun bug msg = raise Fail msg

(* same as andor tree, except for Single (for singlton datacons), Initial, starting
place for merging patterns (no initAndor needed), and Leaf, which seems to be used
as a phantom argument for nullary dcons. (?) *)
datatype matchTree
  = DataCP of  (* datatype dcons choice point *)
    {live : int list,          (* rule patterns matchable at this point *)
     relev : int list,         (* rules for which this choice point is "relevant" *)
     lvar : LV.uid,
     vars : (var * int) list,  (* variables bound to this point, with rule no. *)
     variants: (dcon * matchTree) list, (* the branches -- an OR node *)
     defaults: int list,       (* rules matching here by default (variable pats) *)
     remainder: dcon list} (* datacons not covered by variants --
			    * used to determine whether there is a default branch *)
  | IntCP of  (* integer liternal choice point *)
    {live : int list,           (* rules live at this point *)
     relev: int list,           (* rules for which relevant; not nil *)
     lvar : LV.uid,
     vars : (var * int) list,   (* variables bound at this point *)
     variants: (int * int list) list,  (* int literals, with matching rules *)
     defaults: int list} (* rules matching here by default (variable pats) *)
  | StrCP of  (* string literal choice point *)
    {live : int list,           (* rules live at this point *)
     relev: int list,           (* rules for which relevant; not nil *)
     lvar : LV.uid,
     vars : (var * int) list,   (* variables bound at this point *)
     variants: (string * int list) list, (* string literals, with matching rules *)
     defaults: int list}        (* rules matching here by default (variable pats) *)
  | Single of  (* singular datacon app *)
    {dcon: dcon,  (* the singleton dcon of the datatype for this node *)
     lvar : LV.uid,
     vars: (var * int) list,  (* variables bound to this point *)
     tree: matchTree}  (* arg of the dcon, Leaf if nullary *)
  | Tuple of   (* tuple pattern *)
    {live : int list,           (* live rules *)
     lvar : LV.uid,
     vars : (var * int) list,   (* variables bound at this point *)
     children: matchTree list}  (* tuple components as children -- AND node *)
  | Var of
    {live: int list,           (* rules live at this point *)
     lvar : LV.uid,
     vars: (var * int) list}   (* Invariant: live = map #2 vars *)
  | Initial   (* initial empty matchTree into which patterns are merged *)
  | Leaf of
    {live: int list}           (* leaf, with live rules *)

(* getRelev: matchTree -> int list
 * ASSERT: not(null(getRelev mt))  *)
fun getRelev (DataCP{relev,...}) = relev
  | getRelev (IntCP{relev,...}) = relev
  | getRelev (StrCP{relev,...}) = relev
  | getRelev _ = bug "relev"

(* getLive: matchTree -> int list
 * ASSERT: not(null(live mt))  *)
fun getLive (DataCP{live,...}) = live
  | getLive (IntCP{live,...}) = live
  | getLive (StrCP{live,...}) = live
  | getLive (Tuple{live,...}) = live
  | getLive (Var{live,...}) = live
  | getLive (Leaf{live}) = live
  | getLive (Single{tree,...}) = getLive tree
  | getLive Initial = bug "getLive"

(* lvar : matchTree -> LV.uid *)
fun getLvar (DataCP{lvar,...}) = lvar
  | getLvar (IntCP{lvar,...}) = lvar
  | getLvar (StrCP{lvar,...}) = lvar
  | getLvar (Tuple{lvar,...}) = lvar
  | getLvar (Var{lvar,...}) = lvar
  | getLvar (Single{lvar,...}) = lvar
  | getLvar _ = bug "getLvar"

fun getLvars (DataCP{lvar,...}) = [lvar]
  | getLvars (IntCP{lvar,...}) = [lvar]
  | getLvars (StrCP{lvar,...}) = [lvar]
  | getLvars (Tuple{children,...}) = map getLvar children
       (* ! single level tuples *)
  | getLvars (Var{lvar,...}) = [lvar]
  | getLvars (Single{lvar,...}) = [lvar]
  | getLvars _ = bug "getLvars"

(* isChoice : matchTree -> bool 
 * is the matchTree arg a choice point? *)
fun isChoice (DataCP _ | IntCP _ | StrCP _) = true
  | isChoice _ = false

(* place the new rule number at the end of the rule list *)
fun insert(n, ns) = ns @ [n]

(* mkMatchTree : Absyn.rulepat list
		 -> matchTree * (LVar.uid * LVar.uid) list Array.array *)
fun mkMatchTree (pats : rulepat list) =
let val nRules = length pats

(* lvenvRef: accumulates a mapping (lvenv) from lvars of original pattern
 * variables to matchTree position lvars *)
val lvenvRef : LVenv.lvenv ref = ref LVenv.empty

fun mapLvar (v, lv) = 
    lvenvRef := LVenv.bind(varToLvar v, lv, !lvenvRef)

(* mergeRules : int list * int list -> int list
 * form union of ordered lists of rule numbers *)
fun mergeRules (nil: int list, xs) = xs
  | mergeRules (xs, nil) = xs
  | mergeRules (xs as (x::xs'), ys as (y::ys')) =
    (case Int.compare(x,y)
      of LESS => x :: mergeRules(xs',ys)
       | EQUAL => x :: mergeRules(xs',ys')
       | GREATER => y :: mergeRules(xs,ys'))


(* pushDefaults : int list * matchTree -> matchTree *)
(* Pushes down default rules introduced by variable patterns to all
 * the nodes in the matchTree to which the variable is attached by mergePat.
 * This is used as a post-processing phase after the initial matchTree is
 * constructued by calls of mergePat. *)
fun pushDefaults (defaults : int list, mtree: matchTree) : matchTree =
    case mtree
      of DataCP {live,relev,lvar,vars,variants,defaults,remainder} =>
	 let val defaults' = mergeRules(defaults, map #2 vars)
	     fun push (dcon,mtree') = (dcon, pushDefaults(defaults', mtree'))
	  in DataCP{live=mergeRules(live,defaults'),
		    variants=map push variants,
		    relev=relev, lvar=lvar, vars=vars,
		    defaults = defaults', remainder=remainder}
	 end
       | IntCP {live,relev,lvar,vars,variants,defaults} =>
	 let val defaults' = mergeRules(defaults, map #2 vars)
	     fun push (x,rules) = (x,mergeRules(rules, defaults'))
	  in IntCP{live = mergeRules(live,defaults'),
		   variants = map push variants,
		   defaults = defaults',
		   relev=relev,lvar=lvar,vars=vars}
	 end
       | StrCP {live,relev,lvar,vars,variants,defaults} =>
	 let val defaults' = mergeRules(defaults, map #2 vars)
	     fun push (x,rules) = (x,mergeRules(rules, defaults'))
	  in StrCP{live = mergeRules(live,defaults'),
		   variants = map push variants,
		   defaults = defaults',
		   relev=relev,lvar=lvar,vars=vars}
	 end
       | Single{dcon, lvar, vars, tree} =>
	 let val defaults' = mergeRules(defaults, map #2 vars)
	  in Single{dcon=dcon, lvar=lvar, vars=vars,
		    tree=pushDefaults(defaults',tree)}
	 end
       | Tuple{live,lvar,vars,children} =>
	 let val defaults' = mergeRules(defaults, map #2 vars)
	  in Tuple{live = mergeRules(live,defaults),
		   children = map (fn mt => pushDefaults(defaults',mt)) children,
		   lvar=lvar, vars=vars}
	 end
       | Var{live,lvar,vars} => 
	 Var{live = mergeRules(live,defaults), lvar=lvar,vars=vars}
       | Leaf{live} =>
	 Leaf{live = mergeRules(live,defaults)}
       | Initial => bug "pushDefaults"


fun deleteDcon (dcon, nil) = nil
  | deleteDcon (dcon1, dcon2::dcons) = 
    if DC.same(dcon1,dcon2) then dcons else dcon2::deleteDcon (dcon1,dcons)

(* mergePat: rulepat * int * matchTree -> matchTree
 * merge information for next pattern (with its ruleNo) into the 
 * matchTree produced by previous patterns *)
fun mergePat (pat: rulepat, ruleNo, matchTree) : matchTree =
    case pat
      of AppPat(dcon, bindpat) =>  (* assume dcon not singular *)
	 (case matchTree
	   of Initial  =>
	      if DC.singleton dcon
	      then Single{dcon=dcon, tree=mergePat(BindPat bindpat, ruleNo, Initial),
			  lvar=LVar.new(), vars=nil}
              else
	      let val dcons = DC.allDcons(dcon)
		  val lvar = LVar.new()
	       in DataCP{live = [ruleNo], relev = [ruleNo], lvar = lvar, vars = nil,
		       variants = [(dcon, mergePat(BindPat bindpat, ruleNo, Initial))],
		       defaults=nil, remainder = deleteDcon(dcon,dcons)}
	      end
	    | DataCP{live,relev,lvar,vars,variants,defaults,remainder} =>
	      let val live' = insert(ruleNo,live)
		  val relev' = insert(ruleNo,relev)
		  val variants' = mergeDcon(dcon, SOME bindpat, ruleNo, variants)
		  val remainder' = deleteDcon(dcon,remainder)
	       in DataCP{live=live', relev=relev', lvar=lvar, vars=vars,
		       variants=variants', defaults=defaults, remainder=remainder'}
	      end
	    | Single{dcon, lvar, vars, tree} =>  (* a unique dcon *)
	      Single{dcon=dcon, lvar=lvar, vars=vars,
		     tree=mergePat(BindPat bindpat, ruleNo, tree)}
	    | Var {live,lvar,vars} =>
	      let val dcons = DC.allDcons(dcon)
	       in DataCP{live = insert(ruleNo,live), relev = [ruleNo],
			 lvar = lvar, vars = vars,
			 variants = [(dcon, mergePat(BindPat bindpat, ruleNo, Initial))],
			 defaults = live,
			 remainder = deleteDcon(dcon,dcons)}
	      end
	    | _ => bug "matchTree[AppPat]")
       | BindPat(TuplePat pats) =>
	 (case matchTree
	   of Initial =>
	      Tuple{live=[ruleNo],lvar = LVar.new(), vars = nil,
		    children = map (fn p => mergePat(BindPat(AtomPat p),ruleNo,Initial))
				   pats}
	    | Var{live, lvar, vars} =>  (* live = map #2 vars *)
	      Tuple{live=insert(ruleNo,live), lvar = lvar, vars = vars,
		    children = map (fn p => mergePat(BindPat(AtomPat p),ruleNo,Initial))
				   pats}
	    | Tuple{live, lvar, vars, children} =>
	      Tuple{live=insert(ruleNo,live), lvar = lvar, vars = vars, 
		    children = ListPair.map
				 (fn (p,t) => mergePat(BindPat(AtomPat p), ruleNo, t))
				 (pats, children)}
	    | _ => bug "matchTree[TuplePat]")
       | BindPat(AtomPat(VarPat v)) =>
         (* ruleNo will be added to live and defaults fields later by pushDefaults *)
	 (case matchTree
	   of Initial =>
	      let val lv = LVar.new()
	       in mapLvar(v,lv);
		  Var {live = [ruleNo], lvar = lv, vars = [(v,ruleNo)]}
	      end
	    | Var {live,lvar,vars} => 
	      (mapLvar(v,lvar);
	       Var{live=insert(ruleNo, live), lvar = lvar, vars = insert((v,ruleNo), vars)})
	    | DataCP{live, relev, lvar, vars, variants, defaults, remainder} =>
	      (mapLvar(v,lvar);
	       DataCP{vars = insert((v, ruleNo), vars),
		      live=live, relev=relev, lvar=lvar, variants = variants,
		      defaults = defaults, remainder = remainder})
	    | IntCP{live, relev, lvar, vars, variants, defaults} =>
	      (mapLvar(v,lvar);
	       IntCP{vars = insert((v,ruleNo),vars),
		     live = live, relev=relev, lvar=lvar, defaults = defaults,
		     variants = variants})
	    | StrCP{live, relev, lvar, vars, defaults, variants} =>
	      (mapLvar(v,lvar);
	       StrCP{vars = insert((v,ruleNo),vars),
		     live = live, relev=relev, lvar=lvar, defaults = defaults,
		     variants = variants})
	    | Single{dcon, lvar, vars, tree} =>
	      (mapLvar(v,lvar);
	       Single{vars = insert((v,ruleNo),vars), dcon=dcon, lvar=lvar, tree = tree})
	    | Tuple{live, lvar, vars, children} =>
	      (mapLvar(v,lvar);
	       Tuple{vars = insert((v,ruleNo),vars),
		     live=live, lvar=lvar, children=children}))
       | BindPat(AtomPat(ConPat dcon)) => (* nullary dcon pat *)
	 (case matchTree
	   of Initial  =>
	      if DC.singleton dcon
	      then Single{dcon=dcon,lvar=LVar.new(),vars=nil,tree=Leaf{live=[ruleNo]}}
	      else let val dcons = DC.allDcons(dcon)
		   in DataCP{live = [ruleNo], relev = [ruleNo], lvar=LVar.new(),
			     vars = nil, variants = [(dcon,Leaf{live=[ruleNo]})],
			     defaults = nil, remainder = deleteDcon(dcon,dcons)}
		   end
	    | Var {live,lvar,vars} =>
	      if DC.singleton dcon
	      then Single{dcon=dcon,lvar=lvar,vars=vars,tree=Leaf{live=[ruleNo]}}
	      else let val dcons = DC.allDcons(dcon)
		   in DataCP{live = insert(ruleNo,live), relev = [ruleNo],
			     lvar = lvar, vars = vars,
			     variants = [(dcon, Leaf{live=[ruleNo]})],
			     defaults = live, remainder = deleteDcon(dcon,dcons)}
		   end
	    | DataCP{live,relev,lvar,vars,variants,defaults,remainder} =>
	      let val live' = insert(ruleNo,live)
		  val relev' = insert(ruleNo,relev)
		  val variants' = mergeDcon(dcon, NONE, ruleNo, variants)
		  val remainder' = deleteDcon(dcon,remainder)
	       in DataCP{live=live', relev=relev', lvar=lvar, vars=vars,
			 variants=variants', defaults=defaults, remainder=remainder'}
	      end
	    | Single{dcon, lvar, vars, tree=Leaf{live}} => 
	      Single{dcon=dcon, lvar=lvar, vars=vars,
		     tree=Leaf{live=insert(ruleNo,live)}}
	    | _ => bug "matchTree[AppPat]")
       (* need to deal with repeated occurrences of the same constant pattern *)
       | BindPat(AtomPat(IntPat n)) =>
	 (case matchTree
	   of Initial =>
	      IntCP{live=[ruleNo], relev = [ruleNo], lvar=LVar.new(),
		    vars=nil, defaults=nil, variants=[(n, [ruleNo])]}
	    | Var{live,lvar,vars} =>
	      IntCP{live=insert(ruleNo,live), relev = [ruleNo], lvar=lvar,
		      vars=vars, defaults=live, variants=[(n, [ruleNo])]}
	    | IntCP{live, relev, lvar, vars, defaults, variants} =>
	      IntCP{live=insert(ruleNo,live), relev=insert(ruleNo,relev),
		    lvar=lvar, vars=vars, defaults=defaults,
		    variants=mergeInt(n,ruleNo,variants)}
	    | _ => bug "mergePat[IntPat]")
       | BindPat(AtomPat(StrPat s)) =>
	 (case matchTree
	   of Initial => StrCP{live=[ruleNo], relev=[ruleNo], lvar=LVar.new(),
			       vars=nil, defaults=nil, variants=[(s, [ruleNo])]}
	    | Var{live,lvar,vars} =>
	      StrCP{live=insert(ruleNo,live), relev=[ruleNo], lvar=lvar, vars=vars,
		    defaults=live, variants=[(s, [ruleNo])]}
	    | StrCP{live, relev, lvar, vars, defaults, variants} =>
	      StrCP{live=insert(ruleNo,live), relev=insert(ruleNo,relev),
		    lvar=lvar, vars=vars, defaults=defaults,
		    variants=mergeStr(s,ruleNo,variants)}
	    | _ => bug "mergePat[StrPat]")

and mergeDcon(dcon, argOp, ruleNo, variants) =
    let fun ins nil = (* adding a new dcon variant *)
	    [(dcon,
	      case argOp
	        of NONE => Leaf{live=[ruleNo]}
	         | SOME pat => mergePat(BindPat pat, ruleNo, Initial))]
	  | ins ((c as (dcon', tree)):: rest) =
	    if DC.same (dcon,dcon') then  (* merging into an existing variant *)
		(dcon, case (argOp,tree)
			 of (NONE, Leaf{live}) => Leaf{live=insert(ruleNo,live)}
			  | (SOME pat,_) => mergePat(BindPat pat, ruleNo, tree)) :: rest
	    else c :: ins rest
     in ins variants
    end

and mergeInt(n: int, ruleNo, variants) =
    let fun ins nil = (* adding a new dcon variant *)
	    [(n, [ruleNo])]
	  | ins ((c as (m, rules))::rest) =
	    if n = m then  (* merging into an existing variant *)
		(m, insert(ruleNo, rules))::rest
	    else c :: ins rest
     in ins variants
    end

and mergeStr(s: string, ruleNo, variants) =
    let fun ins nil = (* adding a new dcon variant *)
	    [(s, [ruleNo])]
	  | ins ((c as (t, rules))::rest) =
	    if s = t then  (* merging into an existing variant *)
		(t, insert(ruleNo, rules))::rest
	    else c :: ins rest
     in ins variants
    end

val (mtree,_) = foldl (fn (pat,(tree,ruleNo)) => (mergePat(pat,ruleNo,tree),ruleNo+1))
		  (Initial,0) pats

 in (pushDefaults(nil,mtree), !lvenvRef)
end (* function mkMatchTree *)

end (* structure MatchComp *)

(* ealier version - replaced by pushDefaults *)
(* pushLive : int * matchTree -> matchTree  $$$$ not used! 
(* when a VarPat is merged into a matchTree, its rule number should become live
 * throughout that matchTree. pushLive accomplishes this. *)
fun pushLive (r, mtree) =
    let fun push mtree =
	case mtree
	  of DataCP {live,relev,lvar,vars,variants,remainder} =>
	     DataCP{live=insert(r,live),
		  variants= map (fn (dcon, mtree') => (dcon, push mtree')) variants,
		  relev=relev,lvar=lvar,vars=vars,remainder=remainder}
	   | IntCP {live,relev,lvar,vars,variants} =>
	     IntCP{live=insert(r,live),
		     variants= map (fn (n,rules) => (n, insert(r,rules))) variants,
		     relev=relev,lvar=lvar,vars=vars}
	   | StrCP {live,relev,lvar,vars,variants} =>
	     StrCP{live=insert(r,live),
		     variants= map (fn (s,rules) => (s, insert(r,rules))) variants,
		     relev=relev,lvar=lvar,vars=vars}
	   | Single{dcon, lvar, vars, tree} =>
	     Single{dcon=dcon, lvar=lvar, vars=vars, tree=push tree}
	   | Tuple{live,lvar,vars,children} =>
	     Tuple{live=insert(r,live),
		   chidren=map push children,
		   lvar=lvar, vars=vars} 
	   | Var{live,lvar,vars} => 
	     Var{live=insert(r,live), lvar=lvar,vars=vars}
	   | Leaf{live} =>
	     Leaf{live=insert(r,live)}
	   | Initial => bug "pushLive"
     in push mtree
    end
*)
