(* matchcomp.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature MATCH_COMP =
sig

  type toTcLt = (Types.ty -> PLambdaType.tyc) * (Types.ty -> PLambdaType.lty)

  type genintinfswitch =
       PLambda.lexp * (IntInf.int * PLambda.lexp) list * PLambda.lexp
       -> PLambda.lexp

  val bindCompile :
        StaticEnv.staticEnv * (Absyn.pat * PLambda.lexp) list
          * (PLambda.lexp -> PLambda.lexp) * LambdaVar.lvar * toTcLt
          * ErrorMsg.complainer
	  * genintinfswitch
	-> PLambda.lexp

  val matchCompile :
        StaticEnv.staticEnv * (Absyn.pat * PLambda.lexp) list
          * (PLambda.lexp -> PLambda.lexp) * LambdaVar.lvar * toTcLt
          * ErrorMsg.complainer
	  * genintinfswitch
	-> PLambda.lexp

  val handCompile :
        StaticEnv.staticEnv * (Absyn.pat * PLambda.lexp) list
          * (PLambda.lexp -> PLambda.lexp) * LambdaVar.lvar * toTcLt
          * ErrorMsg.complainer * genintinfswitch
	-> PLambda.lexp

end (* signature MATCH_COMP *)


structure MatchComp : MATCH_COMP =
struct

local structure DA = Access
      structure BT = BasicTypes
      structure LT = PLambdaType
      structure TU = TypesUtil
      structure PO = Primop
      structure MP = PPLexp
      structure EM = ErrorMsg
      structure TP = Types
      structure LN = LiteralToNum
      structure PP = PrettyPrintNew

      open VarCon Types
      open Absyn PLambda
      open PrettyPrintNew
      open MCCommon

in

(* utility functions for managing rule lists (type rules) *)
val intersect=SortedList.intersect
val union = SortedList.merge
val setDifference = SortedList.difference
fun member(i,set) = SortedList.member set i

val debugging = Control.MC.debugging
fun bug s = EM.impossible ("MatchComp: " ^ s)
val say = Control.Print.say
val pd = Control.Print.printDepth
fun ppLexp le =
    PP.with_default_pp(fn ppstrm => MP.ppLexp (!pd) ppstrm le)
fun ppDectree dt =
    PP.with_default_pp(fn ppstrm => PPMatchComp.ppDectree (!pd) ppstrm dt)

type toLty = ty -> LT.lty
type toTcLt = (ty -> LT.tyc) * (ty -> LT.lty)

type genintinfswitch =
     PLambda.lexp * (IntInf.int * PLambda.lexp) list * PLambda.lexp
     -> PLambda.lexp

(* type of matchRep defined in matchComp, previously also referred to as "ruleDesc".pre
 * Invariant 1: The 1st component of the triples are all of the form [(ROOTPATH, pat)].
 * Invariant 2: The 3rd component of all triples is the same lvar (fname in preProcessRule).
 * Comment: don't need the lvar repeated in each triple. Call this lvar the rule "fname". *)

(* type ruleRepTy = ((path * pat) list * path list * lvar) list *)
(* type matchRepsTy = (ruleRepTy * (lvar * PLambda.lexp)) list *)

type multiRule = {pats: (pat * path list) list, fname: lvar, rhsFun: Plambda.lexp}

type matchRep = multiRule list

(*
 * MAJOR CLEANUP REQUIRED ! The function mkv is currently directly taken
 * "raw" from the LambdaVar module; I think it should be taken from the
 * "compInfo". Similarly, should we replace all mkLvar in the backend
 * with the mkv in "compInfo" ? (ZHONG)
 * (DBM) Not clear than anything can go wrong using a single, global source
 * for new lvars -- no known bugs attributed to this practice.  Could 
 * add a lvar generator reset function that could be invoked between 
 * compilation units.
 *)
val mkv = LambdaVar.mkLvar

(** translating the typ field in DATACON into lty; types of constant datacons
    are modified to function types taking ltc_unit as the argument *)
fun toDconLty toLty ty =
    (case ty
       of TP.POLYty{sign, tyfun=TYFUN{arity, body}} =>
          if BT.isArrowType body then toLty ty  (* nonconstant polymorphic constructor *)
          else toLty (TP.POLYty{sign=sign,
				tyfun=TYFUN{arity=arity,
                                            body=BT.-->(BT.unitTy, body)}})
	| _ =>
	  if BT.isArrowType ty then toLty ty (* nonconstant monomorphic constructor *)
          else toLty (BT.-->(BT.unitTy, ty)))

fun numCon (v, ty, msg) =
    let fun mkWORD sz = WORDpcon{ival = v, ty = sz}

     in if TU.equalType(ty, BT.intTy)
	  then mkINT Target.defaultIntSz
	else if TU.equalType(ty, BT.int32Ty)
	  then mkINT 32
	else if TU.equalType(ty, BT.int64Ty)
	  then mkINT 64
	else if TU.equalType(ty, BT.intinfTy)
	  then mkINT 0
	else if TU.equalType(ty, BT.wordTy)
	  then mkWORD Target.defaultIntSz
	else if TU.equalType(ty, BT.word8Ty)
(* QUESTION: perhaps we should preserve the size (e.g., in the case of
 * word8) for better jump tables? *)
	  then mkWORD Target.defaultIntSz
	else if TU.equalType(ty, BT.word32Ty)
	  then mkWORD 32
        else if TU.equalType(ty, BT.word64Ty)
          then mkWORD 64
	  else bug msg
    end

(* default integer pattern constant *)
fun intCon n = INTpcon{ival = IntInf.fromInt n, ty = Target.defaultIntSz}

(* pattern constant for character literal *)
(* QUESTION: perhaps this should be a Word8.word literal? *)
fun charCon s = intCon (Char.ord (String.sub (s, 0)))

(**************************************************************************)

(* allConses = fn : 'a list * 'a list list -> 'a list list *)
(* allConses(hds, tls) returns all conses of an element of hds to an element of tls,
 * such that length(allConses(hds,tls)) = length hds * length tls *)
fun allConses (hds, tls) =
    List.concat (map (fn hd => (map (fn tl => hd::tl) tls)) hds)

(* orExpand : Absyn.pat -> Absyn.pat list *)
(* Invariant: length(orExpand pat) >= 1 *)
fun orExpand (ORpat(pat1,pat2)) =
      (orExpand pat1)@(orExpand pat2)
  | orExpand (pat as RECORDpat{fields,...}) =
     map (mkRECORDpat pat) (foldr allConses [nil] (map (orExpand o #2) fields))
  | orExpand (VECTORpat(pats,t)) =
      map (fn p => VECTORpat(p,t)) (foldr allConses [nil] (map orExpand pats))
  | orExpand (APPpat(k,t,pat)) =
      map (fn pat => APPpat(k,t,pat)) (orExpand pat)
  | orExpand (CONSTRAINTpat(pat,_)) =
      orExpand pat
  | orExpand (LAYEREDpat(lpat, bpat) | LAYEREDpat(CONSTRAINTpat(lpat, _), bpat)) =
      (* lpat assumed to not contain ORpat *)
      map (fn pat => LAYEREDpat(lpat,pat)) (orExpand bpat)
  | orExpand pat =
      [pat]

(* lookupVar : (VarCon.var * 'value) list -> VarCon.var -> 'value
   an association list lookup function where the keys of the association lists
   are (pattern-bound) variables. Identification of keys is based on the path
   field of the var, which should always be a singleton symbolic path consisting
   of the name of the bound variable.
   Called once in preProcessRule.
 *)
fun lookupVar ((VALvar{path=p2,...}, value)::rest) (v as VALvar{path=p1,...}) =
       if SymPath.equal(p1,p2) then value else lookupVar rest v
  | lookupVar [] _ = bug "lookupVar unbound"
  | lookupVar _ _ = bug "lookupVar unexpected arg"

(* boundVariables : Absyn.pat -> VarCon.var list *)
(* traverse the pattern, returning a list of the bound variables it contains
 * Assumption: MARKpats have been removed from argument pat *)
fun boundVariables (VARpat v) = [v]
  | boundVariables (CONSTRAINTpat(pat,_)) = boundVariables pat
  | boundVariables (LAYEREDpat(pat1, pat2)) =
      boundVariables pat1 @ boundVariables pat2
  | boundVariables (APPpat(_,_,pat)) = boundVariables pat
  | boundVariables (RECORDpat{fields,...}) =
      List.concat (map (boundVariables o #2) fields)
  | boundVariables (VECTORpat(pats,_)) = List.concat (map boundVariables pats)
  | boundVariables (ORpat (pat1,_)) = boundVariables pat1 (* shared bound variables in OR pats *)
  | boundVariables (MARKpat _) = bug "boundVariables - MARKpat"
  | boundVariables _ = nil

(* patternBindings : Absyn.pat -> (VarCon.var * path) list *)
(* produces an association list mapping from pattern bound vars to corresponding
 * destructuring paths relative to a value matching the pattern. 
 * Applied to OR split patterns of a rule after OR expansion in preProcessRule *)
fun patternBindings pat =
    let fun bindings (VARpat v, path) = [(v, path)]
	  | bindings (CONSTRAINTpat(pat,_), path) = bindings(pat, path)
	  | bindings (LAYEREDpat(pat1, pat2), path) =
	      bindings(pat1, path) @ bindings(pat2, path)
	  | bindings (APPpat(k,t,pat), path) =
	    bindings(pat, DELTAPATH(DATApcon(k, t), path))
	  | bindings (RECORDpat{fields,...}, path) =
	      let fun doGen(n, nil) = nil
		    | doGen(n, (lab,pat)::rest) =
		      (bindings(pat,PIPATH(n,path))) @ (doGen(n+1,rest))
	      in doGen(0, fields)
	      end
	  | bindings (VECTORpat(pats,t), path) =
	      let fun doGen(n, nil) = nil
		    | doGen(n, pat::rest) =
		      (bindings(pat,VPIPATH(n,t,path))) @ (doGen(n+1,rest))
	      in doGen(0, pats)
	      end
	  | bindings (ORpat _, _) = bug "patternBindings - unexpected OR pattern"
	  | bindings (MARKpat _, path) = bug "patternBindings - MARKpat"
	  | bindings _ = nil
    in bindings(pat, ROOTPATH)
    end

(* varToLvarLty : VarCon.var -> lvar * lty  *)
fun varToLvarLty (VALvar{access=DA.LVAR v, typ,...}, toLty) = (v, toLty (!typ))
  | varToLvarLty _ = bug "varToLvarLty - unexpected variable access"

(* preProcessRule : toLtyTy                      -- type translators
 *                  -> Absyn.pat * Plambda.lexp  -- a rule
 *                  -> multiRule
 * translate a rule into a multiRule *)
fun preProcessRule toLty (pat: Absyn.pat, rhs: lexp) =
  let val pat = AbsynUtil.stripPatMarks pat  (* strip location marks from pattern *)
      val boundVars: VarCon.var list = boundVariables pat
          (* variables bound in the original pattern *)

      (* genRHSFun : VarCon.var list * lexp -> lexp *)
      fun genRHSFun ([], rhs) =  (* dummy abstraction when no lhs bound variables *)
	    FN(mkv(), LT.ltc_unit, rhs)
        | genRHSFun ([v], rhs) =
            let val (argVar,argt) = varToLvarLty(v, toLty)
             in FN(argVar,argt,rhs)
            end
        | genRHSFun (boundvars, rhs) =
	    let val argLvar = mkv()  (* : lvar *)
		val argVar = VAR argLvar  (* : lexp *)
		fun letWrapRHS (nil, n) = (rhs,nil)
		  | letWrapRHS (var1::rest, n) =
		    let val (lvar1,lty1) = varToLvarLty(var1, toLty)
			val (lexp,ltys) = letWrapRHS(rest,n+1)
		    in (LET(lvar1, SELECT(n, argVar), lexp), lty1 :: ltys)
		    end
		val (body,tt) = letWrapRHS(boundvars,0)
	     in FN(argLvar, LT.ltc_tuple tt, body)
	    end

      (* addBVpaths: pat list -> (pat * path list) list *)
      fun addBVpaths nil = nil
        | addBVpaths (pat::rest) =
            let val pathenv = patternBindings pat
                val boundVarPaths =  (* destructuring paths for bound vars *)
		    map (lookupVar pathenv) boundVars
             in (pat, boundVarPaths) :: addBVpaths rest
            end

      val rhsFun = genRHSFun (boundVars, rhs)
          (* original rhs abstracted over matched variables tuple: Plambda.lexp *)
      val orFamily = orExpand pat  (* expand out ORs into separate patterns *)
      val patsWithPaths = addBVpaths orFamily
          (* an OR-family (list) of pats derived from pat *)
      val fname = mkv()
          (* make a fresh lvar "associated with" (?) the result multirule *)
   in {pats = patsWithPaths, fname = fname, rhsFun = rhsFun}
  end

(* makeAndor : pat list -> andor, -- access from AndOr structure *)

(* fireConstraint : path
 *                  * (path * decision list) list   -- from flattenAndors
 *                  * decision list                 -- ready list
 *                  * (path * decision list) list   -- delayed list
 *                  -> decision list * (path * decision list) list *)
fun fireConstraint (path, (needPath, decisions)::rest, ready, delayed) =
      (if pathEq(path, needPath)
         then fireConstraint(path, rest, decisions@ready, delayed)
       else fireConstraint(path, rest, ready, (needPath,decisions)::delayed))
  | fireConstraint (path, nil, ready, delayed) =
      (ready, delayed)

fun relevant (CASEDEC{defaults,...}, rulenum) =
      not (member(rulenum, defaults))
  | relevant (BINDDEC _, _) =
      bug "relevant - unexpected BINDDEC arg"

fun metric (CASEDEC{cases, defaults, ...}) = (length defaults, length cases)
  | metric (BINDDEC _) = bug "metric - unexpected BINDDEC arg"

fun metricBetter((a:int,b:int),(c,d)) = a < c orelse (a = c andalso b < d)

(* extractNth : int * 'a list -> 'a * 'a list *)
fun extractNth(0, a::b) = (a, b)
  | extractNth(n, a::b) =
      let val (c,d) = extractNth(n - 1, b) in (c, a::d) end
  | extractNth _ = bug "extractNth - n too big"

exception PickBest
(* pickBest : decision list * ruleset -> decision * decision list *)
fun pickBest (decisions, active) = 
    let fun pick(nil, _, _, _, NONE) = raise PickBest
	  | pick(nil, _, _, _, SOME n) = n
	  | pick((BINDDEC _)::rest, _, n, _, _) = n
	  | pick((CASEDEC{sign = DA.CSIG(1,0), ...})::rest, _, n, _, _) = n
	  | pick((CASEDEC{sign = DA.CSIG(0,1), ...})::rest, _, n, _, _) = n
	  | pick(aCase::rest, active as act1::_, n, NONE, NONE) =
	    if relevant (aCase, act1)
	    then pick(rest, active, n + 1, SOME(metric aCase), SOME n)
	    else pick(rest, active, n + 1, NONE, NONE)
	  | pick(aCase::rest, active as act1::_, n, SOME m, SOME i) =
	    if relevant (aCase, act1)
	    then let val myMetric = metric aCase
		 in if metricBetter(myMetric, m) then
			pick(rest, active, n + 1, SOME(myMetric), SOME n)
		    else pick(rest, active, n + 1, SOME m, SOME i)
		 end
	    else pick(rest, active, n + 1, SOME m, SOME i)
	  | pick _ = bug "pick - unexpected arg"
	val best = pick(decisions, active, 0, NONE, NONE)
    in  extractNth(best, decisions)
    end

(* genDecisionTree : (decision list * (path * decision list) list) * ruleset
                     -> dectree *)
fun genDecisionTree((decisions, delayed), active as active1::_) =
      ((case pickBest(decisions, active)
         of (BINDDEC{path, ...}, rest) =>
	     genDecisionTree(fireConstraint(path,delayed,rest,nil),active)
          | (CASEDEC{path, sign, cases, defaults}, rest) =>
             let fun isActive(_,rules,_) = intersect(rules, active) <> []
                 val activeCases = List.filter isActive cases
                 val caseTrees =
                     gencases(activeCases, rest, delayed, defaults, active)
                 val defActive = intersect(active, defaults)
		 val signLength =
		     case sign
		      of DA.CSIG(i,j) => i+j
		       | DA.CNIL => 0
		 val decTreeOp =
                     if length activeCases = signLength then NONE
                     else SOME (genDecisionTree((rest, delayed), defActive))
              in CASETEST{path=path, sign=sign, caseTrees=caseTrees, default=decTreeOp}
             end)
       handle PickBest => (RHS active1))
  | genDecisionTree (_,nil) = bug "genDecisionTree - null active"

(* gencases : decision_case list * decision list * (path * decision list) list
              * ruleset * ruleset
              -> (pcon * dectree) list *)
and gencases (nil, decs, delayed, defaults, active) = nil
  | gencases ((pcon,rules,guarded)::rest,decs,delayed,defaults,active)=
      let val rActive = intersect(union(defaults, rules), active)
       in (pcon, genDecisionTree((decs@guarded, delayed), rActive))
          :: (gencases(rest,decs,delayed,defaults,active))
      end


(* rulesUsed : dectree -> rules
 *  returns all rules used in the dectree, maintaining ordering
 *  (because union operation does) *)
fun rulesUsed (RHS n) = [n]
  | rulesUsed (BIND{dectree,...}) = rulesUsed dectree
  | rulesUsed (CASETEST{caseTrees, default=NONE,...}) =
      foldr (fn((_,a), b) => union(rulesUsed a, b)) nil caseTrees
  | rulesUsed (CASETEST{caseTrees, default=SOME dt,...}) =
      foldr (fn((_,a), b) => union(rulesUsed a, b)) (rulesUsed dt) caseTrees

(* fixupUnused : rules * matchRepsTy -> rules *)
(* this code is buggy - the elements of mr aren't what it thinks they are *)
fun fixupUnused (unused: rules, mr: matchRepsTy) : rules=
    let fun fixup (nil, _, _, _, out) = out
	  | fixup (unused, (nil, _)::rest, n, m, out) =
	    fixup (unused, rest, n, m + 1, out)
	  | fixup (unused as ufst::urest, (rule::rules, x)::mrest, n, m, nil) =
	    if ufst = n then
		fixup(urest, (rules, x)::mrest, n + 1, m, [m])
	    else
		fixup(unused, (rules, x)::mrest, n + 1, m, nil)
	  | fixup (unused as ufst::urest, (rule::rules, z)::mrest, n, m, x::y) =
	    if ufst = n then
		(if m <> x then
		     fixup(urest, (rules, z)::mrest, n + 1, m, m::x::y)
		 else fixup(urest, (rules, z)::mrest, n + 1, m, x::y))
	    else fixup(unused, (rules, z)::mrest, n + 1, m, x::y)
	  | fixup _ = bug "fixup - unexpected arg"
    in rev(fixup(unused, mr, 0, 0, nil))
    end

(* redundant : rules * ruleno -> bool
 *  true if rules contains a member not equal to ruleno
 *  i.e. false only if rules = [ruleno]  ??? looks bogus *)
fun redundant (nil, n: int) = false
  | redundant (a::b, n) = a <> n orelse redundant (b, n)

fun complement(n, m, a::b) =
      if n < a then n::(complement(n + 1, m, a::b))
      else complement(n + 1, m, b)
  | complement(n, m, nil) =
      if n < m then n::(complement(n + 1, m, nil)) else nil

type pathList = path list
(* conjectured invariant: a pathList has no duplicate members *)
(* question: does the order of paths in a path list matter? Apparently not. *)

type pathSet = (int * pathList) list
(* terminology: the int component of a pathSet member is called its index.
 * invariant?: index represents common "depth" of paths in pathList, or
 *    in other words, all paths in the pathList have metric = index
 * invariant: indexes occur in (strictly?) ascending order
 * invariant?: no duplicate indexes
 *)

(* dividePathList: (path -> bool) * pathList * pathList * pathList
 *                  -> pathList * pathList
 *   divide path list into a pair of a list of paths satisfying pred and
 *   a list of paths not satisfying pred. *)
fun dividePathList (pred, paths) =
    let fun divide(pred, nil, accyes, accno) = (accyes, accno)
	  | divide(pred, path::rest, accyes, accno) =
	    if pred path then divide(pred, rest, path::accyes, accno)
	    else divide(pred, rest, accyes, path::accno)
    in  divide(pred, paths, nil, nil)
    end

(* addPathToPathList : path * pathList -> pathList
 *  add path to the end of pathList if is not already in pathList *)
fun addPathToPathList (path, paths as path1::rest) =
      if pathEq(path, path1) then paths
      else path1::(addPathToPathList(path, rest))
  | addPathToPathList (path, nil) = [path]

(* unitePathLists : pathList * pathList -> pathList
 *  merge two pathLists, suppressing duplicate paths; the new members of
 *  the first pathlist are added to the end of the 2nd pathList in reverse
 *  order *)
fun unitePathLists(paths1, nil) = paths1
  | unitePathLists(nil, paths2) = paths2
  | unitePathLists(path1::rest1, paths2) =
      addPathToPathList(path1, unitePathLists(rest1, paths2))

(* inPathList : path * pathList -> bool
 *  path is equal (pathEq) to a member of pathList *)
fun inPathList (path1, nil) = false
  | inPathList (path1, path2::rest) =
      pathEq(path1, path2) orelse inPathList(path1, rest)

(* intersectPathLists : pathList * pathList -> pathList
 *  produces pathList containing members of first pathList that
 *  also occur in the second pathList *)
fun intersectPathLists(paths1, nil) = nil
  | intersectPathLists(nil, paths2) = nil
  | intersectPathLists(path1::rest1, paths2) =
      if inPathList(path1,paths2) then
        path1::(intersectPathLists(rest1, paths2))
      else
        intersectPathLists(rest1, paths2)

(* differencPathLists: pathList * pathList -> pathList
 *  sublist of first pathList containing paths that do not occur
 *  in second pathList *)
fun differencePathLists(paths1, nil) = paths1
  | differencePathLists(nil, paths2) = nil
  | differencePathLists(path1::rest1, paths2) =
      if inPathList(path1,paths2) then
        differencePathLists(rest1, paths2)
      else
        path1::(differencePathLists(rest1, paths2))

(* intersectPathsets : pathSet * pathSet -> pathSet
 *   intersection pathSet contains only elements with a common
 *   index, with the corresponding pathList being the intersection
 *   of the respective pathLists
 *)
fun intersectPathsets(pathset1, nil) = nil
  | intersectPathsets(nil, pathset2) = nil
  | intersectPathsets(pathset1 as (n1:int, paths1)::rest1,
                      pathset2 as (n2, paths2)::rest2) =
      if n1 = n2 then
        case intersectPathLists(paths1, paths2)
          of nil => intersectPathsets(rest1, rest2)
           | pl => (n1, pl)::(intersectPathsets(rest1, rest2))
      else if n1 < n2 then
        intersectPathsets(rest1, pathset2)
      else
        intersectPathsets(pathset1, rest2)

(* unitePathsets : pathSet * pathSet -> pathSet
 *  merge two pathSets, consolidating elements with same index
 *  by taking union of corresponding pathLists *)
fun unitePathsets(pathset1, nil) = pathset1
  | unitePathsets(nil, pathset2) = pathset2
  | unitePathsets(pathset1 as (n1:int, paths1)::rest1,
                  pathset2 as (n2, paths2)::rest2) =
      if n1 = n2 then
        (n1, unitePathLists(paths1, paths2))
          :: (unitePathsets(rest1, rest2))
      else if n1 < n2 then
        (n1, paths1)::(unitePathsets(rest1, pathset2))
      else
        (n2, paths2)::(unitePathsets(pathset1, rest2))

(* differencePathsets : pathSet * pathSet -> pathSet
 *  to form the result pathSet:
 *  for each element (n, pl) of pathSet 1, if there is a corresponding
 *  element (n, pl') of pathSet 2 and pl_new = pl - pl' is not nil,
 *  retain the modified element (n, pl_new), otherwise drop the element.
*)
fun differencePathsets(pathset1: pathSet, nil: pathSet) = pathset1
  | differencePathsets(nil, pathset2) = nil
  | differencePathsets(pathset1 as (n1, paths1)::rest1,
                       pathset2 as (n2, paths2)::rest2) =
      if n1 = n2 then
        case differencePathLists(paths1, paths2)
          of nil => differencePathsets(rest1, rest2)
           | pl => (n1, pl)::(differencePathsets(rest1, rest2))
      else if n1 < n2 then
        (n1, paths1)::(differencePathsets(rest1, pathset2))
      else
        differencePathsets(pathset1, rest2)

(* dividePathset : (path -> bool) * pathset -> pathset * pathset
 *  form two pathSets by splitting pathSet elements into two elements
 *  having pathLists that satisfy or don't satisfy the predicate,
 *  dropping new elements if their pathList in nil.  So either or
 *  both of the result pathSets may have fewer elements than the original *)
fun dividePathset(pred, nil) = (nil, nil)
  | dividePathset(pred, (n, paths)::rest) =
      let val (yesSet, noSet) = dividePathset(pred, rest)
      in case dividePathList(pred, paths)
           of (nil, nil) => bug "diviePathset - both empty"
            | (nil, no) => (yesSet, (n,no)::noSet)
            | (yes, nil) => ((n, yes)::yesSet, noSet)
            | (yes, no) => ((n, yes)::yesSet, (n,no)::noSet)
      end

(* pathDepends : path -> path -> bool
 *  is path 1 a "suffix" of path 2  -- need to define "suffix" of a path,
 *  which seems clear when path is "linear" *)
fun pathDepends path1 ROOTPATH = pathEq(path1, ROOTPATH)
  | pathDepends path1 (path2 as PIPATH(_, subpath)) =
      pathEq(path1, path2) orelse pathDepends path1 subpath
  | pathDepends path1 (path2 as VPIPATH(_,_,subpath)) =
      pathEq(path1, path2) orelse pathDepends path1 subpath
  | pathDepends path1 (path2 as DELTAPATH(_,subpath)) =
      pathEq(path1, path2) orelse pathDepends path1 subpath
  | pathDepends path1 (path2 as (VLENPATH (_, subpath))) =
      pathEq(path1, path2) orelse pathDepends path1 subpath

(* pathLength : path -> int
 *  for linear paths, the "length" or "depth" - 1  (ROOTPATH ~ nil) *)
fun pathLength (ROOTPATH) = 0
  | pathLength (PIPATH(_, subpath)) =
      1 + pathLength subpath
  | pathLength (VPIPATH(_,_,subpath)) =
      1 + pathLength subpath
  | pathLength (DELTAPATH(_,subpath)) =
      1 + pathLength subpath
  | pathLength (VLENPATH (_, subpath)) =
      1 + pathLength subpath

(* addPathToPathset : path * pathSet -> pathSet
 *  add, nonredundantly, path to the pathSet element whose index
 *  matches the path metric of path, adding a new element if no index
 *  matches the path metric.
 *  Not called anywhere.
 *)
fun addPathToPathset (path, pathset) =
    let fun add(path, metric, nil) = [(metric, [path])]
	  | add(path, metric, (n:int, paths)::rest) =
	    if n = metric then (n, addPathToPathList(path, paths))::rest
	    else if n < metric then
		(n,paths) :: add(path, metric, rest)
	    else (metric, [path])::(n, paths)::rest
    in add (path, pathLength path, pathset)
    end

(* wrapBindings : pathSet * dectree -> dectree
 *   wrap the dectree in BINDs, for all the paths in pathSet, first outermost *)
fun wrapBindings (nil, rhs) = rhs
  | wrapBindings ((_,paths)::rest, rhs) =
    let fun bind(nil, rhs) = rhs
          | bind(path::rest, rhs) = BIND{path=path, dectree=bind(rest, rhs)}
     in bind(paths, wrapBindings(rest, rhs))
    end

(* subPaths : path -> pathSet
 *  deconstruct a path into a pathSet *)
fun subPaths ROOTPATH = [(0, [ROOTPATH])]
  | subPaths (path as (VLENPATH (_, subpath))) =
      (subPaths subpath)@[(pathLength path, [path])]
  | subPaths (path as VPIPATH (_,_,subpath)) =
      (subPaths subpath)@[(pathLength path, [path])]
  | subPaths (path as PIPATH (_,subpath)) =
      (subPaths subpath)@[(pathLength path, [path])]
  | subPaths (path as DELTAPATH (_,subpath)) =
      (subPaths subpath)@[(pathLength path, [path])]

(* rhsbindings : int * ruleRepTy -> pathSet
 *  select the nth rule description, then select its pathList (2nd) component
 *  translate each path into a pathSet, and merge the pathSets.
 *  Called once in pass1.
 *)
fun rhsbindings (n, matchRep) =
     let val (_, paths, _) = List.nth(matchRep, n)
      in foldr unitePathsets [] (map subPaths paths)
     end

(* pass1cases : (pcon * dectree) list * pathSet * pathSet option * ruleRepTy * path
 *              -> (pcon * dectree) list * pathSet *)
fun pass1cases ((pcon,subtree)::rest, envin, SOME envout, rhs, path) =
        let val (subtree', myEnvout) = pass1(subtree, envin, rhs)
            val (mustBindHere, otherBindings) =
                  dividePathset(pathDepends(DELTAPATH(pcon,path)),myEnvout)
            val envoutSoFar = intersectPathsets(envout, otherBindings)
            val (rest', envout') =
                  pass1cases(rest, envin, SOME envoutSoFar, rhs, path)
            val iBind2 = differencePathsets(otherBindings, envout')
            val subtree'' =
                  wrapBindings(unitePathsets(mustBindHere, iBind2), subtree')
         in ((pcon,subtree'')::rest', envout')
        end
  | pass1cases ((pcon,subtree)::rest, envin, NONE, rhs, path) =
        let val (subtree', myEnvout) = pass1(subtree, envin, rhs)
            val (mustBindHere, otherBindings) =
                  dividePathset(pathDepends(DELTAPATH(pcon,path)),myEnvout)
            val (rest', envout') =
                  pass1cases(rest, envin, SOME otherBindings, rhs, path)
            val iBind2 = differencePathsets(otherBindings, envout')
            val subtree'' =
                  wrapBindings(unitePathsets(mustBindHere, iBind2), subtree')
         in ((pcon,subtree'')::rest', envout')
        end
  | pass1cases (nil, envin, SOME envout, rhs, path) =
        (nil, unitePathsets(envin, envout))
  | pass1cases (nil, envin, NONE, rhs, path) = bug "pass1cases - unexpected arg"

(* pass1 : dectree * pathSet * ruleRepTy -> dectree * pathSet *)
and pass1(RHS n, _, rhs) = (RHS n, rhsbindings(n, rhs))
  | pass1(CASETEST{path, sign, caseTrees, default=NONE}, paths, rhs) =
      let val (caseTrees', paths') =
		pass1cases(caseTrees, unitePathsets(paths, subPaths path),
			   NONE, rhs, path)
      in (CASETEST{path=path, sign=sign, caseTrees=caseTrees', default=NONE},
	  paths')
      end
  | pass1(CASETEST{path, sign, caseTrees, default=SOME subtree}, paths, rhs) =
      let val paths1 = unitePathsets(paths, subPaths path)
	  val (subtree', paths2) = pass1(subtree, paths1, rhs)
	  val (caseTrees', paths3) =
		pass1cases(caseTrees, paths1, SOME paths2, rhs, path)
	  val subbindings = differencePathsets(paths2, paths3)
	  val subtree'' = wrapBindings(subbindings, subtree')
      in (CASETEST{path=path, sign=sign, caseTrees=caseTrees', default=SOME subtree''},
	  paths3)
      end
  | pass1 _ = bug "pass1 - unexpected arg"


(* generate : dectree * matchRep * lvar * (toTycTy * toLtyTy) * genintinfswitch
 *            -> codeTy
 * Given a decision tree for a match, a matchRep list and the lvar
 * bound to the value to be matched, produce code for the match.
 *)
fun generate (dt, matchRep, rootVar, (toTyc, toLty), giis) =
  let val (subtree, envout) = pass1(dt, [(0, [ROOTPATH])], matchRep)
      fun mkDcon (DATACON {name, rep, typ, ...}) =
            (name, rep, toDconLty toLty typ)
      fun genpath (PIPATH(n, path), env) =
            SELECT(n, VAR(lookupPath(path, env)))
        | genpath (p as DELTAPATH(pcon, path), env) =
            VAR(lookupPath(p, env))
        | genpath (VPIPATH(n, t, path), env) = let
            val tc = toTyc t
	    val lt_sub = let
                  val x = LT.ltc_vector (LT.ltc_tv 0)
                  in
		    LT.ltc_poly([LT.tkc_mono],
                      [LT.ltc_parrow(LT.ltc_tuple [x, LT.ltc_int], LT.ltc_tv 0)])
                  end
	    in
	      APP(
		PRIM(PO.SUBSCRIPTV, lt_sub, [tc]),
		RECORD[
		    VAR(lookupPath(path, env)),
		    INT{ival = IntInf.fromInt n, ty = Target.defaultIntSz}
		  ])
            end
        | genpath (VLENPATH (t, path), env) =
            let val tc = toTyc t
                val lt_len = LT.ltc_poly([LT.tkc_mono],
                                 [LT.ltc_parrow(LT.ltc_tv 0, LT.ltc_int)])
                val argtc = LT.tcc_vector tc
             in APP(PRIM(PO.LENGTH, lt_len, [argtc]),
                    VAR(lookupPath(path, env)))
            end
        | genpath (ROOTPATH, env) = VAR(lookupPath(ROOTPATH, env))

      fun genswitch (sv, sign, [(DATAcon((_, DA.REF, lt), ts, x), e)], NONE) =
            LET(x, APP (PRIM (Primop.DEREF, LT.lt_swap lt, ts), sv), e)
        | genswitch(sv, sign, [(DATAcon((_, DA.SUSP(SOME(_, DA.LVAR f)), lt),
                                        ts, x), e)], NONE) =
            let val v = mkv()
             in LET(x, LET(v, TAPP(VAR f, ts), APP(VAR v, sv)), e)
            end
	| genswitch (sv, sign, cases as ((INTcon{ty=0, ...}, _) :: _), default) =
	    let fun strip (INTcon{ty=0, ival}, e) = (ival, e)
		  | strip _ = bug "genswitch - INTINFcon"
	    in
		case default of
		    NONE => bug "getswitch - no default in switch on IntInf"
		  | SOME d => giis (sv, map strip cases, d)
	    end
        | genswitch x = SWITCH x

      fun pass2rhs (n, env, matchRep) =
        (case List.nth(matchRep, n)
          of (_, [path], fname) => APP(VAR fname, VAR(lookupPath(path, env)))
           | (_, paths, fname) =>
               APP(VAR fname,
                 RECORD (map (fn path => VAR(lookupPath(path, env))) paths)))

      fun pass2 (BIND{path=DELTAPATH _, dectree}, env, rhs) =
            pass2(dectree, env, rhs)
            (** we no longer generate explicit DECON anymore, instead,
                we add a binding at each switch case. *)
        | pass2 (BIND{path, dectree}, env, rhs) =
            let val newvar = mkv()
                val subcode = pass2(dectree, (path, newvar)::env, rhs)
             in LET(newvar, genpath(path, env), subcode)
            end
        | pass2 (CASETEST{path, sign, caseTrees=[], default=NONE}, _, _) =
            bug "pass2 - empty cases"
        | pass2 (CASETEST{path, sign, caseTrees=[], default=SOME subtree}, env, rhs) =
            pass2(subtree,env,rhs)
        | pass2 (CASETEST{path, sign, caseTrees, default}, env, rhs) =
            let val sv = VAR(lookupPath(path, env))
             in genswitch(sv, sign, pass2cases(path,caseTrees,env,rhs),
                          (case default
                            of NONE => NONE
                             | SOME subtree => SOME(pass2(subtree,env,rhs))))
            end
        | pass2 (RHS n, env, rhs) = pass2rhs(n, env, rhs)

      and pass2cases (path, nil, env, rhs) = nil
        | pass2cases (path, (pcon,subtree)::rest, env, rhs) =
            let (** always implicitly bind a new variable at each branch. *)
                val (ncon, nenv) = pconToCon(pcon, path, env)
                val res = (ncon, pass2(subtree, nenv, rhs))
             in res::(pass2cases(path, rest, env, rhs))
            end

      (* pconToCon : pcon * path * (path * lvar) list -> lexp * (path * lvar) list *)
      and pconToCon (pcon, path, env) =
	  (case pcon
	     of DATApcon (dc, ts) =>
		  let val newvar = mkv()
		      val nts = map (toTyc o TP.VARty) ts
		      val nenv = (DELTAPATH(pcon, path), newvar)::env
		   in (DATAcon (mkDcon dc, nts, newvar), nenv)
		  end
	      | VLENpcon(i, t) => (VLENcon i, env)
	      | INTpcon i => (INTcon i, env)
	      | WORDpcon w => (WORDcon w, env)
	      | STRINGpcon s => (STRINGcon s, env)
	    (* end case *))

   in case wrapBindings(envout, subtree)
       of BIND{path=ROOTPATH, dectree} =>
            pass2(dectree, [(ROOTPATH, rootVar)], matchRep)
        | _ => pass2(subtree, [], matchRep)
  end

(* matchComp :
   (Absyn.pat * Plambda.lexp) list   // rules: the match rules
   * (lexp -> lexp)   // finish: a "finishing" function that wraps a LET
   * lvar             // rootvar: a root lvar LET-bound to case subject by finishing fn
   * toLcLtTy         // toTcLt: Absyn type to FLINT type translators
   * errTy            // err: error reporting function
   * genintinfswitch  // giis: int inf switch generator
     -> lexp          // the translation of the match
      * ruleset       // indexes of unused rules
      * bool          // redundant match flag
      * bool          // exhaustive match flag (true if match is exhaustive)
*)
fun matchComp(rules, finish, rootvar, toTcLt as (_, toLty), err, giis) =
  let val lastRule : int = length rules - 1  (* rule no. of added default rule *)
      val multiRules : multiRule list = map (preProcessRule toLty) rules
      val (patterns,rhss) =
          foldr (fn ({pats, fname, rhsFun},(c,d)) => (map #1 pats@c,(fname,rhsFun)::d))
		([], []) multiRules
      val allRules = List.tabulate(length patterns, fn x => x);
          (* length patterns will be > length rules if any OR pats are expanded
           * by orExpand in preProcessRule *)

      val andor = makeAndor patterns
			    
      val choices = flattenAndor(andor, ROOTPATH, allRules)
				  
      val ready_delayed = fireConstraint(ROOTPATH, choices, nil, nil)

      val dt = genDecisionTree(ready_delayed,allRules)

      val _ = PPMatchComp.debugPrint debugging
	        ("#dectree#",
		  fn ppstrm => fn dt =>
		       PPMatchComp.ppDectree (!pd) ppstrm dt, dt)

      val numRules = length matchRep
      val rawUnusedRules = complement(0, numRules, rulesUsed dt)
      val unusedRules = fixupUnused(rawUnusedRules, multiRules)
      val exhaustiveF = member(lastRule, unusedRules)
      val redundantF = redundant(unusedRules, lastRule)

      fun g((fname, fbody), body) = LET(fname, fbody, body)
      val code = foldr g (generate(dt, matchRep(* ? *), rootvar, toTcLt, giis)) rhsRep

   in (finish(code), unusedRules, redundantF, exhaustiveF)
  end

(* type as_match = (Absyn.pat * Absyn.exp) list *)

(* noVarsInPat : as_match -> bool
 * Test pat, the guard pattern of the first rule of a match,
 * for the occurence of variables (including layering variables)
 * or wildcards. Return true if any are present, false otherwise.
 *)
fun noVarsInPat ((pat,_)::_) =
      let fun var WILDpat = true (* might want to flag this *)
            | var (VARpat _) = true
            | var (LAYEREDpat _) = true
            | var (CONSTRAINTpat(p,_)) = var p
            | var (APPpat(_,_,p)) = var p
            | var (RECORDpat{fields,...}) = List.exists (var o #2) fields
            | var (VECTORpat(pats,_)) = List.exists var pats
            | var (ORpat (pat1,pat2)) = var pat1 orelse var pat2
            | var _ = false
       in not(var pat)
      end
  | noVarsIn _ = bug "noVarsInPat - unexpected arg"


(*
 * The three entry points for the match compiler: bindCompile, handCompile,
 * and matchCompile.
 *
 * They take as arguments an environment (env); a match represented
 * as a list of pattern--lambda expression pairs (match); and a
 * function to use in printing warning messages (warn).
 *
 * env and warn are only used in the printing of diagnostic information.
 *
 * If the control flag Control.MC.printArgs is set, they print match.
 *
 * They call matchComp to actually compile match.
 * This returns a 4-tuple (code, unused, redundant, exhaustive).
 * code is lambda code that implements match.  unused
 * is a list of the indices of the unused rules.  redundant
 * and exhaustive are boolean flags which are set if
 * match is redundant or exhaustive respectively.
 *
 * They print warning messages as appropriate, as described below.
 * If the control flag Control.MC.printRet is set, they print code.
 *
 * They return code.
 *
 * They assume that match has one element for each rule of the match
 * to be compiled, in order, plus a single, additional, final element.
 * This element must have a pattern that is always matched
 * (in practice, it is either a variable or wildcard), and a
 * lambda expression that implements the appropriate behavior
 * for argument values that satisfy none of the guard patterns.
 * A pattern is exhaustive if this dummy rule is never used,
 * and is irredundant if all of the other rules are used.
 *)

local open Control.MC (* make various control flags visible *)
in

(*
 * bindCompile: Entry point for compiling matches induced by val declarations
 * (e.g., val listHead::listTail = list).
 * The match (rules) is a two  element list. The first rule corresponds
 * to the let binding itself, while the second is a default rule
 * (usually "_ => raise Bind") added, e.g. in the function mkVBs in
 * translate.sml, or by applying ElabUtil.completeMatch.
 * Thus the match itself will always be exhaustive, but the case where the
 * let binding per se is nonexhaustive will still be detected by matchComp
 * (see the comment above), and if the control flag Control.MC.bindNonExhaustiveWarn
 * is set then a nonexhaustive binding warning is printed. If the control
 * flag Control.MC.bindNoVariableWarn is set, and the first pattern
 * (i.e., the only non-dummy pattern) of match contains no variables or
 * wildcards, a warning is printed. Arguably, a pattern containing no
 * variables, but one or more wildcards, should also trigger a warning,
 * but this would cause warnings on constructions like
 * val _ = <exp>  and  val _:<ty> = <exp>.
 *)
fun bindCompile (env, rules, finish, rootv, toTcLt, err, giis) =
  let val _ =
          if !printArgs then (say "BC called with:"; MP.ppMatch env rules)
          else ()
      val (code, _, _, exhaustive) =
          matchComp(rules, finish, rootv, toTcLt, err, giis)

      val nonexhaustiveF =
	  not exhaustive andalso
	  (!bindNonExhaustiveWarn orelse !bindNonExhaustiveError)
      val noVarsF = !bindNoVariableWarn andalso noVarsInPat rules

   in if nonexhaustiveF
      then err (if !bindNonExhaustiveError then EM.COMPLAIN else EM.WARN)
	       ("binding not exhaustive" ^
	          (if noVarsF then " and contains no variables" else ""))
	       (bindPrint(env,rules))
      else if noVarsF
           then err EM.WARN "binding contains no variables"
                    (bindPrint(env,rules))
           else ();

      if !printRet then
        (say "MC:  returns with\n"; ppLexp code)
      else ();
      code
  end

(*
 * Entry point for compiling matches induced by exception handlers.
 * (e.g., handle Bind => Foo).  If the control flag
 *  Control.MC.matchRedundantWarn is set, and match is redundant,
 *  a warning is printed.  If Control.MC.matchRedundantError is also
 *  set, the warning is promoted to an error message.
 *)
fun handCompile (env, rules, finish, rootv, toTcLt, err, giis) =
  let val _ =
        if !printArgs then (say "HC called with: "; MP.ppMatch env rules)
        else ()
      val (code, unused, redundant, _) =
        matchComp(rules, finish, rootv, toTcLt, err, giis)
      val  redundantF= !matchRedundantWarn andalso redundant

   in if redundantF
      then err
	     (if !matchRedundantError then EM.COMPLAIN else EM.WARN)
	     "redundant patterns in match"
             (matchPrint(env,rules,unused))
      else ();

      if !printRet
      then (say "MC:  returns with\n"; ppLexp code)
      else ();
      code
  end

(*
 * Entry point for compiling matches induced by function expressions
 * (and thus case expression, if-then-else expressions, while expressions
 * and fun declarations) (e.g., fn (x::y) => ([x],y)). If the control flag
 * Control.MC.matchRedundantWarn is set, and match is redundant, a warning
 * is printed; if Control.MC.matchRedundantError is also set, the warning
 * is promoted to an error. If the control flag Control.MC.matchExhaustive
 * is set, and match is nonexhaustive, a warning is printed.
 *)
fun matchCompile (env, rules, finish, rootv, toTcLt, err, giis) =
  let val _ =
        if !printArgs then (say "MC called with: "; MP.ppMatch env rules)
        else ()
      val (code, unused, redundant, exhaustive) =
        matchComp(rules, finish, rootv, toTcLt, err, giis)

      val nonexhaustiveF =
	  not exhaustive andalso
	  (!matchNonExhaustiveError orelse !matchNonExhaustiveWarn)
      val redundantF =
	  redundant andalso (!matchRedundantError orelse !matchRedundantWarn)
   in case (nonexhaustiveF,redundantF)
       of (true, true) =>
            err (if !matchRedundantError orelse !matchNonExhaustiveError
		     then EM.COMPLAIN else EM.WARN)
	        "match redundant and nonexhaustive"
	        (matchPrint(env, rules, unused))

        | (true, false) =>
            err (if !matchNonExhaustiveError then EM.COMPLAIN else EM.WARN)
                "match nonexhaustive"
		(matchPrint(env, rules, unused))

        | (false, true) =>
            err (if !matchRedundantError then EM.COMPLAIN else EM.WARN)
	      "match redundant" (matchPrint(env, rules, unused))

        | _ => ();

      if (!printRet)
      then (say "MatchComp:  returns with\n"; ppLexp code) else ();
      code
  end


val matchCompile =
  Stats.doPhase(Stats.makePhase "Compiler 045 matchcomp") matchCompile

end (* local Control.MC *)

end (* topleve local *)
end (* structure MatchComp *)
