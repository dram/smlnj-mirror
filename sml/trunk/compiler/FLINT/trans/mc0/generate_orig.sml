(* FLINT/trans/generate.sml *)

(* "code" (lexp) generation *)

structure Generate =
struct

local
  structure A = Access
  structure T = Types
  structure BT = BasicTypes
  structure PO = Primop
  structure LT = PLambdaType
  structure PL = PLambda
  structure MT = MCCommon
  open MCCommon

  val mkv = LambdaVar.mkLvar 
in

type path = MT.path

(* --------------------------------------------------------------------------- *)
(* pathList : list (sets) of paths *)

type pathList = path list
(* conjectured invariant: a pathList has no duplicate members *)
(* QUESTION: does the order of paths in a path list matter? Apparently not. *)

(* memberPL : path * pathList -> bool
 *  path is equal (pathEq) to a member of pathList *)
fun memberPL (path1: path, nil: pathList): bool = false
  | memberPL (path1, path2::rest) =
      pathEq(path1, path2) orelse memberPL(path1, rest)

(* addPL : path * pathList -> pathList
 *  add path to the end of pathList iff it is not already in pathList *)
fun addPL (path: path, (paths as path1::rest): pathList) =
      if MT.pathEq(path, path1) then paths
      else path1::(addPL(path, rest))
  | addPL (path, nil) = [path]

(* suffix : path -> path -> bool
 *  is path1 a suffix of path2, or, equivalently, (rev path1) is a prefix
 *  of (rev path2) *)
fun suffix path1 nil = pathEq(path1, nil)
  | suffix path1 (path2 as (_ :: tail2)) =
      pathEq(path1, path2) orelse suffix path1 tail2

(* dividePL: (path -> bool) * pathList -> pathList * pathList
 *   divide path list into a pair of a list of paths satisfying pred and
 *   a list of paths not satisfying pred. *)
fun dividePL (pred: path -> bool, paths: pathList) : pathList * pathList =
    let fun divide(pred, nil, accyes, accno) = (accyes, accno)
	  | divide(pred, path::rest, accyes, accno) =
	    if pred path then divide(pred, rest, path::accyes, accno)
	    else divide(pred, rest, accyes, path::accno)
    in  divide(pred, paths, nil, nil)
    end

(* unionPL : pathList * pathList -> pathList
 *  merge two pathLists, suppressing duplicate paths; the new members of
 *  the first pathlist are added to the end of the 2nd pathList in reverse
 *  order *)
fun unionPL(paths1: pathList, nil: pathList) : pathList = paths1
  | unionPL(nil, paths2) = paths2
  | unionPL(path1::rest1, paths2) =
      addPL(path1, unionPL(rest1, paths2))

(* intersectionPL : pathList * pathList -> pathList
 *  produces pathList containing members of first pathList that
 *  also occur in the second pathList *)
fun intersectionPL (paths1, nil) = nil
  | intersectionPL (nil, paths2) = nil
  | intersectionPL (path1::rest1, paths2) =
      if memberPL (path1,paths2)
      then path1::(intersectionPL(rest1, paths2))
      else intersectionPL(rest1, paths2)

(* differencPL: pathList * pathList -> pathList
 *  sublist of first pathList containing paths that do not occur
 *  in second pathList *)
fun differencePL (paths1: pathList, nil: pathList): pathList = paths1
  | differencePL (nil, paths2) = nil
  | differencePL (path1::rest1, paths2) =
      if memberPL (path1,paths2)
      then differencePL (rest1, paths2)
      else path1::(differencePL(rest1, paths2))


(* --------------------------------------------------------------------------- *)
(* pathSets *)

type pathSet = (int * pathList) list
(* terminology: the int component of a pathSet member is called its index.
 * invariant?: index represents common "depth" of paths in pathList, or
 *    in other words, all paths in the pathList have metric = index
 * invariant: indexes occur in (strictly?) ascending order
 * invariant?: no duplicate indexes
 *)

(* subPaths : path -> pathSet
 *  deconstruct a path into a (linear) pathSet, with indexes running from 
 *  0 to lenth path and all the pathLists of length 1 containing successive
 *  prefixes of the initial path *)
fun subPaths (nil: path) : pathSet = [(0, [nil])]
  | subPaths (path as (_ :: pathTail)) =
      (subPaths pathTail) @ [(pathLength path, [path])]

(* unionPS : pathSet * pathSet -> pathSet
 *  merge two pathSets, consolidating elements with same index
 *  by taking union of corresponding pathLists; continue with the longer
 *  if not equal lengths *)
fun unionPS(pathset1: pathSet, nil: pathSet): pathSet = pathset1
  | unionPS(nil, pathset2) = pathset2
  | unionPS(pathset1 as (n1, paths1)::rest1,
            pathset2 as (n2, paths2)::rest2) =
      if n1 = n2
      then (n1, unionPL(paths1, paths2)) :: (unionPS(rest1, rest2))
      else if n1 < n2
      then (n1, paths1)::(unionPS(rest1, pathset2))
      else (n2, paths2)::(unionPS(pathset1, rest2))

(* intersectionPS : pathSet * pathSet -> pathSet
 *   intersection pathSet contains only elements with a common
 *   index, with the corresponding pathList being the intersection
 *   of the respective pathLists, if non-empty
 *)
fun intersectionPS(pathset1: pathSet, nil: pathSet): pathSet = nil
  | intersectionPS(nil, pathset2) = nil
  | intersectionPS(pathset1 as (n1:int, paths1)::rest1,
                      pathset2 as (n2, paths2)::rest2) =
      if n1 = n2
      then case intersectionPL(paths1, paths2)
             of nil => intersectionPS(rest1, rest2)
              | paths => (n1, paths)::(intersectionPS(rest1, rest2))
      else if n1 < n2
      then intersectionPS(rest1, pathset2)
      else intersectionPS(pathset1, rest2)

(* differencePS : pathSet * pathSet -> pathSet
 *  to form the result pathSet:
 *  for each element (n, pl) of pathSet 1, if there is a corresponding
 *  element (n, pl') of pathSet 2 and pl_new = pl - pl' is not nil,
 *  retain the modified element (n, pl_new), otherwise drop the element.
*)
fun differencePS(pathset1: pathSet, nil: pathSet) : pathSet = pathset1
  | differencePS(nil, pathset2) = nil
  | differencePS(pathset1 as (n1, paths1)::rest1,
                       pathset2 as (n2, paths2)::rest2) =
      if n1 = n2
      then case differencePL(paths1, paths2)
             of nil => differencePS(rest1, rest2)
              | pl => (n1, pl)::(differencePS(rest1, rest2))
      else if n1 < n2
      then (n1, paths1)::(differencePS(rest1, pathset2))
      else differencePS(pathset1, rest2)

(* dividePS : (path -> bool) * pathSet -> pathSet * pathSet
 *  form two pathSets by splitting pathSet elements into two elements
 *  having pathLists that satisfy or don't satisfy the predicate,
 *  dropping new elements if their pathList in nil.  So either or
 *  both of the result pathSets may have fewer elements than the original *)
fun dividePS(pred : path -> bool, nil: pathSet) : pathSet * pathSet = (nil, nil)
  | dividePS(pred, (n, paths)::rest) =
      let val (yesSet, noSet) = dividePS(pred, rest)
      in case dividePL(pred, paths)
           of (nil, nil) => bug "dividePS - both empty"
            | (nil, no) => (yesSet, (n,no)::noSet)
            | (yes, nil) => ((n, yes)::yesSet, noSet)
            | (yes, no) => ((n, yes)::yesSet, (n,no)::noSet)
      end

(* -------------------------------------------------------------------- *)
(* translation to PLambda.lexp -- "code generation" *)

 
(* lhsToPathSet : int * ramifiedLHS -> pathSet
 *  select the nth rule description, then select its pathList (2nd) component
 *  translate each path into a pathSet, and merge the pathSets. This collects all
 *  the variable paths from the ramified patterns of the (original) rule.
 *)
fun lhsToPathSet (n: int, rlhs: ramifiedLHS): pathSet =
     let val (_, paths, _) = List.nth(rlhs, n)
      in foldr unionPS [] (map subPaths paths)
     end

(* wrapBindings : pathSet * dectree -> dectree
 *  wrap the dectree in BINDs, for all the paths in pathSet, first outermost.
 *  outer iteration over the elements of the pathSet, inner iteration over
 *  the elements of the pathList of each pathSet element *)
fun wrapBindings (nil: pathSet, dectree) = dectree
  | wrapBindings ((_, paths)::rest, dectreee) =
      let fun bind (pathList, dectree) = foldr BIND pathList dectree
       in bind(paths, wrapBindings(rest, dectree))
      end

(* pass1 : dectree * pathSet * ramifiedLHS -> dectree * pathSet *)
fun pass1 (dectree, pathSet, rlhs) =
    let fun pass1x (RHS n, _) = (RHS n, lhsToPathSet(n, rlhs))
	  | pass1x (CHOICE {path, sign, cases, default = NONE}, inps) =
	      let val (cases', outps') =
		      pass1Cases (cases, unionPS(inps, subPaths path), NONE, path)
	       in (CHOICE {path = path, sign = sign, cases = cases', default = NONE}, outps')
	      end
	  | pass1x (CHOICE {path, sign, cases, default = SOME defaultDT}, inps) =
	      let val newps = unionPS (inps, subPaths path)
		  val (defaultDT0, defaultOutps) = pass1x (defaultDT, newps)
		  val (cases', outps') = pass1Cases (cases, newps, SOME defaultOutps, path)
		  val defaultBindings = differencePS (defaultOutps, outps')
		  val defaultDT1 = wrapBindings (defaultBindings, defaultDT0)
	       in (CHOICE {path = path, sign = sign, cases = cases', default = SOME defaultDT1},
		   outps')
	      end
	  | pass1x (BIND _, _, _) = bug "pass1x: BIND"
              (* BINDs are introduced (by calling wrapBindings) _after_ calls of pass1 *)

	(* pass1cases : (con * dectree) list * pathSet * pathSet option * path * ramifiedLHS
	 *              -> (con * dectree) list * pathSet *)
	and pass1cases ((con, dectree) :: rest, inps, outpsOp, path) =
	      let val (dectree0, outps0) = pass1x(dectree, inps)
		  val (mustBindHere, otherBindings) =
			dividePS (suffix (MT.CON con :: path), outps0)
		  val outps1 =
		      case outpsOp
		       of SOME outps => intersectionPS (outps, otherBindings)
			| NONE => otherBindings
		  val (restCases, outpsRest) = pass1cases (rest, inps, SOME outps1, path)
		  val bindPathSet = unionPS (mustBindHere, differencePS (otherBindings, outpsRest))
		  val dectree1 = wrapBindings (bindPathSet, dectree0)
	       in ((con, dectree1) :: restCases, outpsRest)
	      end
	  | pass1cases (nil, inps, outpsOp, path) =
	      (case outpsOp
		 of SOME outps => (nil, unionPS (inps, outps))
		  | NONE => bug "pass1cases")

    in pass1x (dectree, pathset)
    end

(* generate : dectree * ramifiedLHS * lvar
 *            * (toTycTy * toLtyTy) * giisTy
 *            -> PL.lexp
 *  Given a decision tree for a match, a matchRep list and the lvar
 *  bound to the value to be matched, produce code for the match.
 *)
fun generate (dectree, rlhs, rootVar, (toTyc, toLty), giis) =
  let 
      (* transDcon : T.datacon -> PL.dataconstr
       *  uses toLty arg of generate; used only in conToCon *)
      fun transDcon (T.DATACON {name, rep, typ, ...}) =
	  let val lty = (* translation of the datacon type *)
		  (case typ
		     of T.POLYty{sign, tyfun=T.TYFUN{arity, body}} =>
			if BT.isArrowType body then toLty typ
			else toLty (T.POLYty{sign=sign,
					      tyfun=T.TYFUN{arity=arity,
							     body=BT.-->(BT.unitTy, body)}})
		      | _ => if BT.isArrowType ty then toLty typ
			     else toLty (BT.-->(BT.unitTy, typ)))
	   in (name, rep, lty)
	  end

      (* conToCon : MT.con * path * pathLvarEnv -> PL.con * pathLvarEnv
       *  translates MCCommon.con to PLambda.con and introduces a variable naming
       *  destruct of a non-nullary datacon *)
      fun conToCon (con, path, env) =
	  (case con
	     of MT.DATAcon (datacon, ts) =>
		  let val newvar = mkv ()  (* new lvar that destructed value will be bound to *)
		      val nts = map (toTyc o T.VARty) ts
		      val newps = bindPath (MT.CON con :: path, newvar, env)
		   in (PL.DATAcon (transDcon datacon, nts, newvar), newps)
		  end
	      | MT.VLENcon(i, t) => (PL.VLENcon i, env)
	      | MT.INTcon i => (PL.INTcon i, env)
	      | MT.WORDcon w => (PL.WORDcon w, env)
	      | MT.STRINGcon s => (PL.STRINGcon s, env)
	    (* end case *))

      (* genpath : path * pathLvarEnv -> PL.lexp *)
      fun genpath (PI n :: path : path, env : pathLvarEnv): PL.lexp =
            PL.SELECT (n, PL.VAR (lookupPath (path, env)))
        | genpath (p as (MT.CON con :: path), env) =
            PL.VAR (lookupPath(p, env))  (* the "case variable" for this con case *)
        | genpath (VPI (n, ty) :: path, env) =
            (* "path" leads to the vector node, and the full path to the nth vector element *)
	    let val tyc = toTyc ty
		val lt_sub =
                    let val x = LT.ltc_vector (LT.ltc_tv 0)
                    in LT.ltc_poly([LT.tkc_mono],
				   [LT.ltc_parrow(LT.ltc_tuple [x, LT.ltc_int], LT.ltc_tv 0)])
                    end
	    in PL.APP(PL.PRIM(PO.SUBSCRIPTV, lt_sub, [tyc]), (* apply vec. subscript primop *)
		 PL.RECORD [ PL.VAR (lookupPath(path, env)),  (* the vector value *)
		             PL.INT {ival = IntInf.fromInt n, ty = Target.defaultIntSz} ]) (* the index *)
            end
        | genpath (VLEN t :: path, env) =  (* designated node is a vector length CHOICE *)
            let val tyc = toTyc t
                val lt_len = LT.ltc_poly([LT.tkc_mono],
                                 [LT.ltc_parrow(LT.ltc_tv 0, LT.ltc_int)])
                val argtyc = LT.tcc_vector tyc
             in PL.APP (PL.PRIM(PO.LENGTH, lt_len, [argtyc]),  (* apply vec length primop *)
			PL.VAR(lookupPath(path, env))) (* "path" designates the vector *)
            end
        | genpath (nil, env) = PL.VAR(lookupPath(nil, env))  (* the root node, return root variable *)

      (* genswitch : PL.lexp * A.consig * (con * lexp) list * lexp option -> PL.lexp
       *  (sv: lexp) will have form PL.VAR lvar, representing the subject of the switch *)
      fun genswitch (sv, _, [(PL.DATAcon((_, A.REF, lty), tycs, x), e)], NONE) =
            PL.LET(x, PL.APP (PL.PRIM (Primop.DEREF, LT.lt_swap lty, tycs), sv), e)
        | genswitch (sv, _, [(PL.DATAcon((_, A.SUSP(SOME(_, A.LVAR f)), lty), tycs, x), e)],
		     NONE) =
            let val v = mkv ()
             in PL.LET(x, PL.LET(v, PL.TAPP(PL.VAR f, tycs), PL.APP(PL.VAR v, sv)), e)
            end
	| genswitch (sv, _, cases as ((PL.INTcon{ty=0, ...}, _) :: _), default) = (* IntInf constant *)
	    let fun strip (PL.INTcon{ty=0, ival}, e) = (ival, e)
		  | strip _ = bug "genswitch - INTINFcon"
	    in case default of
		 of NONE => bug "getswitch - no default in switch on IntInf"
		  | SOME d => giis (sv, map strip cases, d)
	    end
        | genswitch x = PL.SWITCH x

      (* pass2rlhs : ruleno * pathLvarEnv * ramifiedLHS -> PL.lexp *)
      fun pass2rlhs (n, env, rlhs) =
	  (case List.nth (rlhs, n)
	     of (_, [path], fname) => PL.APP(PL.VAR fname, PL.VAR(lookupPath(path, env)))
	      | (_, paths, fname) =>
		 PL.APP(PL.VAR fname,
			PL.RECORD (map (fn path => PL.VAR(lookupPath(path, env))) paths)))

      (* pass2 : dectree * pathLvarEnv * ramifiedLHS -> PL.lexp *)
      fun pass2 (BIND(MT.CON _ :: _, dectree), env, rlhs) =
            pass2 (dectree, env, rlhs)   (* ignore BIND(MT.CON ...) around dectree *)
            (** we no longer generate explicit DECON anymore, instead,
                we add a binding at each switch case. *)
        | pass2 (BIND(path, subtree), env, rlhs) =
            let val newvar = mkv ()
                val subcode = pass2 (subtree, bindPath (path, newvar, env), rlhs)
             in PL.LET(newvar, genpath (path, env), subcode)
            end
        | pass2 (CHOICE {path, sign, cases = [], default = NONE}, _, _) =
            bug "pass2 - empty cases, no default"
        | pass2 (CHOICE {path, sign, cases = [], default = SOME defaultDT}, env, rlhs) =
            pass2 (defaultDT, env, rlhs)
        | pass2 (CHOICE {path, sign, cases, default}, env, rlhs) =
            let val switchVar = PL.VAR (lookupPath (path, env))
		val switchCases = pass2cases (path, cases, env, rlhs)
		val switchDefault = Option.map (fn dectree => pass2 (dectree, env, rlhs)) default
             in genswitch(switchVar, sign, switchCases, switchDefault)
            end
        | pass2 (RHS n, env, rlhs) = pass2rlhs (n, env, rlhs)

      (* pass2cases : path * (con * dectree) list * pathLvarEnv * ramifiedLHS
       *              -> (PL.con * PL.lexp) list *)
      and pass2cases (path, nil, env, rhs) = nil
        | pass2cases (path, (con, subtree)::rest, env, rhs) =
            let (** always implicitly bind a new variable at each branch. *)
                val (ncon, nenv) = conToCon (con, path, env)
                val res = (ncon, pass2 (subtree, nenv, rhs))
             in res :: pass2cases (path, rest, env, rhs)
            end

      val (dectree0, pathset) = pass1 (dectree, [(0, [nil])], rlhs)

   in case wrapBindings(pathset, dectree0)
        of BIND (nil, dectree1) =>
             pass2 (dectree1, [(nil, rootVar)], matchRep)
         | _ => pass2 (dectree0, [], matchRep)

  end (* fun generate *)

end (* local *)
end (* structure Generate *)
