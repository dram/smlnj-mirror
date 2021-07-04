(* lexp match code generation from 110.9.1 *)

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
  | pathDepends path1 (path2 as CONPATH(_,subpath)) =
      pathEq(path1, path2) orelse pathDepends path1 subpath
  | pathDepends path1 (path2 as (VLENPATH (_, subpath))) =
      pathEq(path1, path2) orelse pathDepends path1 subpath

(* pathLength : path -> int
 *  the length of the path - 1  (ROOTPATH ~ nil) *)
fun pathLength ROOTPATH = 0
  | pathLength (PIPATH(_, subpath)) =
      1 + pathLength subpath
  | pathLength (VPIPATH(_,_,subpath)) =
      1 + pathLength subpath
  | pathLength (CONPATH(_,subpath)) =
      1 + pathLength subpath
  | pathLength (VLENPATH (_, subpath)) =
      1 + pathLength subpath

(* addPathToPathset : path * pathSet -> pathSet
 *  add, nonredundantly, path to the pathSet element whose index
 *  matches the path metric of path, adding a new element if no index
 *  matches the path metric
 *)
fun addPathToPathset (path, pathset) =
    let fun add(path, len, nil) = [(len, [path])]
	  | add(path, len, (n:int, paths)::rest) =
	    if n = len then (n, addPathToPathList(path, paths))::rest
	    else if n < len then
		(n,paths) :: add(path, len, rest)
	    else (len, [path])::(n, paths)::rest
    in add (path, pathLength path, pathset)
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
  | subPaths (path as CONPATH (_,subpath)) =
      (subPaths subpath)@[(pathLength path, [path])]

(* rhsbindings : int * matchRepTy -> pathSet
 *  select the nth rule description, then select its pathList (2nd) component
 *  translate each path into a pathSet, and merge the pathSets
 *)
fun rhsbindings (n, matchRep) =
     let val (_, paths, _) = List.nth(matchRep, n)
      in foldr unitePathsets [] (map subPaths paths)
     end

(* wrapBindings : pathSet * dectree -> dectree
 *   wrap the dectree in BINDs, for all the paths in pathSet, first outermost *)
fun wrapBindings (nil, rhs) = rhs
  | wrapBindings ((_, paths)::rest, rhs) =
    let fun bind (nil, rhs) = rhs
          | bind (path::rest, rhs) = BIND (path, bind(rest, rhs))
     in bind (paths, wrapBindings (rest, rhs))
    end

(* pass1cases : (pcon * dectree) list * pathSet * pathSet option * matchRepTy * path
 *              -> (pcon * dectree) list * pathSet *)
fun pass1cases ((pcon,subtree)::rest, envin, SOME envout, rhs, path) =
        let val (subtree', myEnvout) = pass1(subtree, envin, rhs)
            val (mustBindHere, otherBindings) =
                  dividePathset(pathDepends(CONPATH(pcon,path)),myEnvout)
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
                  dividePathset(pathDepends(CONPATH(pcon,path)),myEnvout)
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

(* pass1 : dectree * pathSet * matchRepTy -> dectree * pathSet *)
and pass1(RHS n, envin, rhs) = (RHS n, rhsbindings(n, rhs))
  | pass1(CASETEST(path, sign, cases, NONE), envin, rhs) =
        let val (cases', envout') =
              pass1cases(cases, unitePathsets(envin, subPaths path),
                         NONE, rhs, path)
         in (CASETEST(path, sign, cases', NONE), envout')
        end
  | pass1(CASETEST(path, sign, cases, SOME subtree), envin, rhs) =
        let val newenv = unitePathsets(envin, subPaths path)
            val (subtree', subEnvout) = pass1(subtree, newenv, rhs)
            val (cases', envout') =
              pass1cases(cases, newenv, SOME subEnvout, rhs, path)
            val subbindings = differencePathsets(subEnvout, envout')
            val subtree'' = wrapBindings(subbindings, subtree')
         in (CASETEST(path, sign, cases', SOME subtree''), envout')
        end
  | pass1 _ = bug "pass1 - unexpected arg"

type pathenv = (path * lvar) list
val lookupPath : path * pathenv -> lvar
				       
(* generate : dectree * matchRepTy * lvar * (toTycTy * toLtyTy) * giisTy
 *            -> codeTy
 * Given a decision tree for a match, a matchRep list and the lvar
 * bound to the value to be matched, produce code for the match.
 *)
fun generate (dectree, matchRep, rootVar, (toTyc, toLty), giis) =
  let
      val (subtree, envout) = pass1(dectree, [(0, [ROOTPATH])], matchRep)

      fun mkDcon (DATACON {name, rep, typ, ...}) =
            (name, rep, toDconLty toLty typ)

      (* genpath : path * pathenv -> lexp *)
      fun genpath (PIn :: path, pathenv) =
            SELECT(n, VAR(lookupPath(path, pathenv)))
        | genpath (path as CON pcon :: _), pathenv) =
            VAR(lookupPath(path, pathenv))
        | genpath (VPI(n, t) :: path, pathenv) =
	    let val tc = toTyc t
		val lt_sub =
                    let val x = LT.ltc_vector (LT.ltc_tv 0)
                    in LT.ltc_poly([LT.tkc_mono],
				   [LT.ltc_parrow(LT.ltc_tuple [x, LT.ltc_int], LT.ltc_tv 0)])
                    end
	    in APP(PRIM(PO.SUBSCRIPTV, lt_sub, [tc]),
		   RECORD[ VAR(lookupPath(path, pathenv)),
		           INT{ival = IntInf.fromInt n, ty = Target.defaultIntSz} ])
            end
        | genpath (VLEN t :: path), pathenv) =
            let val tc = toTyc t
                val lt_len = LT.ltc_poly([LT.tkc_mono],
                                 [LT.ltc_parrow(LT.ltc_tv 0, LT.ltc_int)])
                val argtc = LT.tcc_vector tc
             in APP(PRIM(PO.LENGTH, lt_len, [argtc]),
                    VAR(lookupPath(path, pathenv)))
            end
        | genpath (nil, env) = VAR(lookupPath(nil, pathenv))

      (* moved to trans/translate.sml for new match compiler *)
      (* just adds special-case treatments for deconstructing ref, susp, and intinf cases,
       * switch = SWITCH for the general case  *)
      fun switch (sv, sign, [(DATAcon((_, DA.REF, lt), ts, x), e)], NONE) =
            LET(x, APP (PRIM (Primop.DEREF, LT.lt_swap lt, ts), sv), e)
        | switch (sv, sign, [(DATAcon((_, DA.SUSP(SOME(_, DA.LVAR f)), lt),
                                        ts, x), e)], NONE) =
            let val v = mkv()
             in LET(x, LET(v, TAPP(VAR f, ts), APP(VAR v, sv)), e)
            end
	| switch (sv, sign, cases as ((INTcon{ty=0, ...}, _) :: _), default) =
	    let fun strip (INTcon{ty=0, ival}, e) = (ival, e)
		  | strip _ = bug "genswitch - INTINFcon"
	    in
		case default of
		    NONE => bug "getswitch - no default in switch on IntInf"
		  | SOME d => giis (sv, map strip cases, d)
	    end
        | switch x = SWITCH x

      fun genrhs (n, pathenv, matchRep) =
        (case List.nth(matchRep, n)
          of (_, [path], fname) => APP(VAR fname, VAR(lookupPath(path, pathenv)))
           | (_, paths, fname) =>
               APP(VAR fname,
                   RECORD (map (fn path => VAR(lookupPath(path, pathenv))) paths)))

      fun gen (BIND(CON _ :: _, subtree), pathenv, rhs) =
            gen (subtree, pathenv, rhs)
            (** we no longer generate explicit DECON anymore, instead,
                we add a binding at each switch case. *)
        | gen (BIND(path, subtree), pathenv, rhs) =
            let val newvar = mkv()
                val subcode = gen(subtree, (path, newvar)::pathenv, rhs)
             in LET(newvar, genpath(path, pathenv), subcode)
            end
        | gen (CASETEST(path, sign, [], NONE), _, _) =
            bug "gen - empty cases"
        | gen (CASETEST(path, sign, [], SOME subtree), pathenv, rhs) =
            gen (subtree, pathenv, rhs)
        | gen (CASETEST(path, sign, cases, defaultOp), pathenv, rhs) =
            let val switchVar = VAR(lookupPath(path, pathenv))
		val switchCases = gencases(path,cases,pathenv,rhs)
		val switchDefaultOp = Option.map (fn subtree => gen(subtree,pathenv,rhs)) defaultOp
             in switch(switchVar, sign, switchCases, switchDefaultOp)
            end
        | gen (RHS n, pathenv, rhs) = genrhs(n, pathenv, rhs)

      and gencases (path, nil, env, rhs) = nil
        | gencases(path, (pcon,subtree)::rest, env, rhs) =
            let (** always implicitly bind a new variable at each branch. *)
                val (ncon, nenv) = pconToCon(pcon, path, env)
                val res = (ncon, gen(subtree, nenv, rhs))
             in res::(gencases(path, rest, env, rhs))
            end

      (* pconToCon : pcon * path * (path * lvar) list -> lexp * (path * lvar) list *)
      and pconToCon (pcon, path, env) =
	  (case pcon
	     of DATApcon (dc, ts) =>
		  let val newvar = mkv()
		      val nts = map (toTyc o TP.VARty) ts
		      val nenv = (CONPATH(pcon, path), newvar)::env
		   in (DATAcon (mkDcon dc, nts, newvar), nenv)
		  end
	      | VLENpcon(i, t) => (VLENcon i, env)
	      | INTpcon i => (INTcon i, env)
	      | WORDpcon w => (WORDcon w, env)
	      | STRINGpcon s => (STRINGcon s, env)
	    (* end case *))

   in case wrapBindings (envout, subtree)
       of BIND(ROOTPATH, subtree') =>
            gen(subtree', [(ROOTPATH, rootVar)], matchRep)
        | _ => gen(subtree, [], matchRep)
  end
