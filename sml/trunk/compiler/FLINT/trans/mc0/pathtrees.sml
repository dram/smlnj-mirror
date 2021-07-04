fun bug s = raise Fail s

datatype link
  = PI of int  
  | VPI of int 
  | VLEN of int
  | CON of string

(* _reverse_ of path to an andor node *)
type path = link list

fun pathLength path = length path
fun linkEq (PI i1, PI i2) = (i1 = i2)
  | linkEq (VPI i1, VPI i2) = (i1 = i2)
  | linkEq (VLEN _, VLEN _) = true
  | linkEq (CON c1, CON c2) = (c1 = c2)
  | linkEq _ = false

fun pathEq (path1, path2) = ListPair.allEq linkEq (path1, path2)

type pathList = path list
(* conjectured invariant: a pathList has no duplicate members *)
(* QUESTION: does the order of paths in a path list matter? Apparently not. *)

type pathSet = (int * pathList) list

(* dividePathList: (path -> bool) * pathList -> pathList * pathList
 *   divide path list into a pair of a list of paths satisfying pred and
 *   a list of paths not satisfying pred. Reverses order of the original
 *   lists in the result lists *)
fun dividePathList (pred: path -> bool, paths: pathList) =
    let fun divide(pred, nil, accyes, accno) = (accyes, accno)
	  | divide(pred, path::rest, accyes, accno) =
	    if pred path then divide(pred, rest, path::accyes, accno)
	    else divide(pred, rest, accyes, path::accno)
    in  divide(pred, paths, nil, nil)
    end

(* addPathToPathList : path * pathList -> pathList
 *  add path to the end of pathList iff it is not already in pathList
 *  if used exclusively to creat paths list, guantees that path lists
 *  contain no duplications *)
fun addPathToPathList (path: path, (paths as path1::rest): pathList) =
      if pathEq(path, path1) then paths (* avoid duplicates *)
      else path1::(addPathToPathList(path, rest))
  | addPathToPathList (path, nil) = [path]

(* unitePathLists : pathList * pathList -> pathList
 *  merge two pathLists, suppressing duplicate paths; the "new" (relative to paths2)
 *  members of the first pathlist are added to the "front" of the 2nd pathList in
 *  reverse order (unless the 2nd pathlist is nil, when paths1 is returned unchanged) *)
fun unitePathLists(paths1: pathList, nil: pathList) : pathList = paths1
  | unitePathLists(nil, paths2) = paths2
  | unitePathLists(path1::rest1, paths2) =
      addPathToPathList(path1, unitePathLists(rest1, paths2))

(* inPathList : path * pathList -> bool
 *  path is equal (pathEq) to a member of pathList *)
fun inPathList (path1: path, nil: pathList): bool = false
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
fun differencePathLists(paths1: pathList, nil: pathList): pathList = paths1
  | differencePathLists(nil, paths2) = nil
  | differencePathLists(path1::rest1, paths2) =
      if inPathList(path1,paths2)
      then differencePathLists(rest1, paths2)
      else path1::(differencePathLists(rest1, paths2))

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

(* suffix : path -> path -> bool
 *  (the reverse of) path 1 is a "prefix" of (the reverse of) path 2 *)
fun suffix path1 nil = pathEq(path1, nil)
  | suffix path1 (path2 as (_ :: subpath)) =
      pathEq(path1, path2) orelse suffix path1 subpath

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
fun subPaths nil = [(0, [nil])]
  | subPaths (path as (_ :: subpath)) =
      (subPaths subpath) @ [(pathLength path, [path])]

(* example paths *)

val p0 = [];
val p1 = [PI 2];
val p2 = rev [CON "cons", PI 1];
val p3 = rev [VLEN 0, VPI 2];
val p4 = p2 @ p2;
