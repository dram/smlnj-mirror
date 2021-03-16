(* matchcomp/layers.sml *)

(* layers -- refinement of (rule) layering *)

structure Layers =
struct

(* type layer -- a generalization of ruleno that accounts for OR-pattern
 * the integers in the int list will be 0 or 1, designating left or right,
 * thus the int list is a bit string representing left or right branches of
 * nested "OR patterns". Layers are ordered, with the ruleno dominating the
 * bitstring in the ordering.
 * INVARIANT: a pattern variable can "occur" only once along any hlpath
 * INVARIANT: a pattern variable can occur multiple times on "incomparable"
 *   hlpaths
 *)

type hlpath = int list  (* horizontal layer path *)
type layer = Rules.ruleno * hlpath

fun layerEq ((r1,s1): layer, (r2,s2)) =
    r1 = r2 andalso ListPair.allEq (op =) (s1, s2)

(* DEFN: two hlpaths p1 and p2 are compatible if one is a prefix of the
 * other. So hlpath p = nil is compatible with any other hlpath *)

(* two occurrences of a variable in a pattern must be separated by an "|", so
 * their layer paths must _differ_ at some point, with the path going left (0)
 * being less than the path going right (1). If one hlpath is a prefix of
 * another, they are considered "equal" because they do not diverge.
 * hlpathLT (p1,p2) will return true iff there exists a point n such that
 * 0 <= n <= min(length p1, length p2) and p1(n) = 0 and p2(n) = 1,
 * i.e. p1(n) < p2(n). If p1 <= p2 or p2 <= p1 (prefix order), then 
 * hlpathLT (p1, p2) = false. *)			       
fun hlpathLT (nil, nil) = false
  | hlpathLT (nil, _) = false
  | hlpathLT (_, nil) = false
  | hlpathLT (b1::rest1, b2::rest2) =
    b1 < b2 orelse (b1 = b2 andalso hlpathLT (rest1,rest2))

(* hlpathCompare : hlpath * hlpath -> order
 * If p1 <= p2 or p2 <= p1 (either is a prefix of the other) then
 * hlpathCompare (p1, p2) = EQUAL *)   
fun hlpathCompare (nil, nil) = EQUAL
  | hlpathCompare (nil, p2) = EQUAL
  | hlpathCompare (p1, nil) = EQUAL
  | hlpathCompare (p1, p2) =
    if hlpathLT(p1,p2) then LESS
    else if hlpathLT(p2,p1) then GREATER
    else EQUAL

datatype hlpOrder = EQUALHL | PREFIX1 | PREFIX2 | LEFT | RIGHT

(* hlpathPrefixCompare : hlpath * hlpath -> hlpOrder *)
fun hlpathPrefixCompare (p1,p2) =
    case (p1,p2)
     of (nil, nil) => EQUALHL
      | (x::_, nil) => PREFIX2
      | (nil, x::_) => PREFIX1
      | (b1::rest1, b2::rest2) =>
	(case Int.compare (b1,b2)
	  of LESS => LEFT
	   | EQUAL => hlpathPrefixCompare (rest1, rest2)
	   | GREATER => RIGHT)

fun layerLT ((r1,p1): layer, (r2,p2)) =
    r1 < r2 orelse
    r1 = r2 andalso hlpathLT (p1,p2)

(* layerCompare : layer * layer -> order *)
fun layerCompare ((r1,p1), (r2,p2)) =
    case Int.compare(r1,r2)
     of LESS => LESS
      | GREATER => GREATER
      | EQUAL => hlpathCompare (p1, p2)

fun toRule (r, _) = r
fun fromRule r = (r, nil)
fun extendLeft (r,s) = (r, s@[0])
fun extendRight (r,s) = (r, s@[1])

fun hlpathToString nil = ""
  | hlpathToString [b] = Int.toString b
  | hlpathToString (b::rest) =
    concat [Int.toString b, ".", hlpathToString rest]

fun layerToString (r,s) =
    concat [Int.toString r, ".", hlpathToString s]


(* --------------------------------------------------------------------------- *)
(* loyer sets *)
	   
structure Set =
struct
  type set = layer list (* sorted in "ascending" order *)

  val empty = nil

  (* isEmpty : set -> bool *)
  fun isEmpty set = null set

  (* member: set * layer -> bool *)
  fun member (nil, _) = false
    | member (layer0::rest, layer) =
      (case layerCompare (layer0, layer)
	of EQUAL => true (* rules are equal, hlpaths are "compatible" *)
	 | _ => member (rest, layer))

  (* singleton : layer -> set *)
  fun singleton layer = [layer]  (* == add(empty,layer) *)

  (* add : set * layer -> set *)
  fun add (nil: set, l: layer) = l::nil
    | add (set as ((layer1 as (r1,p1))::rest), layer0 as (r0,p0)) =
      (case Int.compare (r0,r1)
	of LESS => layer0 :: set
	 | GREATER => layer1 :: add(rest, layer0)
	 | EQUAL =>
	   (case hlpathPrefixCompare (p0,p1)
	     of EQUALHL => set
	      | PREFIX1 => set  (* p0 a prefix of p1 *)
	      | PREFIX2 => layer0 :: rest  (* p1 a prefix of p0 *)
	      | LEFT => layer0 :: set
	      | RIGHT => layer1 :: add (rest, layer0)))

  fun add' (layer, set) = add (set, layer)

  (* addList : set * layer list -> set *)
  fun addList (set, layers) =
      foldl add' set layers

  (* union : set * set -> set *)
  fun union (set1, set2) =
      foldr add' set2 set1

  (* intersect : set * set -> set *)
  fun intersect (set0, set1) =
      case (set0, set1)
       of (nil, set2) => nil
        | (set1, nil) => nil
	| ((layer0 as (r0,p0))::rest0, ((layer1 as (r1,p1))::rest1)) =>
	      (case Int.compare(r0,r1)
		of LESS => intersect (rest0, set1)
		 | GREATER => intersect (set0, rest1)
		 | EQUAL =>
		   (case hlpathPrefixCompare (p0,p1)
		     of EQUALHL => layer0 :: intersect (rest0, rest1)
		      | PREFIX1 => layer1 :: intersect (rest0, rest1)
		      | PREFIX2 => layer0 :: intersect (rest0, rest1)
		      | LEFT => intersect (rest0, set1)
		      | RIGHT => intersect (set0, rest1)))

  (* minItem : set -> layer option *)
  fun minItem (nil: set) = NONE
    | minItem (layer::_) = SOME layer

  fun numItems set = length set

  (* isSubset : set * set -> bool *)
  fun isSubset (set1, set2) = false

  fun listItems set = set

  fun layerSetToString set =
      PrintUtil.listToString ("[",",","]") layerToString set

end (* structure Set *)

end (* structure Layers *)

(* Notes:

Terminology: "(horizontally) layered patterns" <== "OR patterns"
   Abbreviated as "layer patterns".

  "hlpath" = node path in a binary tree of horizontal layers,
    (represented by a bit string (int list) where 0 = left, 1 = right)


Defn: two layer patterns are independent if neither is nested in the other,
  i.e. they are AND-cousins.

  Ex1: ((x as true | x), (y as false | y)).
    Here (x as true | x) and (y as false | y) are two independent layer patterns.

Defn: a "layer" is a pair (ruleno, hlpath), designating a match rule
  and an hlpath within that rule.

Claim [?!]: layers associated with independent horizontal layers within a single
  rule pattern will never be "confused", i.e. compared with one another.

Ex1 (cont.): In the above Ex1, the layer (0,1) ambiguously represents the position
  of both x and y (as primary variables) in the pattern.

Ordering:

  hllayers l1 < l2 if either at some index i, l1.i < l2.i or l1 is a prefix of l2.
    (i.e. l1 splits from l2 to the left as some point, or (for linearity!), l2 extneds
    l1, so l2 is "below" l1 in the layer heirarchy.)
*)
