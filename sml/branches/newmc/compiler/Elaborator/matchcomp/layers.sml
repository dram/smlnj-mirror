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
type layer = ruleno * hlpath

fun layerEq ((r1,s1): layer, (r2,s2)) =
    r1 = r2 andalso ListPair.allEq (op =) (s1, s2)

(* two occurrences of a variable in a pattern must be separated by an "|", so
 * their layer paths must _differ_ at some point, with the path going left (0)
 * being less than the path going right. If one hlpath is a prefix of
 * another, they are considered "equal" because they do not diverge. *)
fun hlpathLT (nil, nil) = false
  | hlpathLT (nil, _) = false
  | hlpathLT (_, nil) = false
  | hlpathLT (b1::rest1, b2::rest2) =
    b1 < b2 orelse (b1 = b2 andalso hlpathLT (rest1,rest2))

fun hlpathCompare (p1, p2) =
    if hlpathLT(p1,p2) then LESS
    else if hlpath(p2,p1) then GREATER
    else EQUAL

fun layerLT ((r1,p1): layer, (r2,p2)) =
    r1 < r2 orelse
    r1 = r2 andalso hlpathLT (p1,p2)

fun layerCompare ((r1,p1), (r2,p2)) =
    case Int.compare(r1,r2)
     of LESS => LESS
      | GREATER => GREATER
      | EQUAL => hlpathCompare (p1, p2)

fun ruleToLayer r = (r, nil)
fun newLayerLeft (r,s) = (r, s@[0])
fun newLayerRight (r,s) = (r, s@[1])

structure OrdKey =
struct
  type key = layer
  val compare = layerCompare
end

structure Set = RedBlackSetFn (OrdKey)

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
