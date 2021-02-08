(* matchcomp/layers.sml *)



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
