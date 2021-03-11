(* OR-info.sml *)

(* Information on all the OR nodes in an AndOr tree used for building the decision tree.
 * We don't really need to build the ONH intermediate structure, but it may be useful
 * for analysis and debugging. *)

structure ORinfo =
struct

local 
  structure Map = IntRedBlackMap
  structure MT = MCTypes

  fun bug msg = ErrorMsg.impossible ("ORinfo: " ^ msg)
in

(* OR-node Hierarchy tree *)
datatype ONH = ONH of int * ONH list

(* map from OR-node ids to the weight of the node *)
type weights = int Map.map

(* calcWeights1 : ONH -> weights *)
fun calcWeights1 (onh, map0) =
    let fun mapThis (ONH(id, nil), map) = (1, Map.insert(map, id, 1))
	  | mapThis (ONH(id, children), map) =
	    let val (w_children, map_children) =
		    foldl (fn (child,(w,map)) =>
			      let val (w',map') = mapThis (child, map)
			      in (w + w', map')
			      end)
			  (0,map)
			  children
		val weight = w_children + 1
	     in (weight, Map.insert(map_children, id, weight))
	    end
     in #2 (mapThis (onh, map0))
    end

(* calcWeights : ONH list -> weights *)
fun calcWeights (onhs: ONH list) = 
    foldl calcWeights1 Map.empty onhs

(* accessibleORNodes : andor -> andor list *)
(* ASSERT: nodes in result (if any) are OR nodes *)
fun accessibleORNodes andor =
    (case andor
       of MT.AND{children,...} => accessibleORNodeList children
	| MT.OR _ => [andor]       (* non-degenerate OR node is opaque *)
	| MT.SINGLE{variant = (_,arg),...} => accessibleORNodes arg
	    (* SINGLE nodes are "transparent" *)
	| _ => [])  (* VARS, LEAF, INITIAL *)

(* accessibleList : andor list -> andor list *)
and accessibleORNodeList andors =
    List.concat (map accessibleORNodes andors)

(* andorToONH : andor -> onh *)
(* REQUIRES: is_OR andor *)
fun andorToONH (MT.OR{info={id,...}, variants,...}) =
    let val variantAndors = map #2 (Variants.listItems' variants)
	val subORs = List.concat (map accessibleORNodes variantAndors)
	val subONHs = map andorToONH subORs
     in ONH (id, subONHs)
    end
  | andorToONH _ = bug "andorToONH, OR-node required"

(* andorToONHs : andor -> onh list *)
fun andorToONHs andor =
    map andorToONH (accessibleORNodes andor)

(* id : 'a -> 'a *)
fun id x = x
	       
(* sum : ('a -> int) -> 'a list -> int *)
fun sum f xs = foldl (fn (x,acc) => f x + acc) 0 xs

(* ORnodeCount : ONH -> int *)
fun ORnodeCount (ONH (_, onhs)) =
    1 + sum ORnodeCount onhs

val totalORNodes : int ref = ref 0
val topONHs : ONH list ref = ref nil
val weights : weights ref = ref Map.empty
val useCounts : int Map.map ref = ref Map.empty

fun reset () =
    (totalORNodes := 0;
     topONHs := nil;
     weights := Map.empty;
     useCounts := Map.empty)

(* initInfo : MT.andor -> unit *)
fun initInfo andor =
    (* overwrites any previous info, so no need to reset *)
    (topONHs := andorToONHs andor;
     totalORNodes := sum id (map ORnodeCount (!topONHs));
     weights := calcWeights (!topONHs))

(* getWeight :  int -> int option *)
fun getWeight (id: int) = Map.find (!weights, id)
    
(* getWeights : unit -> (int * int) list *)
fun getWeights () =
    Map.listItemsi (!weights)

fun incrementUse id =
    useCounts := Map.insertWith (op +) (!useCounts, id, 1)

(* getUseCount : int -> int option *)
fun getUseCount id = Map.find (!useCounts, id)

fun getTotalUses () =
    sum id (Map.listItems (!useCounts))

fun maxUseCount () =
    let fun comp (id1, count1, (id2, count2)) =
	    case Int.compare(count1,count2)
	     of (LESS | EQUAL) => (id2, count2)
	      | GREATER => (id1, count1)
     in Map.foldli comp (~1,0) (!useCounts)
    end

end (* local *)
end (* structure ORinfo *)

(* DISCARDS:

(* accessible : andor -> int list *)
fun accessible andor =
    (case andor
       of AND{children,...} => accessibleList children
	| OR _ => [getId andor]       (* non-degenerate OR node is opaque *)
	| SINGLE{variant = (_,arg),...} => accessible arg
	    (* SINGLE nodes are "transparent" *)
	| _ => [])  (* VARS, LEAF, INITIAL *)

(* accessibleList : andor list -> int list *)
and accessibleList andors =
    foldl (fn (andor,ids) => accessible andor @ ids) nil andors

*)
