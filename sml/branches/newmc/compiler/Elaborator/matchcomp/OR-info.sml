(* or-info.sml *)

(* Information on all the OR nodes in an AndOr tree used for building the decision tree *)

structure Map = IntRedBlackMap

val numOrNodes = ref (0)

(* OR-node Hierarchy tree *)
type onh = int * onh list

(* map from OR-node ids to the weight of the node *)
type weights = int IntRedBlackMap.map

(* calcWeights : onh -> weights *)
fun calcWeights (ohn) =
    let fun mapThis ((id, children), map) =
	    let val (w, map') = mapChildren (children, map)
	    in Map.insert(map', id, w)
	    end
	and mapChildren (nil, map) = map
	  | mapChildren (ohn::rest,map) =
	    mapChildren (rest, mapThis (ohn, map))
    in mapThis (ohn, Map.empty)
    end
