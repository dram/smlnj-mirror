(* ORinfo.sml *)

(* Collect information on all the OR nodes in an AndOr tree used for
 * determining priorities of OR nodes that are used when building the decision tree.
 *)

structure ORinfo =
struct

local 
  structure Map = IntRedBlackMap
  structure LS = Layers.Set
  structure MT = MCTypes

  fun bug msg = ErrorMsg.impossible ("ORinfo: " ^ msg)

  (* identity : 'a -> 'a *)
  fun identity x = x
	       
  (* sum : ('a -> int) -> 'a list -> int *)
  fun sum f xs = foldl (fn (x,acc) => f x + acc) 0 xs

in

(* --------------------------------------------------------------------------- *)
(* OR-node hierarchy and weight *)

(* We don't really need to build the ONH intermediate structure, but it may be useful
 * for analysis and debugging. *)

(* OR-node Hierarchy tree *)
datatype ONH = ONH of int * ONH list

(* map from OR-node ids to the weight of the node *)
type weightsMap = int Map.map

(* calcWeights : ONH -> weightsMap *)
fun calcWeights (onh: ONH, mapIn: weightsMap) =
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
	val (_, mapOut) = mapThis (onh, mapIn)
     in mapOut
    end

(* mkWeightsMap : ONH list -> weightsMap *)
(* applied to the list of top-level, independent ONHs from an andor tree *)
fun mkWeightsMap (onhs: ONH list) = 
    foldl calcWeights Map.empty onhs

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

(* ORnodeCount : ONH -> int *)
fun ORnodeCount (ONH (_, onhs)) =
    1 + sum ORnodeCount onhs

val totalORNodes : int ref = ref 0
val topONHs : ONH list ref = ref nil
val weightsMap : weightsMap ref = ref Map.empty

(* getWeight :  int -> int *)
fun getWeight (id: int) =
    case Map.find (!weightsMap, id)
     of SOME w => w
      | NONE => 0
		    
(* getWeights : unit -> (int * int) list *)
fun getWeights () =
    Map.listItemsi (!weightsMap)

fun initWeights andor =				
    (topONHs := andorToONHs andor;
     totalORNodes := sum identity (map ORnodeCount (!topONHs));
     weightsMap := mkWeightsMap (!topONHs))


(* --------------------------------------------------------------------------- *)
(* variable relevance: binding layers for andor nodes *)

(* map andor node id to set of layers such that a variable is bound
 * at that layer in some node at or below the given node. Represented
 * by a layerset IntRedBlackMap. 
 *
 * Could refine to collect both layers _and_ the variables bound for each
 * node (e.g. MCTypes.varBindings). This would support a refined notion of
 * binding relevance. Note:
 *    layer -> var list
 *    layer -> r:rule -> pvars(r) 
 * These should agree.
 *)

(* bindingLayers : andor -> LS.set *)
(* fails if andor does not have an info (i.e. LEAF, INITIAL) *)
fun bindingLayers andor =
    let val {vars,asvars,...} = MT.getInfo andor
     in LS.addList (LS.empty, map #2 (vars @ asvars))
    end

(* mkBindingLayersMap : andor * LS.set Map.map -> LS.set * LS.set Map.map *)
fun mkBindingLayersMap (andor,map0) =
    let fun addAndor (andor, (layerset, map1)) =
	    let val (andorLayerset, newmap) = mkBindingLayersMap(andor, map1)
		val newLayers = LS.union (andorLayerset,layerset)
	    in (newLayers, newmap)
	    end
     in case andor
	 of MT.AND{info={id,...},children,...} =>
	      let val localLayers = bindingLayers andor
		  val (childrenLayers,childrenMap) =
		      foldl addAndor (localLayers,map0) children
		  val allLayers = LS.union (localLayers, childrenLayers)
	       in (allLayers, Map.insert (childrenMap, id, allLayers))
	      end
	  | MT.OR{info={id,...},variants,...} => 
	      let val localLayers = bindingLayers andor
		  val (variantsLayers, variantsMap) =
		      foldl addAndor (localLayers,map0) (Variants.listItems variants)
		  val allLayers = LS.union (localLayers, variantsLayers)
	       in (allLayers, Map.insert (variantsMap, id, allLayers))
	      end
	  | MT.SINGLE {info={id,...},variant} =>
	      let val localLayers = bindingLayers andor
		  val (variantLayers, variantMap) = mkBindingLayersMap (#2 variant, map0)
		  val allLayers = LS.union (localLayers, variantLayers)
	       in (allLayers, Map.insert (variantMap, id, allLayers))
	      end
	  | MT.VARS{info={id,...},...} => 
	      let val localLayers = bindingLayers andor
	       in (localLayers, Map.insert(map0, id, localLayers))
	      end
	  | MT.LEAF _ => (LS.empty, map0)
	  | MT.INITIAL => bug "mkBindingLayersMap: INITIAL"
    end

val bindingLayersMap : LS.set Map.map ref = ref Map.empty

(* initBindingLayersMap : andor -> unit *)
fun initBindingLayersMap andor =
    let val (_,map) = mkBindingLayersMap (andor, Map.empty)
     in bindingLayersMap := map
    end

(* getVarLayers : int -> LS.set *)
(* int is node id *)
fun getBindingLayers id =
    case Map.find (!bindingLayersMap, id)
     of SOME layerset => layerset
      | NONE => LS.empty

fun getNumBindingLayers id =
    LS.numItems (getBindingLayers id)


(* --------------------------------------------------------------------------- *)
(* OR-node use counts *)

val useCounts : int Map.map ref = ref Map.empty

fun incrementUse id =
    useCounts := Map.insertWith (op +) (!useCounts, id, 1)

(* getUseCount : int -> int option *)
fun getUseCount id = Map.find (!useCounts, id)

fun getTotalUses () =
    sum identity (Map.listItems (!useCounts))

fun maxUseCount () =
    let fun comp (id1, count1, (id2, count2)) =
	    case Int.compare(count1,count2)
	     of (LESS | EQUAL) => (id2, count2)
	      | GREATER => (id1, count1)
     in Map.foldli comp (~1,0) (!useCounts)
    end

fun initUseCounts () =
    useCounts := Map.empty


(* --------------------------------------------------------------------------- *)
(* initializating and reseting state *)

fun reset () =
    (totalORNodes := 0;
     topONHs := nil;
     weightsMap := Map.empty;
     bindingLayersMap := Map.empty;
     useCounts := Map.empty)

(* initInfo : MT.andor -> unit *)
fun initInfo andor =
    (* overwrites any previous info, so no need to reset *)
    (initWeights andor;
     initBindingLayersMap andor;
     initUseCounts ())

end (* local *)
end (* structure ORinfo *)

(* DISCARDED CODE:

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
