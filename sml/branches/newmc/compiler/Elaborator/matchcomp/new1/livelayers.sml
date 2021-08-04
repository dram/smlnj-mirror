(* livelayers.sml *)

(* combine management of direct and defaults variable bindings at nodes. *)

structure LiveLayers =
struct

local
    structure L = Layers
    structure LS = L.Set
in

type liveLayers = {directs: LS.set, defaults: LS.set}

(* empty : liveLayers *)
val empty = {directs = LS.empty, defaults = LS.empty}

(* newDirect : L.layer -> liveLayers *)
fun newDirect l = {directs = LS.singleton l, defaults = LS.empty}

(* addDirect : L.layer * liveLayers -> liveLayers *)
fun addDirect (layer, {directs, defaults}: liveLayers) =
    {directs = LS.add(directs, layer), defaults = defaults}

(* addDefaults : LS.set * liveLayers -> liveLayers *)
fun addDefaults (layers: LS.set, {directs, defaults}: liveLayers) =
    {directs = directs, defaults = LS.union (defaults, layers)}

(* all : liveLayers -> LS.set *)
fun all {directs, defaults} = LS.union (directs, defaults)

(* directs : liveLayers -> LS.set *)
fun directs ({directs, ...}: liveLayers) = directs

(* defaults : liveLayers -> LS.set *)
fun defaults ({defaults, ...}: liveLayers) = defaults

end (* local *)
end (* structure LiveLayers *)
