(* variant-maps.sml *)

structure Variants =
struct

local
  function bug msg = ErrorMsg.impossible ("Variants: "^msg)
  open Key
in

structure IntInfOrdKey =
struct
  type ord_key = IntInf.int
  val compare = IntInf.compare
end

structure InfInfMap = RedBlackMapFn(IntInfOrdKey)
				   
structure StringOrdKey =
struct
  type ord_key = string
  val compare = String.compare
end

structure StringMap = RedBlackMapFn(StringOrdKey)

datatype 'a variants
  = MapII of 'a IntInfMap.map
  | MapI of 'a IntRedBlackMap.map
  | MapS of 'a StringMap.map

fun keyToInf key =
    (case key
      of C c => IntInf.fromInt (Char.ord c)
       | D (dcon, _) => IntInf.fromInt (dataconIndex dcon)
       | V sz => IntInf.fromInt sz
       | _ => bug "keyToInt" )

fun keyToInfInf key =
    (case key
      of (I {ival,...} | W {ival,...}) => ival
       | _ => bug "keyToInfInt" )

(* empty : Key.key -> 'a variants *)
fun empty (I _ | W _) = MapII IntInfMap.empty
  | empty (C _ | D _ | V _) = MapI IntRedBlackMap.empty
  | empty (S _) = MapS StringMap.empty
  | empty _ = bug "empty"

(* find : 'a variants * Key.key -> 'a option *)
fun find (MapII m, key) = IntInfMap.find (m, keyToInfInt key)
  | find (MapI m, key) = IntRedBlackMap.find (m, keyToInt key)
  | find (MapS m, S s) = StringMap.find(m, s)

(* insert : 'a variants * Key.key * 'a -> 'a variants *)
fun insert (MapII m, key, x) = IntInfMap.insert (m, keyToInfInt key, x)
  | insert (MapI m, key, x) = IntRedBlackMap.insert (m, keyToInt key, x)
  | insert (MapS m, S s, x) = StringMap.insert (m, s, x)

(* map : ('a -> 'b) -> 'a variants -> 'b variants *)
fun map f (MapII m) = IntInfMap.map f m
  | map f (MapI m) = IntRedBlackMap.map f m
  | map f (MapS m) = StringMap.map f m

fun exists f (MapII m) = IntInfMap.exists f m
  | exists f (MapI m) = IntRedBlackMap.exists f m
  | exists f (MapS m) = StringMap.exists f m

(* ... etc. *)

end (* local *)

end (* structure Variants *)
    
