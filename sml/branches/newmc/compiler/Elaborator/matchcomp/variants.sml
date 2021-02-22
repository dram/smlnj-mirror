(* matchcomp/variants.sml *)

structure Variants =
struct

local
  structure TU = TypesUtil
  fun bug msg = ErrorMsg.impossible ("Variants: "^msg)
  open Key
in

structure IntInfOrdKey =
struct
  type ord_key = IntInf.int
  val compare = IntInf.compare
end

structure IntInfMap = RedBlackMapFn(IntInfOrdKey)
				   
structure StringOrdKey =
struct
  type ord_key = string
  val compare = String.compare
end

structure StringMap = RedBlackMapFn(StringOrdKey)

(* we include the key in the map entry for printing purposes (in mcprint.sml) *)
datatype 'a variants
  = MapII of (Key.key * 'a) IntInfMap.map
  | MapI of (Key.key * 'a) IntRedBlackMap.map
  | MapS of (Key.key * 'a) StringMap.map

fun keyToInt key =
    (case key
      of C c => Char.ord c
       | V sz => sz
       | _ => bug "keyToInt" )

fun keyToIntInf key =
    (case key
      of (I {ival,...} | W {ival,...}) => ival
       | _ => bug "keyToIntInf" )

fun keyToString (S s) = s
  | keyToString (D (dcon,_)) = Symbol.name (TU.dataconName dcon)
  | keyToString _ = bug "keyToString"

(* empty : Key.key -> 'a variants *)
fun empty (I _ | W _) = MapII IntInfMap.empty
  | empty (C _ | V _) = MapI IntRedBlackMap.empty
  | empty (D _ | S _) = MapS StringMap.empty
  | empty _ = bug "empty"

(* find' : 'a variants * Key.key -> (Key.key * 'a) option *)
fun find' (variants, key) =
    (case variants
       of MapII m => IntInfMap.find (m, keyToIntInf key)
	| MapI m => IntRedBlackMap.find (m, keyToInt key)
        | MapS m => StringMap.find(m, keyToString key))

(* find : 'a variants * Key.key -> 'a option *)
fun find (variants, key) = Option.map #2 (find' (variants, key))

(* insert : 'a variants * Key.key * 'a -> 'a variants *)
fun insert (MapII m, key, x) = MapII (IntInfMap.insert (m, keyToIntInf key, (key, x)))
  | insert (MapI m, key, x) = MapI (IntRedBlackMap.insert (m, keyToInt key, (key, x)))
  | insert (MapS m, key, x) =
    let val s =
	    case key
	      of D (dcon,_) => Symbol.name(TU.dataconName dcon)
	       | S s => s
     in MapS (StringMap.insert (m, s, (key, x)))
    end

(* map : ('a -> 'b) -> 'a variants -> 'b variants *)
fun map f variants =
    let fun f' (k,x) = (k, f x)
     in case variants
	  of MapII m => MapII (IntInfMap.map f' m)
           | MapI m => MapI (IntRedBlackMap.map f' m)
           | MapS m => MapS (StringMap.map f' m)
    end

(* app' : ((Key.key * 's) -> unit) -> 'a variants -> unit *)
fun app' f variants =
    (case variants
      of MapII m => IntInfMap.app f m
       | MapI m => IntRedBlackMap.app f m
       | MapS m => StringMap.app f m)

(* exists : ('a -> bool) -> 'a variants -> bool *)
fun exists f variants =
    let fun f' (k,x) = f x	   
     in case variants
	  of MapII m => IntInfMap.exists f' m
           | MapI m => IntRedBlackMap.exists f' m
           | MapS m => StringMap.exists f' m
    end

(* numItems : 'a variants -> int *)
fun numItems (MapII m) = IntInfMap.numItems m
  | numItems (MapI m) = IntRedBlackMap.numItems m
  | numItems (MapS m) = StringMap.numItems m

(* listItems' : 'a variants -> (Key.key * 'a) list *)
fun listItems' variants =
    (case variants
      of MapII m => IntInfMap.listItems m
       | MapI m => IntRedBlackMap.listItems m
       | MapS m => StringMap.listItems m)


(* ... etc. *)

end (* local *)

end (* structure Variants *)
    
