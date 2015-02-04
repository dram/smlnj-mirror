(* intset.sml *)

structure Intset : sig type intset
		        val new : unit -> intset
		        val add : intset -> int -> unit
			val rem : intset -> int -> unit
			val mem : intset -> int -> bool
		   end =
struct
  val p = 211
  type intset = int list array
  fun new () = array(p,nil : int list)
  fun add a i = let val index = i mod p in update(a,index,i::(a sub index)) end
  fun mem a i = exists((fn j => j=i), a sub (i mod p))
  fun rem a i = let fun f (j::r) = if i=j then f r else j :: f r
	              | f nil = nil
		    val index = i mod p
		 in update(a,index, f(a sub index))
		end
end

