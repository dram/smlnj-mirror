(* intmap.sml *)

structure Intmap : sig type 'a intmap
			exception Intmap
		        val new : unit -> 'a intmap
		        val add : 'a intmap -> int*'a -> unit
			val rem : 'a intmap -> int -> unit
			val map : 'a intmap -> int -> 'a
			val app : (int*'a -> unit) -> 'a intmap -> unit
		   end =
struct
  val p = 211
  type 'a intmap = (int*'a) list array
  exception Intmap
  fun new () = array(p,nil) : 'a intmap
  fun add a (i,j) = 
    let val index = i mod p in update(a,index,(i,j)::(a sub index)) end
  fun map a i = 
    let fun find ((i',j)::r) = if i=i' then j else find r
	  | find nil = raise Intmap
     in find (a sub (i mod p))
    end
  fun rem a i = let fun f ((b as (i',j))::r) = 
				if i=i' then f r else b :: f r
	              | f nil = nil
		    val index = i mod p
		 in update(a,index, f(a sub index))
		end
  fun app (f: int*'a -> unit) a =
      let fun zap 0 = ()
	    | zap n = let val m = n-1 in List.app f (a sub m); zap m end
      in  zap p
      end
end

