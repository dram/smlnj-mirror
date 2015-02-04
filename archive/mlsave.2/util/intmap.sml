(* intset.sml *)

structure intmap : sig type 'a intmap
			exceptionx intmap : unit
		        val new : unit -> 'a intmap
		        val add : 'a intmap -> int*'a -> unit
			val rem : 'a intmap -> int -> unit
			val map : 'a intmap -> int -> 'a
		   end =
struct
  val P = 211
  type 'a intmap = (int*'a) list array
  exceptionx intmap : unit
  fun new () = array(211,nil) : 'a intmap
  fun add a (i,j) = 
    let val index = i mod P in update(a,index,(i,j)::(a sub index)) end
  fun map a i = 
    let fun find ((i',j)::r) = if i=i' then j else find r
	  | find nil = raisex intmap
     in find (a sub (i mod P))
    end
  fun rem a i = let fun f ((b as (i',j))::r) = 
				if i=i' then f r else b :: f r
	              | f nil = nil
		    val index = i mod P
		 in update(a,index, f(a sub index))
		end
end

