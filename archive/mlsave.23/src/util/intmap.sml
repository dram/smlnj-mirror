(* intmap.sml *)
structure Intmap : sig type 'a intmap
		       val new : exn -> '1a intmap
		       val add : '2a intmap -> int*'2a -> unit
		       val rem : 'a intmap -> int -> unit
		       val map : 'a intmap -> int -> 'a
		       val app : (int*'a -> unit) -> 'a intmap -> unit
		   end =
struct
  datatype 'a intmap = Hashed of {size : int ref, table : (int *'a) list array ref,
				  elems : int ref, primes: int list ref, exn : exn}
  val primes = [97,199,397,797,1597,3191,6397,12799,25013,50021,100003,200003,
	        400009,800011,1600033,3200003,6400013,12800009,25000009,50000017,
		100000007,200000033,500000003];
  fun new exn = Hashed{size=ref 37,table=ref(array(37,nil)),elems=ref 0,
		       primes= ref primes, exn = exn}
  fun map (Hashed{size,table=ref a,exn,...}) i =
      let fun find ((i',j)::r) = if i=i' then j else find r
            | find nil = raise exn
      in  find (a sub (i mod !size))
      end
  fun rem (Hashed{size,table=ref a,elems,...}) i =
      let fun f ((b as (i',j))::r) = if i=i' then (dec elems; r) else b :: f r
	    | f nil = nil
	  val index = i mod !size
      in update(a, index, f(a sub index))
      end
  fun app f (Hashed{size,table=ref a,...}) =
      let fun zap 0 = ()
	    | zap n = let val m = n-1 in List.app f (a sub m); zap m end
      in  zap(!size)
      end
  fun add (m as Hashed{size,table=table as ref a,elems,primes,...}) (v as (i,j)) =
      if !elems <> !size
      then let val index = i mod !size
	       fun f nil = (inc elems; [v])
	         | f ((a as (i',j))::b) = if i=i' then v::b else a::f b
	   in update(a,index,f(a sub index))
	   end
      else (let val newsize::newprimes = !primes
		val new = array(newsize,nil)
		fun add'(v as (i,j)) =
		    let val index = i mod newsize
		    in  update(new,index,v::(new sub index))
		    end
	    in app add' m; table := new; size := newsize; 
	    primes := newprimes; add m v
	    end
	    handle Bind => ErrorMsg.impossible "intmap too big")
end
