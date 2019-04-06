(* intmap.sml *)
structure Intmap : sig type 'a intmap
		       val new : exn -> '1a intmap
		       val add : '2a intmap -> int*'2a -> unit
		       val rem : 'a intmap -> int -> unit
		       val map : 'a intmap -> int -> 'a
		       val app : (int*'a -> unit) -> 'a intmap -> unit
		   end =
struct
  datatype 'a intmap = Hashed of {size : int ref, table : (int *'a) list array ref, elems : int ref, primes: int list ref, exn : exn}
  val primes = [97,199,397,797,1597,3191,6397,12799,25013,50021,
		100003,200003,400009,800011,1600033,3200003,6400013,
		12800009,25000009,50000017,100000007,200000033,500000003];
  fun new exn = Hashed{size=ref 37,table=ref(array(37,nil)),elems=ref 0,
			primes= ref primes, exn = exn}
  fun map m i =
  (fn(Hashed{size,table=ref a,exn,...},i) =>
    let fun find ((i',j)::r) = if i=i' then j else find r
	  | find nil = raise exn
    in  find (a sub (i mod !size))
    end) (m,i)
  fun rem m i =
  (fn(Hashed{size,table=ref a,elems,...},i) =>
    let fun f ((b as (i',j))::r) = if i=i' then (dec elems; f r) else b :: f r
	  | f nil = nil
	val index = i mod !size
    in  update(a,index, f(a sub index))
    end) (m,i)
  fun app f m =
  (fn(f,Hashed{size,table=ref a,...}) =>
      let fun zap 0 = ()
	    | zap n = let val m = n-1 in List.app f (a sub m); zap m end
      in  zap(!size)
      end) (f,m)
  fun revapp f m =
  (fn(f,Hashed{size,table=ref a,...}) =>
      let fun zap 0 = ()
	    | zap n = let val m = n-1 in List.revapp f (a sub m); zap m end
      in  zap(!size)
      end) (f,m)
  fun add m v =
  (fn(Hashed{size,table=table as ref a,elems,primes,...},(i,j)) =>
	let val index = i mod !size
	in  update(a,index,v::(a sub index));
	    inc elems;
	    if !elems <> !size then ()
	    else
		(let val size2::p' = !primes
		    val new = array(size2,nil)
		    fun add'(v as (i,j)) =
		      let val index = i mod size2
		      in  update(new,index,v::(new sub index))
		      end
		in
		    revapp add' m; table := new; size := size2; primes := p'
		end
		handle Bind => ErrorMsg.impossible "intmap too big")
	end)(m,v)
end
