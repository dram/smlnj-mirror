signature UNIONFIND =
  sig
    exception Union
    val new : (int -> bool) ->
	      {union: int * int -> int,
	       find : int -> int}
  end


structure Unionfind : UNIONFIND =
  struct
    exception Union
    fun new (fixed) =
	let open Intmap (* locally rebinding new, of course! *)
	    val m = new()
	    and n = new()
	    fun find x = 
		 let val z = find(map m x)
		  in add m (x,z); z
		 end 
		 handle Intmap.Intmap => x
	    fun union (x,y) =
		let val x' = find x and y' = find y
		    val xn = map n x'  handle Intmap.Intmap => 1
		    and yn = map n y'  handle Intmap.Intmap => 1
		 in if x' <> y'
		    then if fixed(x')
			 then if fixed(y')
			      then raise Union
			      else (add m (y', x'); add n (x',xn+yn); x')
			 else if fixed(y')
			      then (add m (x', y'); add n (y',xn+yn); y')
			      else if xn < yn			      
			      then (add m (x', y'); add n (y',xn+yn); y')
			      else (add m (y', x'); add n (x',xn+yn); x')
		    else x'
		end
	 in {union=union, find=find}
	end
  end


signature SIBLINGS =
  sig
    val new : (int -> bool) ->
	      {assoc : int * 't -> unit,  (* 't should be weak *)
	       union : int * int -> int,
               find : int -> int,
	       getassoc : int -> 't list}
     (* assoc(i,x) must be called for any element i before 
        i is used as an argument to union or find or getassoc *)
  end


structure Siblings : SIBLINGS =
  struct
    fun new(fixed) =
	let val {union = uni, find = find} = Unionfind.new(fixed)
	    val a = Intmap.new()
	    val add = Intmap.add a
	    val map = Intmap.map a
	    fun assoc (i,x) = 
		let val (_,l) = map i handle Intmap.Intmap => (x,nil)
		 in add (i,(x,l))
		end
	    fun include(i,j) =
	      let val (x,l) = map j
	       in add (j,(x,i::l)); j
	      end
	    fun union (i,j) = 
		let val i' = find i and j' = find j
		 in if i' = j' then i'
		    else let val k = uni(i',j')
			  in if k=i' then include(j',k) else include(i',k)
			 end
		end
	    fun get(i,l) = 
	       let fun f (a::b) = get(a,f(b)) | f nil = l
		   val (x,r) = map i
		in x::f(r)
	       end
	    fun getassoc i = get(find i,nil)
	 in {assoc=assoc, union=union, find=find, getassoc=getassoc}
	end
  end (* structure Siblings *)

