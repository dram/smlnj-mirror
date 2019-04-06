signature SORTED_SET_ITEM =
sig
  type t
  type k
  val key: t -> k
  val lt : k * k -> bool
end

signature SORTED_SET =
sig
  type s
  type t
  type k
  exception NotFound
  exception DuplicateKey
  val new: unit -> s
  val insert: s * t -> s
  val delete: s * k -> s
  val find: s * k -> t (* exact match *)
  val findp: s * k -> t (* pred match *)
  val finds: s * k -> t (* succ match *)
  val update: s * t -> s (* must already exist *)
  val iterate: s * (t->'a) -> unit (* in "list app" order *)
  val fold: s * ((t * 'a) -> 'a) * 'a -> 'a    (* in "list fold" order *)
  val revfold: s * ((t * 'a) -> 'a) * 'a -> 'a (* in "list revfold" order *)
  val size: s -> int
end

functor SortedSet (I:SORTED_SET_ITEM) : SORTED_SET =
(* this is the dumbest possible implementation *)
struct
  type s = I.t list
  type t = I.t
  type k = I.k
  exception NotFound
  exception DuplicateKey
  fun new () = nil
  fun insert (s,n) = 
    let fun f nil = [n]
          | f (l as h::t) = if I.lt(I.key n,I.key h) then 
				n::l 
			    else if I.lt(I.key h,I.key n) then
				h::f t
			    else raise DuplicateKey
    in f s
    end
  fun find (s,k) =
    let fun f nil = raise NotFound
          | f (l as h::t) = if I.lt(k, I.key h) then raise NotFound
			    else if I.lt(I.key h, k) then f t
			    else h
    in f s
    end
  fun findp (s,k) =  (* returns item or its predecessor *)
    let fun f (nil,last) = last()
          | f (l as h::t, last) = if I.lt(I.key h, k) then
				    f (t, fn x => h)
				  else if I.lt(k, I.key h) then 
				    last()
				  else h
    in f(s, fn x => raise NotFound)
    end
  fun finds (s,k) = (* returns item or its successor *)
    let fun f (nil) = raise NotFound
          | f (l as h::t) = if I.lt(I.key h, k) then f t
			    else h
    in f s
    end
  fun delete (s,k) = 
    let fun f nil = raise NotFound
          | f (h::t) = if I.lt(k, I.key h) then raise NotFound
		       else if I.lt (I.key h, k) then h::f t
		       else t
    in f s
    end
  fun update(s,n) =
    let fun f nil = raise NotFound
          | f (h::t) = if I.lt(I.key n, I.key h) then raise NotFound
		       else if I.lt(I.key h, I.key n) then h::f t
		       else n::t
    in f s
    end
  fun iterate (s,f) = app f s
  fun fold (s,f,i) = List.fold f s i
  fun revfold (s,f,i) = List.revfold f s i
  fun size s = List.length s
end
  
