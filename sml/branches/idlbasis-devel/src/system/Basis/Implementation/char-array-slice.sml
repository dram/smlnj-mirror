(* word8-array-slice-sig.sml
 *
 * COPYRIGHT (c) 2000 Bell Labs, Lucent Technologies.
 *
 *)

structure CharArraySlice: MONO_ARRAY_SLICE =
struct

  structure A = InlineT.CharArray
  type elem = Char.char
  type array = CharArray.array
  type vector = CharVector.vector
  type vector_slice = CharVectorSlice.slice
  datatype slice = SL of {base: array, start: int, stop: int}

  val (op <)  = InlineT.DfltInt.<
  val (op >=) = InlineT.DfltInt.>=
  val (op +)  = InlineT.DfltInt.+
  val sub' = A.sub
  val update' = A.update
  val geu = InlineT.DfltInt.geu

(* val full : array -> slice *)
(* val slice : array * int * int option -> slice *)
(* val subslice : slice * int * int option -> slice *)
(* val base : slice -> array * int * int *)
(* val vector : slice -> vector *)
(* val copy    : {src : slice, dst : array, di : int} -> unit *)
(* val copyVec : {src : vector_slice, dst : array, di : int} -> unit *)
(* val isEmpty : slice -> bool *)
(* val getItem : slice -> (elem * slice) option *)
(* val appi : (int * elem -> unit) -> slice -> unit *)
(* val app  : (elem -> unit) -> slice -> unit *)
(* val modifyi : (int * elem -> elem) -> slice -> unit *)
(* val modify  : (elem -> elem) -> slice -> unit *)
(* val foldli : (int * elem * 'b -> 'b) -> 'b -> slice -> 'b *)
(* val foldr  : (elem * 'b -> 'b) -> 'b -> slice -> 'b *)
(* val foldl  : (elem * 'b -> 'b) -> 'b -> slice -> 'b *)
(* val foldri : (int * elem * 'b -> 'b) -> 'b -> slice -> 'b *)
(* val findi : (int * elem -> bool) -> slice -> (int * elem) option *)
(* val find  : (elem -> bool) -> slice -> elem option *)
(* val exists : (elem -> bool) -> slice -> bool *)
(* val all : (elem -> bool) -> slice -> bool *)
(* val collate : (elem * elem -> order) -> slice * slice -> order *)
    
(*----------------------------------------------------------------------*)
(* val length : slice -> int *)
  fun length(SL{start,stop,...}) = stop - start

(* val sub : slice * int -> elem *)
(* sub(s,j) valid if 0<=j<stop-start, otherwise raises Subscript *)
  fun sub (SL{base, start, stop}, j) =
      let val j' = start+j
       in if geu(j', stop)  (* checks for j' >= 0 *)
          then raise Core.Subscript
          else sub'(base, j')
      end

(* val update : slice * int * elem -> unit *)
  fun update (SL{base,start,stop},j,x) =
      let val j' = start + j
      in if geu (j', stop) then raise Core.Subscript
	 else update' (base, j', x)
      end

(* val full : array -> slice *)
  fun full base = SL{base=base,start=0,stop=A.length base}
(*
      let val blen = V.length base
       in if geu(start, blen)  (* checks start >= 0 *)
          then raise Core.Subscript
	  else case lenOp
		 of NONE => SL{base=base,start=start,stop=blen}
		  | SOME n => 
		      if geu(n, blen-start) (* checks n >= 0 *)
		      then raise Core.Subscript
		      else SL{base=base,start=start,stop=start+n}
      end
*)

(* val slice : vector * int * int option -> slice *)
  fun slice (base,start,lenOp) =
      let val blen = A.length base
       in if geu(start, blen)  (* checks start >= 0 *)
          then raise Core.Subscript
	  else case lenOp
		 of NONE => SL{base=base,start=start,stop=blen}
		  | SOME n => 
		      if geu(n, blen-start) (* checks n >= 0 *)
		      then raise Core.Subscript
		      else SL{base=base,start=start,stop=start+n}
      end
      
(* val subslice : slice * int * int option -> slice *)
  fun subslice (SL{base, start, stop}, i, sz) =
      if geu(i, stop-start)  (* checks start >= 0 *)
      then raise Core.Subscript
      else case sz
	     of NONE => SL{base=base,start=start+i,stop=stop}
	      | SOME n => 
		 if geu(n, stop-start-i) (* checks n >= 0 *)
		     then raise Core.Subscript
		 else SL{base=base,start=start+i,stop=start+i+n}

(*
(* val base : slice -> vector * int * int *)
  fun full base = SL{base=base,start=0,stop=V.length base}
      let val blen = A.length base
       in if geu(start, blen)  (* checks start >= 0 *)
          then raise Core.Subscript
	  else case lenOp
		 of NONE => SL{base=base,start=start,stop=blen}
		  | SOME n => 
		      if geu(n, blen-start) (* checks n >= 0 *)
		      then raise Core.Subscript
		      else SL{base=base,start=start,stop=start+n}
      end
*)

(* val vector : slice -> vector *)
  fun vector (SL{base,start,stop}) =
      CharVector.tabulate(stop-start, fn n => sub'(base,n+start))

(* utility functions *)
  fun checkLen n =
      if InlineT.DfltInt.ltu(CharVector.maxLen, n)
	  then raise General.Size
      else ()

  fun rev ([], l) = l
    | rev (x::r, l) = rev (r, x::l)

(*
(* val concat : slice list -> vector *)
(* DBM: this is inefficient since it unnecessarily creates an intermediate
 * list containing all elements. Should calculate total length and preallocate
 * result vector and then copy elements directly from slices. *)
    fun concat [v] = vector v
      | concat vl =
	  (* get the total length and flatten the list *)
	  let val len = List.foldl (fn (vs,i) => (length vs)+i) 0 vl
              val _ = checkLen len
              val v = InlineT.CharVector.create len
	      fun cpslice (SL{base,start,stop},j) = 
		  let fun cp (i,j) =
		          if i = stop then j
		          else (InlineT.CharVector.update(v,j,sub'(base,i));
				cp (i+1,j+1))
                   in cp (start,stop)
		  end
           in List.foldl cpslice 0 vl;
	      v
	  end
*)

(* val isEmpty : slice -> bool *)
  fun isEmpty (SL{base,start,stop}) = stop<=start

(* val getItem : slice -> (elem * slice) option *)
  fun getItem (SL{base,start,stop}) =
      if stop<=start then NONE
      else SOME(sub'(base, start), SL{base=base,start=start+1,stop=stop})

(* val appi : (int * elem -> unit) -> slice -> unit *)
  fun appi f (SL{base,start,stop}) =
      let fun app i = if (i < stop)
	      then (f (i, sub'(base, i)); app(i+1))
	      else ()
       in app start
      end

(* val app  : (elem -> unit) -> slice -> unit *)
  fun app f (SL{base,start,stop}) =
      let fun app i = if (i < stop)
	      then (f (sub'(base, i)); app(i+1))
	      else ()
       in app start
      end

(* val mapi : (int * elem -> 'b) -> slice -> vector *)
  fun mapi f (SL{base,start,stop}) =
      let val len = stop - start
	  fun mapf (i, l) = if (i < stop)
		then mapf (i+1, f (i, sub'(base, i)) :: l)
		else Assembly.A.create_v(len, rev(l, []))
       in if (len > 0)
	      then mapf (start, [])
	  else Assembly.vector0
      end

(* val map  : (elem -> 'b) -> slice -> vector *)
  fun map f (SL{base,start,stop}) =
      let val len = stop - start
	  fun mapf (i, l) = if (i < stop)
		then mapf (i+1, f (sub'(base, i)) :: l)
		else Assembly.A.create_v(len, rev(l, []))
	  in
	    if (len > 0)
	      then mapf (start, [])
	      else Assembly.vector0
	  end

(* val foldli : (int * elem * 'b -> 'b) -> 'b -> slice -> 'b *)
  fun foldli f init (SL{base,start,stop}) = 
      let fun fold (i, accum) = if (i < stop)
	      then fold (i+1, f (i, sub'(base, i), accum))
	      else accum
       in fold (start, init)
      end

(* val foldri : (int * elem * 'b -> 'b) -> 'b -> slice -> 'b *)
  fun foldri f init (SL{base,start,stop}) =
      let fun fold (i, accum) = if (i >= start)
	      then fold (i-1, f (i, sub'(base, i), accum))
	      else accum
       in fold (stop - 1, init)
      end

(* val foldl  : (elem * 'b -> 'b) -> 'b -> slice -> 'b *)
  fun foldl f init (SL{base,start,stop}) = 
      let fun fold (i, accum) = if (i < stop)
	      then fold (i+1, f (sub'(base, i), accum))
	      else accum
       in fold (start, init)
      end

(* val foldr  : (elem * 'b -> 'b) -> 'b -> slice -> 'b *)
  fun foldr f init (SL{base,start,stop}) =
      let fun fold (i, accum) = if (i >= start)
	      then fold (i-1, f (sub'(base, i), accum))
	      else accum
       in fold (stop - 1, init)
      end

(* val findi : (int * elem -> bool) -> slice -> (int * elem) option *)
  fun findi f (SL{base,start,stop}) =
      let fun findi' i =
	      if (i < stop)
	      then let val item = (i,sub'(base, i))
		    in if f(item)
		       then SOME item
		       else findi' (i+1)
		   end
	      else NONE
       in findi' start
      end

(* val find  : (elem -> bool) -> slice -> elem option *)
  fun find f (SL{base,start,stop}) =
      let fun find' i =
	      if (i < stop)
	      then let val item = sub'(base, i)
		    in if f item
		       then SOME(item)
		       else find' (i+1)
		   end
	      else NONE
       in find' start
      end

(* val exists : (elem -> bool) -> slice -> bool *)
  fun exists f (SL{base,start,stop}) =
      let fun exists' i =
	      if (i < stop)
	      then if f(sub'(base, i))
		   then true
		   else exists' (i+1)
	      else false
       in exists' start
      end

(* val all : (elem -> bool) -> slice -> bool *)
  fun all f (SL{base,start,stop}) =
      let fun all' i =
	      if (i < stop)
	      then if f(sub'(base, i))
		   then all' (i+1)
		   else false
	      else true
       in all' start
      end

(* val collate : (elem * elem -> order) -> slice * slice -> order *)
  fun collate comp (SL{base,start,stop},SL{base=base',start=start',stop=stop'}) =
      let fun cmp (i,i') =
	      if (i < stop)
	      then if (i' >= stop') then GREATER
		   else case comp(sub'(base, i),
		                  sub'(base', i'))
                          of EQUAL => cmp(i+1,i'+1)
			   | x => x
	      else if (i' < stop') then LESS
	      else EQUAL
       in cmp(start,start')
      end

  fun base (SL{base=b,start,stop}) = (b, start, stop - start)

  fun copy {src = SL{base,start,stop},dst,di} =
      if di < 0 orelse 
	 di + (stop - start) > A.length dst 
      then raise Core.Subscript
      else if di <= start
      then let fun cp i =
                   if (i < stop)
	           then (update'(dst,di+i,sub'(base, i));
		         cp(i+1))
	           else ()
	    in cp start
	   end
      else let fun cp i =
                   if (i >= start)
	           then (update'(dst,di+i,sub'(base, i));
		         cp(i-1))
	           else ()
	    in cp (stop-1)
	   end

  fun copyVec {src,dst,di} =
      if di < 0 orelse 
	 di + CharVectorSlice.length src > A.length dst 
      then raise Core.Subscript
      else CharVectorSlice.appi(fn (i,x) => update'(dst,di+i,x)) src

  fun modifyi f (sl as SL{base,start,stop}) =
      appi (fn (i, x) => update' (base, start+i, f (i, x))) sl

  fun modify f (sl as SL{base,start,stop}) =
      appi (fn (i, x) => update' (base, start+i, f x)) sl
end (* structure CharArraySlice *)
