(* array.sml
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *
 *)

structure Array : ARRAY =
  struct
    val (op +)  = InlineT.DfltInt.+
    val (op <)  = InlineT.DfltInt.<
    val (op >=) = InlineT.DfltInt.>=

    type 'a array = 'a PrimTypes.array
    type 'a vector = 'a PrimTypes.vector

    val maxLen = Core.max_length

    val array : int * 'a -> 'a array = InlineT.PolyArray.array
(*
    fun array (0, _) = InlineT.PolyArray.newArray0()
      | array (n, init) = 
          if InlineT.DfltInt.ltu(maxLen, n) then raise Core.Size 
          else Assembly.A.array (n, init)
*)

    fun tabulate (0, _) = InlineT.PolyArray.newArray0()
      | tabulate (n, f : int -> 'a) : 'a array = 
          let val a = array(n, f 0)
              fun tab i = 
                if (i < n) then (InlineT.PolyArray.update(a, i, f i); tab(i+1))
                else a
           in tab 1
          end

    fun fromList [] = InlineT.PolyArray.newArray0()
      | fromList (l as (first::rest)) = 
          let fun len(_::_::r, i) = len(r, i+2)
                | len([x], i) = i+1
                | len([], i) = i
              val n = len(l, 0)
              val a = array(n, first)
              fun fill (i, []) = a
                | fill (i, x::r) = 
                    (InlineT.PolyArray.update(a, i, x); fill(i+1, r))
           in fill(1, rest)
          end

    val length : 'a array -> int = InlineT.PolyArray.length
    val sub : 'a array * int -> 'a = InlineT.PolyArray.chkSub
    val update : 'a array * int * 'a -> unit = InlineT.PolyArray.chkUpdate

    fun extract (v, base, optLen) = let
	  val len = length v
	  fun newVec n = let
		fun tab (~1, l) = Assembly.A.create_v(n, l)
		  | tab (i, l) = tab(i-1, InlineT.PolyArray.sub(v, base+i)::l)
		in
		  tab (n-1, [])
		end
	  in
	    case (base, optLen)
	     of (0, NONE) => if (0 < len) then newVec len else Assembly.vector0
	      | (_, SOME 0) => if ((base < 0) orelse (len < base))
		  then raise General.Subscript
		  else Assembly.vector0
	      | (_, NONE) => if ((base < 0) orelse (len < base))
		    then raise General.Subscript
		  else if (len = base)
		    then Assembly.vector0
		    else newVec (len - base)
	      | (_, SOME n) =>
		  if ((base < 0) orelse (n < 0) orelse (len < (base+n)))
		    then raise General.Subscript
		    else newVec n
	    (* end case *)
	  end

    fun copy {src, si, len, dst, di} = let
	  val (sstop, dstop) = let
		val srcLen = length src
		in
		  case len
		   of NONE => if ((si < 0) orelse (srcLen < si))
		        then raise Subscript
		        else (srcLen, di+srcLen-si)
		    | (SOME n) => if ((n < 0) orelse (si < 0) orelse (srcLen < si+n))
		        then raise Subscript
		        else (si+n, di+n)
		  (* end case *)
		end
	  fun copyUp (j, k) = if (j < sstop)
		then (
		  InlineT.PolyArray.update(dst, k, InlineT.PolyArray.sub(src, j));
		  copyUp (j+1, k+1))
		else ()
	  fun copyDown (j, k) = if (si <= j)
		then (
		  InlineT.PolyArray.update(dst, k, InlineT.PolyArray.sub(src, j));
		  copyDown (j-1, k-1))
		else ()
	  in
	    if ((di < 0) orelse (length dst < dstop))
	      then raise Subscript
	    else if (si < di)
	      then copyDown (sstop-1, dstop-1)
	      else copyUp (si, di)
	  end

    fun copyVec {src, si, len, dst, di} = let
	  val (sstop, dstop) = let
		val srcLen = InlineT.PolyVector.length src
		in
		  case len
		   of NONE => if ((si < 0) orelse (srcLen < si))
		        then raise Subscript
		        else (srcLen, di+srcLen-si)
		    | (SOME n) => if ((n < 0) orelse (si < 0) orelse (srcLen < si+n))
		        then raise Subscript
		        else (si+n, di+n)
		  (* end case *)
		end
	  fun copyUp (j, k) = if (j < sstop)
		then (
		  InlineT.PolyArray.update(dst, k, InlineT.PolyVector.sub(src, j));
		  copyUp (j+1, k+1))
		else ()
	  in
	    if ((di < 0) orelse (length dst < dstop))
	      then raise Subscript
	      else copyUp (si, di)
	  end

    fun app f arr = let
	  val len = length arr
	  fun app i = if (i < len)
		then (f (InlineT.PolyArray.sub(arr, i)); app(i+1))
		else ()
	  in
	    app 0
	  end

    fun foldl f init arr = let
	  val len = length arr
	  fun fold (i, accum) = if (i < len)
		then fold (i+1, f (InlineT.PolyArray.sub(arr, i), accum))
		else accum
	  in
	    fold (0, init)
	  end

    fun foldr f init arr = let
	  fun fold (i, accum) = if (i >= 0)
		then fold (i-1, f (InlineT.PolyArray.sub(arr, i), accum))
		else accum
	  in
	    fold (length arr - 1, init)
	  end

    fun modify f arr = let
	  val len = length arr
	  fun modify' i = if (i < len)
		then (
		  InlineT.PolyArray.update(arr, i, f (InlineT.PolyArray.sub(arr, i)));
		  modify'(i+1))
		else ()
	  in
	    modify' 0
	  end

    fun chkSlice (arr, i, NONE) = let val len = length arr
	  in
	    if (InlineT.DfltInt.ltu(len, i))
	      then raise Subscript
	      else (arr, i, len)
	  end
      | chkSlice (arr, i, SOME n) = let val len = length arr
	  in
	    if ((0 <= i) andalso (0 <= n) andalso (i+n <= len))
	      then (arr, i, i+n)
	      else raise Subscript
	  end

    fun appi f slice = let
	  val (arr, start, stop) = chkSlice slice
	  fun app i = if (i < stop)
		then (f (i, InlineT.PolyArray.sub(arr, i)); app(i+1))
		else ()
	  in
	    app start
	  end

    fun foldli f init slice = let
	  val (arr, start, stop) = chkSlice slice
	  fun fold (i, accum) = if (i < stop)
		then fold (i+1, f (i, InlineT.PolyArray.sub(arr, i), accum))
		else accum
	  in
	    fold (start, init)
	  end

    fun foldri f init slice = let
	  val (arr, start, stop) = chkSlice slice
	  fun fold (i, accum) = if (i >= start)
		then fold (i-1, f (i, InlineT.PolyArray.sub(arr, i), accum))
		else accum
	  in
	    fold (stop - 1, init)
	  end

    fun modifyi f slice = let
	  val (arr, start, stop) = chkSlice slice
	  fun modify' i = if (i < stop)
		then (
		  InlineT.PolyArray.update(arr, i,
		    f (i, InlineT.PolyArray.sub(arr, i)));
		  modify'(i+1))
		else ()
	  in
	    modify' start
	  end

  end (* structure Array *)


