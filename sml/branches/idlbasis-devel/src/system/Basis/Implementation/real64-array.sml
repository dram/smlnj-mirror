(* real64-array.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

structure Real64Array : MONO_ARRAY =
  struct

  (* unchecked access operations *)
    val unsafeUpdate = InlineT.Real64Array.update
    val unsafeSub = InlineT.Real64Array.sub
(*    val vecUpdate = InlineT.Real64Vector.update*) (** not yet **)
    val vecSub = InlineT.Real64Vector.sub
    val vecLength = InlineT.Real64Vector.length

    type array = Assembly.A.real64array
    type elem = Real64.real
    type vector = Real64Vector.vector

    val maxLen = Core.max_length

    fun array (0, _) = InlineT.Real64Array.newArray0()
      | array (len, v) =if (InlineT.DfltInt.ltu(maxLen, len))
	    then raise General.Size
	    else let
	      val arr = Assembly.A.create_r len
	      fun init i = if (i < len)
		    then (unsafeUpdate(arr, i, v); init(i+1))
		    else ()
	      in
		init 0; arr
	      end

    fun tabulate (0, _) = InlineT.Real64Array.newArray0()
      | tabulate (len, f) = if (InlineT.DfltInt.ltu(maxLen, len))
	    then raise General.Size
	    else let
	      val arr = Assembly.A.create_r len
	      fun init i = if (i < len)
		    then (unsafeUpdate(arr, i, f i); init(i+1))
		    else ()
	      in
		init 0; arr
	      end

    fun fromList [] = InlineT.Real64Array.newArray0()
      | fromList l = let
	  fun length ([], n) = n
	    | length (_::r, n) = length (r, n+1)
	  val len = length (l, 0)
	  val _ = if (maxLen < len) then raise General.Size else ()
	  val arr = Assembly.A.create_r len
	  fun init ([], _) = ()
	    | init (c::r, i) = (unsafeUpdate(arr, i, c); init(r, i+1))
	  in
	    init (l, 0); arr
	  end

    val length = InlineT.Real64Array.length
    val sub    = InlineT.Real64Array.chkSub
    val update = InlineT.Real64Array.chkUpdate

(*
    fun extract (v, base, optLen) = let
	  val len = length v
	  fun newVec n = let
		fun tab (~1, l) = Assembly.A.create_v(n, l)
		  | tab (i, l) = tab(i-1, unsafeSub(v, base+i)::l)
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
*)

    fun vector a = let
	val len = length a
    in
	if 0 < len then let
		fun tab (~1, l) = Assembly.A.create_v(len, l)
		  | tab (i, l) = tab(i-1, unsafeSub (a, i)::l)
	    in
		tab (len-1, [])
	    end
	else Assembly.vector0
    end

    fun copy {src, dst, di} = let
	val srcLen = length src
	val sstop = srcLen
	val dstop = di + srcLen
	fun copyDown (j, k) =
	    if 0 <= j then
		(unsafeUpdate (dst, k, unsafeSub (src, j));
		 copyDown (j - 1, k - 1))
	    else ()
    in
	if di < 0 orelse length dst < dstop then raise Subscript
	else copyDown (sstop - 1, dstop - 1)
    end
(*
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
		  unsafeUpdate(dst, k, unsafeSub(src, j));
		  copyUp (j+1, k+1))
		else ()
	  fun copyDown (j, k) = if (si <= j)
		then (
		  unsafeUpdate(dst, k, unsafeSub(src, j));
		  copyDown (j-1, k-1))
		else ()
	  in
	    if ((di < 0) orelse (length src < sstop))
	      then raise Subscript
	    else if (si < di)
	      then copyDown (sstop-1, dstop-1)
	      else copyUp (si, di)
	  end
*)

    fun copyVec {src, dst, di} = let
	val srcLen = vecLength src
	val sstop = srcLen
	val dstop = di + srcLen
	(* assuming that there is no aliasing between vectors and arrays
	 * it should not matter whether we copy up or down... *)
	fun copyDown (j, k) =
	    if 0 <= j then
		(unsafeUpdate (dst, k, vecSub (src, j));
		 copyDown (j - 1, k - 1))
	    else ()
    in
	if di < 0 orelse length dst < dstop then raise Subscript
	else copyDown (sstop - 1, dstop - 1)
    end
(*
    fun copyVec {src, si, len, dst, di} = let
	  val (sstop, dstop) = let
		val srcLen = vecLength src
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
		  unsafeUpdate(dst, k, vecSub(src, j));
		  copyUp (j+1, k+1))
		else ()
	  in
	    if ((di < 0) orelse (vecLength src < sstop))
	      then raise Subscript
	      else copyUp (si, di)
	  end
*)

    fun app f arr = let
	  val len = length arr
	  fun app i = if (i < len)
		then (f (unsafeSub(arr, i)); app(i+1))
		else ()
	  in
	    app 0
	  end

    fun foldl f init arr = let
	  val len = length arr
	  fun fold (i, accum) = if (i < len)
		then fold (i+1, f (unsafeSub(arr, i), accum))
		else accum
	  in
	    fold (0, init)
	  end

    fun foldr f init arr = let
	  fun fold (i, accum) = if (i >= 0)
		then fold (i-1, f (unsafeSub(arr, i), accum))
		else accum
	  in
	    fold (length arr - 1, init)
	  end

   fun modify f arr = let
	  val len = length arr
	  fun modify' i = if (i < len)
		then (
		  unsafeUpdate(arr, i, f (unsafeSub(arr, i)));
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

    fun appi f arr = let
	  val stop = length arr
	  fun app i = if (i < stop)
		then (f (i, unsafeSub(arr, i)); app(i+1))
		else ()
	  in
	    app 0
	  end

    fun foldli f init arr = let
	  val stop = length arr
	  fun fold (i, accum) = if (i < stop)
		then fold (i+1, f (i, unsafeSub(arr, i), accum))
		else accum
	  in
	    fold (0, init)
	  end

    fun foldri f init arr = let
	  val stop = length arr
	  fun fold (i, accum) = if (i >= 0)
		then fold (i-1, f (i, unsafeSub(arr, i), accum))
		else accum
	  in
	    fold (stop - 1, init)
	  end

    fun modifyi f arr = let
	  val stop = length arr
	  fun modify' i = if (i < stop)
		then (
		  unsafeUpdate(arr, i, f (i, unsafeSub(arr, i)));
		  modify'(i+1))
		else ()
	  in
	    modify' 0
	  end

    fun findi p a = let
	val len = length a
	fun loop i =
	    if i >= len then NONE
	    else let val v = unsafeSub (a, i)
		 in if p (i, v) then SOME (i, v) else loop (i + 1)
		 end
    in
	loop 0
    end

    fun find p a = let
	val len = length a
	fun loop i =
	    if i >= len then NONE
	    else let val v = unsafeSub (a, i)
		 in if p v then SOME v else loop (i + 1)
		 end
    in
	loop 0
    end

    fun exists p a = let
	val len = length a
	fun loop i =
	    i < len andalso (p (unsafeSub (a, i)) orelse loop (i + 1))
    in
	loop 0
    end

    fun all p a = let
	val len = length a
	fun loop i =
	    i >= len orelse (p (unsafeSub (a, i)) andalso loop (i + 1))
    in
	loop 0
    end

    fun collate ecmp (a, b) = let
	val al = length a
	val bl = length b
	val l = if al < bl then al else bl
	fun loop i =
	    if i >= l then Int31Imp.compare (al, bl)
	    else case ecmp (unsafeSub (a, i), unsafeSub (b, i)) of
		     EQUAL => loop (i + 1)
		   | unequal => unequal
    in
	loop 0
    end

  end (* structure Real64Array *)
