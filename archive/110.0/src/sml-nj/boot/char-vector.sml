(* char-vector.sml
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *
 * Vectors of characters (aka strings).
 *
 *)

structure CharVector : MONO_VECTOR =
  struct

    val (op <)  = InlineT.DfltInt.<
    val (op >=) = InlineT.DfltInt.>=
    val (op +)  = InlineT.DfltInt.+

  (* unchecked access operations *)
    val unsafeUpdate = InlineT.CharVector.update
    val unsafeSub = InlineT.CharVector.sub

    type elem = char
    type vector = string

    val maxLen = String.maxSize

    val fromList = String.implode

    fun tabulate (0, _) = ""
      | tabulate (n, f) = let
	  val _ = if (InlineT.DfltInt.ltu(maxLen, n)) then raise General.Size else ()
	  val ss = Assembly.A.create_s n
	  fun fill i = if (i < n)
		then (unsafeUpdate(ss, i, f i); fill(i+1))
		else ()
	  in
	    fill 0; ss
	  end

    val length   = InlineT.CharVector.length
    val sub      = InlineT.CharVector.chkSub

    val extract  = String.extract
    val concat   = String.concat

    fun app f vec = let
	  val len = length vec
	  fun app i = if (i < len)
		then (f (unsafeSub(vec, i)); app(i+1))
		else ()
	  in
	    app 0
	  end

    val map = String.map

    fun foldl f init vec = let
	  val len = length vec
	  fun fold (i, accum) = if (i < len)
		then fold (i+1, f (unsafeSub(vec, i), accum))
		else accum
	  in
	    fold (0, init)
	  end

    fun foldr f init vec = let
	  fun fold (i, accum) = if (i >= 0)
		then fold (i-1, f (unsafeSub(vec, i), accum))
		else accum
	  in
	    fold (length vec - 1, init)
	  end

    fun chkSlice (vec, i, NONE) = let val len = length vec
	  in
	    if (InlineT.DfltInt.ltu(len, i))
	      then raise Subscript
	      else (vec, i, len)
	  end
      | chkSlice (vec, i, SOME n) = let val len = length vec
	  in
	    if ((0 <= i) andalso (0 <= n) andalso (i+n <= len))
	      then (vec, i, i+n)
	      else raise Subscript
	  end

    fun appi f slice = let
	  val (vec, start, stop) = chkSlice slice
	  fun app i = if (i < stop)
		then (f (i, unsafeSub(vec, i)); app(i+1))
		else ()
	  in
	    app start
	  end

    fun mapi f slice = let
	  val (vec, start, stop) = chkSlice slice
	  in
	    case (stop - start)
	     of 0 => ""
	      | len => let
		  val newVec = Assembly.A.create_s len
		  fun mapf (i, j) = if (i < len)
			then (
			  unsafeUpdate(newVec, i, f(j, unsafeSub(vec, j)));
			  mapf(i+1, j+1))
			else ()
		  in
		    mapf (0, start); newVec
		  end
	    (* end case *)
	  end

    fun foldli f init slice = let
	  val (vec, start, stop) = chkSlice slice
	  fun fold (i, accum) = if (i < stop)
		then fold (i+1, f (i, unsafeSub(vec, i), accum))
		else accum
	  in
	    fold (start, init)
	  end

    fun foldri f init slice = let
	  val (vec, start, stop) = chkSlice slice
	  fun fold (i, accum) = if (i >= start)
		then fold (i-1, f (i, unsafeSub(vec, i), accum))
		else accum
	  in
	    fold (stop - 1, init)
	  end

  end (* CharVector *)


(*
 * $Log: char-vector.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:37  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.6  1997/10/08 19:53:15  jhr
 *   Fixed bug with mapi function using wrong base (bug #1293).
 *
 * Revision 1.5  1997/07/07  18:06:33  jhr
 *   Added extract function to String (moved the implementation from CharVector).
 *
 * Revision 1.4  1997/05/29  14:44:20  jhr
 *   SML'97 Basis Library changes (phase 1)
 *
 * Revision 1.3  1997/03/19  20:08:48  george
 *   bugfix for 1108 -- String.maxSize missnamed as String.maxLen
 *
 * Revision 1.2  1997/02/11  15:15:38  george
 * got rid of structure rebinding, since inlining is now preserved
 *
 * Revision 1.1.1.1  1997/01/14  01:38:13  george
 *   Version 109.24
 *
 *)
