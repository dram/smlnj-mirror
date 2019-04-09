(* string-cvt.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

structure StringCvt : STRING_CVT =
  struct

  (* get radix and realfmt types from type-only structure *)
    open StringCvt

    val op + = InlineT.DfltInt.+
    val op - = InlineT.DfltInt.-
    val op < = InlineT.DfltInt.<
    val op > = InlineT.DfltInt.>

    local
      fun fillStr (c, s, i, n) = let
	    val stop = i+n
	    fun fill j = if (j < stop)
		  then (InlineT.CharVector.update(s, j, c); fill(j+1))
		  else ()
	    in
	      fill i
	    end
      fun cpyStr (src, srcLen, dst, start) = let
	    fun cpy (i, j) = if (i < srcLen)
		  then (
		    InlineT.CharVector.update(dst, j, InlineT.CharVector.sub(src, i));
		    cpy (i+1, j+1))
		  else ()
	    in
	      cpy (0, start)
	    end
    in
    fun padLeft padChr wid s = let
	  val len = InlineT.CharVector.length s
	  val pad = wid - len
	  in
	    if (pad > 0)
	      then let
		val s' = PreString.create wid
		in
		  fillStr (padChr, s', 0, pad);
		  cpyStr (s, len, s', pad);
		  s'
		end
	      else s
	  end
    fun padRight padChr wid s = let
	  val len = InlineT.CharVector.length s
	  val pad = wid - len
	  in
	    if (pad > 0)
	      then let
		val s' = PreString.create wid
		in
		  fillStr (padChr, s', len, pad);
		  cpyStr (s, len, s', 0);
		  s'
		end
	      else s
	  end
    end (* local *)

    fun revImplode (0, _) = ""
      | revImplode (n, chars) = PreString.revImplode(n, chars)

    fun splitl pred getc rep = let
	  fun lp (n, chars, rep) = (case (getc rep)
		 of NONE => (revImplode(n, chars), rep)
		  | SOME(c, rep') => if (pred c)
		      then lp(n+1, c::chars, rep')
		      else (revImplode(n, chars), rep)
		(* end case *))
	  in
	    lp (0, [], rep)
	  end
    fun takel pred getc rep = let
	  fun lp (n, chars, rep) = (case (getc rep)
		 of NONE => revImplode(n, chars)
		  | SOME(c, rep') => if (pred c)
		      then lp(n+1, c::chars, rep')
		      else revImplode(n, chars)
		(* end case *))
	  in
	    lp (0, [], rep)
	  end
    fun dropl pred getc = let
	  fun lp rep = (case (getc rep)
		 of NONE => rep
		  | SOME(c, rep') => if (pred c) then lp rep' else rep
		(* end case *))
	  in
	    lp
	  end
    val skipWS = PreBasis.skipWS

  (* the cs type is the type used by scanString to represent a stream of
   * characters; we use the current index in the string being scanned.
   *)
    type cs = int
    val scanString = PreBasis.scanString

  end


(*
 * $Log: string-cvt.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:39  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.3  1997/11/18 17:19:23  jhr
 *   Fixed a bug in splitl (dropping the split character).
 *
 * Revision 1.2  1997/06/02  19:15:03  jhr
 *   SML'97 Basis Library changes (phase 2)
 *
 * Revision 1.1.1.1  1997/01/14  01:38:17  george
 *   Version 109.24
 *
 *)
