(* core-word64.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Basic (simulated) 64-bit word support.
 *
 * Author: Matthias Blume (blume@tti-c.org)
 *)

structure CoreWord64 =
  struct

    local
      infix o val op o : ('b -> 'c) * ('a -> 'b) -> 'a -> 'c = InLine.compose
      val not : bool -> bool = InLine.inlnot
      infix 7 *     val op * : word32 * word32 -> word32 = InLine.w32mul
      infix 6 + -
      val op + : word32 * word32 -> word32 = InLine.w32add
      val op - : word32 * word32 -> word32 = InLine.w32sub
      infix 5 << >>
      val op << : word32 * word -> word32 = InLine.w32lshift
      val op >> : word32 * word -> word32 = InLine.w32rshiftl
      infix 5 ++    val op ++ = InLine.w32orb
      infix 5 &     val op & : word32 * word32 -> word32 = InLine.w32andb
      infix 4 <     val op < = InLine.w32lt
      infix 4 <=    val op <= = InLine.w32le
      infix 4 >     val op > = InLine.w32gt
      infix 4 >=    val op >= = InLine.w32ge

      val w64p : word64 -> word32 * word32 = InLine.w64p
      val p64w : word32 * word32 -> word64 = InLine.p64w

      fun lift1' f = f o w64p
      fun lift1 f = p64w o lift1' f
      fun lift2' f (x, y) = f (w64p x, w64p y)
      fun lift2 f = p64w o lift2' f

      fun split16 w32 = (w32 >> 0w16, w32 & 0wxffff)

      fun neg64 (hi, 0w0) = (InLine.w32neg hi, 0w0)
	| neg64 (hi, lo) = (InLine.w32notb hi, InLine.w32neg lo)

(*
      fun add64 ((hi1, lo1), (hi2, lo2)) =
	  let val (lo, hi) = (lo1 + lo2, hi1 + hi2)
	  in (if lo < lo1 then hi + 0w1 else hi, lo)
	  end
*)

    (* this version of addition does not use conditional branches and
     * is slightly faster than version that does (see above).
     * They are based on the implementation in Hacker's Delight by
     * Henry S. Warren.
     *)

    (* bitcast conversions between Int32.int and Word32.word *)
      val w2i : word32 -> int32 = InLine.copy_32_32_wi
      val i2w : int32 -> word32 = InLine.copy_32_32_iw

    (* bitwise equivalence '≡' *)
      fun ^= (a, b) = InLine.w32notb(InLine.w32xorb(a, b))
      infix 4 ^=

      fun add64 ((hi1, lo1), (hi2, lo2)) = let
	    val lo = lo1 + lo2
	  (* from "Hacker's Delight": c = ((lo1 & lo2) | ((lo1 | lo2) & ¬lo)) >> 31 *)
	    val c = ((lo1 & lo2) ++ ((lo1 ++ lo2) & InLine.w32notb lo)) >> 0w31
	    val hi = hi1 + hi2 + c
	    in
	      (hi, lo)
	    end

      fun sub64 ((hi1, lo1), (hi2, lo2)) =
	  let val (lo, hi) = (lo1 - lo2, hi1 - hi2)
	  in (if lo1 < lo then hi - 0w1 else hi, lo)
	  end

(* version w/o conditional branches is slightly slower than the version with them
      fun sub64 ((hi1, lo1), (hi2, lo2)) = let
	    val lo = lo1 - lo2
	  (* from "Hacker's Delight": b = ((¬lo1 & lo2) | ((lo1 ≡ lo2) & lo)) >> 31 *)
	    val b = ((InLine.w32notb lo1 & lo2) ++ ((lo1 ^= lo2) & lo)) >> 0w31
	    val hi = hi1 - hi2 - b
	    in
	      (hi, lo)
	    end
*)

      fun mul64 ((hi1, lo1), (hi2, lo2)) = let
	    val ((a1, b1), (c1, d1)) = (split16 hi1, split16 lo1)
	    val ((a2, b2), (c2, d2)) = (split16 hi2, split16 lo2)
	    val dd = d1 * d2
	    val (cd, dc) = (c1 * d2, d1 * c2)
	    val (bd, cc, db) = (b1 * d2, c1 * c2, d1 * b2)
	    val (ad, bc, cb, da) = (a1 * d2, b1 * c2, c1 * b2, d1 * a2)
	    val diag0 = dd
	    val diag1 = cd + dc
	    val diag1carry = if diag1 < cd then 0wx10000 else 0w0
	    val diag2 = bd + cc + db
	    val diag3 = ad + bc + cb + da
	    val lo = diag0 + (diag1 << 0w16)
	    val locarry = if lo < diag0 then 0w1 else 0w0
	    val hi = (diag1 >> 0w16) + diag2 + (diag3 << 0w16)
		     + locarry + diag1carry
	    in
	      (hi, lo)
	    end

      local
	structure CII = CoreIntInf
	val up = CII.copyInf64 val dn = CII.truncInf64
      in
      (* This is even more inefficient than doing it the hard way,
       * but I am lazy... *)
      fun div64 (x, y) = dn (CII.div (up x, up y))
      end

      fun mod64 (x, y) = sub64 (x, mul64 (div64 (x, y), y))

      fun lt64 ((hi1, lo1), (hi2, lo2)) =
	    (hi1 < hi2) orelse (InLine.w32eq (hi1, hi2) andalso lo1 < lo2)
      fun gt64 ((hi1, lo1), (hi2, lo2)) =
            (hi1 > hi2) orelse (InLine.w32eq (hi1, hi2) andalso (lo1 > lo2))
      fun le64 ((hi1, lo1), (hi2, lo2)) =
	    (hi1 < hi2) orelse (InLine.w32eq (hi1, hi2) andalso (lo1 <= lo2))
      fun ge64 ((hi1, lo1), (hi2, lo2)) =
            (hi1 > hi2) orelse (InLine.w32eq (hi1, hi2) andalso (lo1 >= lo2))

    in
    val extern = w64p
    val intern = p64w

    val ~ = lift1 neg64
    val op + = lift2 add64
    val op - = lift2 sub64
    val op * = lift2 mul64
    val div = lift2 div64
    val mod = lift2 mod64
    val op < = lift2' lt64
    val op <= = lift2' le64
    val op > = lift2' gt64
    val op >= = lift2' ge64
    end (* local *)

  end (* structure CoreWord64 *)
