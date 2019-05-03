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
      infix o val op o : ('b -> 'c) * ('a -> 'b) -> 'a -> 'c = InLine.inl_compose
      val not : bool -> bool = InLine.inl_not
      infix 7 *     val op * : word32 * word32 -> word32 = InLine.word32_mul
      infix 6 + -
      val op + : word32 * word32 -> word32 = InLine.word32_add
      val op - : word32 * word32 -> word32 = InLine.word32_sub
      infix 5 << >>
      val op << : word32 * word -> word32 = InLine.word32_lshift
      val op >> : word32 * word -> word32 = InLine.word32_rshiftl
      infix 5 ++    val op ++ = InLine.word32_orb
      infix 5 &     val op & : word32 * word32 -> word32 = InLine.word32_andb
      infix 4 <     val op < = InLine.word32_lt
      infix 4 <=    val op <= = InLine.word32_le
      infix 4 >     val op > = InLine.word32_gt
      infix 4 >=    val op >= = InLine.word32_ge

      val w64p : word64 -> word32 * word32 = InLine.word64_to_pair
      val p64w : word32 * word32 -> word64 = InLine.word64_from_pair

      fun lift1' f = f o w64p
      fun lift1 f = p64w o lift1' f
      fun lift2' f (x, y) = f (w64p x, w64p y)
      fun lift2 f = p64w o lift2' f

      fun split16 w32 = (w32 >> 0w16, w32 & 0wxffff)

      fun neg64 (hi, 0w0) = (InLine.word32_neg hi, 0w0)
	| neg64 (hi, lo) = (InLine.word32_notb hi, InLine.word32_neg lo)

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
      val w2i : word32 -> int32 = InLine.copy_word32_to_int32
      val i2w : int32 -> word32 = InLine.copy_int32_to_word32

    (* bitwise equivalence '≡' *)
      fun ^= (a, b) = InLine.word32_notb(InLine.word32_xorb(a, b))
      infix 4 ^=

      fun add64 ((hi1, lo1), (hi2, lo2)) = let
	    val lo = lo1 + lo2
	  (* from "Hacker's Delight": c = ((lo1 & lo2) | ((lo1 | lo2) & ¬lo)) >> 31 *)
	    val c = ((lo1 & lo2) ++ ((lo1 ++ lo2) & InLine.word32_notb lo)) >> 0w31
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
	    val b = ((InLine.word32_notb lo1 & lo2) ++ ((lo1 ^= lo2) & lo)) >> 0w31
	    val hi = hi1 - hi2 - b
	    in
	      (hi, lo)
	    end
*)

(*
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
*)
  (* from Hacker's Delight (Figure 8.1); this version does not use conditionals
   * and is about 7% faster.
   *)
      fun mul64 ((hi1, lo1), (hi2, lo2)) = let
	    val a1 = (lo1 >> 0w16) val a0 = (lo1 & 0wxffff)
	    val a3 = (hi1 >> 0w16) val a2 = (hi1 & 0wxffff)
	    val b1 = (lo2 >> 0w16) val b0 = (lo2 & 0wxffff)
	    val b3 = (hi2 >> 0w16) val b2 = (hi2 & 0wxffff)
	    val t = a0 * b0;
	    val acc0 = (t & 0wxffff)
	    val t = a1 * b0 + (t >> 0w16)
	    val acc1 = (t & 0wxffff)
	    val t = a2 * b0 + (t >> 0w16)
	    val acc2 = (t & 0wxffff)
	    val t = a3 * b0 + (t >> 0w16)
	    val acc3 = (t & 0wxffff)
	    val t = a0 * b1 + acc1
	    val acc1 = (t & 0wxffff)
	    val t = a1 * b1 + acc2 + (t >> 0w16)
	    val acc2 = (t & 0wxffff)
	    val t = a2 * b1 + acc3 + (t >> 0w16)
	    val acc3 = (t & 0wxffff)
	    val t = a0 * b2 + acc2
	    val acc2 = (t & 0wxffff)
	    val t = a1 * b2 + acc3 + (t >> 0w16)
	    val acc3 = (t & 0wxffff)
	    val t = a0 * b3 + acc3
	    val acc3 = (t & 0wxffff)
	    in
	      ((acc3 << 0w16) ++ acc2, (acc1 << 0w16) ++ acc0)
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
	    (hi1 < hi2) orelse (InLine.word32_eql (hi1, hi2) andalso lo1 < lo2)
      fun gt64 ((hi1, lo1), (hi2, lo2)) =
            (hi1 > hi2) orelse (InLine.word32_eql (hi1, hi2) andalso (lo1 > lo2))
      fun le64 ((hi1, lo1), (hi2, lo2)) =
	    (hi1 < hi2) orelse (InLine.word32_eql (hi1, hi2) andalso (lo1 <= lo2))
      fun ge64 ((hi1, lo1), (hi2, lo2)) =
            (hi1 > hi2) orelse (InLine.word32_eql (hi1, hi2) andalso (lo1 >= lo2))

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
