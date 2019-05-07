(* core-word64.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This module defines multiplication, division, and modulo for 64-bit
 * words on 32-bit targets.  The other Word64 operations are directly
 * supported by the compiler or are implemented in the Basis/Implementation
 * directory.
 *)

structure CoreWord64 =
  struct

    local
      infix 7 *
      val op * : word32 * word32 -> word32 = InLine.word32_mul
      infix 6 + -
      val op + : word32 * word32 -> word32 = InLine.word32_add
      val op - : word32 * word32 -> word32 = InLine.word32_sub
      infix 5 << >>
      val op << : word32 * word -> word32 = InLine.word32_lshift
      val op >> : word32 * word -> word32 = InLine.word32_rshiftl
      infix 5 ++
      val op ++ : word32 * word32 -> word32 = InLine.word32_orb
      infix 5 &
      val op & : word32 * word32 -> word32 = InLine.word32_andb

      val extern : word64 -> word32 * word32 = InLine.word64_to_pair
      val intern : word32 * word32 -> word64 = InLine.word64_from_pair

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
	val up = CII.copy64Inf
        val dn = CII.truncInf64
      in
      (* This is even more inefficient than doing it the hard way,
       * but I am lazy... *)
      fun div64 (x, y) = dn (CII.div (up x, up y))
      fun mod64 (x, y) = dn (CII.mod (up x, up y))
      end

      fun lift2 f (x, y) = intern (f (extern x, extern y))

    in
    val op * = lift2 mul64
    val div = lift2 div64
    val mod = lift2 mod64
    end (* local *)

  end (* structure CoreWord64 *)
