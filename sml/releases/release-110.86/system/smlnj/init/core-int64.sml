(* core-int64.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Basic (simulated) 64-bit integer support.
 *
 * Author: Matthias Blume (blume@tti-c.org)
 *)

structure CoreInt64 =
  struct

    local
      structure CII = CoreIntInf

      infix o       val op o = InLine.compose
      infix 7 *     val op * = InLine.w32mul
      infix 6 + -   val op + = InLine.w32add     val op - = InLine.w32sub
      infix 5 << >> val op << = InLine.w32lshift val op >> = InLine.w32rshiftl
      infix 5 ++    val op ++ = InLine.w32orb
      infix 5 &     val op & = InLine.w32andb
      infix 4 <     val op < = InLine.w32lt
      infix 4 <=    val op <= = InLine.w32le
      infix 4 >     val op > = InLine.w32gt
      infix 4 >=    val op >= = InLine.w32ge
      infix 4 <>    val op <> = InLine.w32ne
      infix 4 ==    val op == = InLine.w32eq
      val not = InLine.inlnot

      fun lift1' f = f o InLine.i64p
      fun lift1 f = InLine.p64i o lift1' f
      fun lift2' f (x, y) = f (InLine.i64p x, InLine.i64p y)
      fun lift2 f = InLine.p64i o lift2' f

      fun neg64 (0wx80000000, 0w0) = raise Assembly.Overflow
	| neg64 (hi, 0w0) = (InLine.w32neg hi, 0w0)
	| neg64 (hi, lo) = (InLine.w32notb hi, InLine.w32neg lo)

      fun negbit hi = hi & 0wx80000000
      fun isneg hi = negbit hi <> 0w0

(*
      fun add64 ((hi1, lo1), (hi2, lo2)) =
	  let val (hi, lo) = (hi1 + hi2, lo1 + lo2)
	      val hi = if lo < lo1 then hi + 0w1 else hi
	      val nb1 = negbit hi1
	  in if nb1 <> negbit hi2 orelse nb1 == negbit hi then (hi, lo)
	     else raise Assembly.Overflow
	  end

      fun sub64 ((hi1, lo1), (hi2, lo2)) =
	  let val (hi, lo) = (hi1 - hi2, lo1 - lo2)
	      val hi = if lo1 < lo then hi - 0w1 else hi
	      val nb1 = negbit hi1
	  in if nb1 == negbit hi2 orelse nb1 == negbit hi then (hi, lo)
	     else raise Assembly.Overflow
	  end
*)

    (* these versions of addition and subtraction use fewer conditional
     * branches and the underlying trapping arithmetic to signal Overflow.
     * Experiments suggest that addition is ~16% faster and subtraction is
     * ~10-12% faster.
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
	    val hi1' = w2i hi1
	    val hi2' = w2i hi2
	  (* we need this test to get Overflow right in the edge cases *)
	    val hi = if InLine.i32le(hi1', hi2')
		  then i2w(InLine.i32add(InLine.i32add(hi1', w2i c), hi2'))
		  else i2w(InLine.i32add(InLine.i32add(hi2', w2i c), hi1'))
	    in
	      (hi, lo)
	    end

      fun sub64 ((hi1, lo1), (hi2, lo2)) = let
	    val lo = lo1 - lo2
	    val hi1' = w2i hi1
	    val hi2' = w2i hi2
	  (* from "Hacker's Delight": b = ((¬lo1 & lo2) | ((lo1 ≡ lo2) & lo)) >> 31 *)
	    val b = ((InLine.w32notb lo1 & lo2) ++ ((lo1 ^= lo2) & lo)) >> 0w31
	  (* we need this test to get Overflow right in the edge cases *)
	    val hi = if InLine.i32le(hi1', hi2')
		  then i2w(InLine.i32sub(InLine.i32sub(hi1', hi2'), w2i b))
		  else i2w(InLine.i32sub(InLine.i32sub(hi1', w2i b), hi2'))
	    in
	      (hi, lo)
	    end

      (* I am definitely too lazy to do this the pedestrian way, so
       * here we go... *)
      fun mul64 (x, y) =
	  CII.testInf64 (CII.* (CII.extendInf64 x, CII.extendInf64 y))

      fun div64 (_, (0w0, 0w0)) = raise Assembly.Div
	| div64 (x, (0w0, 0w1)) = x
	| div64 (x, (0wxffffffff, 0wxffffffff)) = neg64 x
	| div64 (x, y) =
	    (* again, the easy way out... *)
	    CII.truncInf64 (CII.div (CII.extendInf64 x, CII.extendInf64 y))

      fun mod64 (x, y) = sub64 (x, mul64 (div64 (x, y), y))

(*
  (* NOTE: a more efficient implementation is to compare the high 32 bits using
   * signed < and the lower 32 bits using unsigned <.
   *)
      fun lt64 ((hi1, lo1), (hi2, lo2)) = (case (isneg hi1, isneg hi2)
	     of (true, false) => true
	      | (false, true) => false
	      | _ => hi1 < hi2 orelse (hi1 == hi2 andalso lo1 < lo2)
	    (* end case *))
      fun gt64 ((hi1, lo1), (hi2, lo2)) = (case (isneg hi1, isneg hi2)
	     of (true, false) => false
	      | (false, true) => true
	      | _ => hi1 > hi2 orelse (hi1 == hi2 andalso lo1 > lo2)
	    (* end case *))
      val le64 = not o gt64
      val ge64 = not o lt64
*)
      fun lt64 ((hi1, lo1), (hi2, lo2)) =
	    InLine.i32lt(w2i hi1, w2i hi2) orelse ((hi1 == hi2) andalso (lo1 < lo2))

      fun le64 ((hi1, lo1), (hi2, lo2)) =
	    InLine.i32lt(w2i hi1, w2i hi2) orelse ((hi1 == hi2) andalso (lo1 <= lo2))

      fun gt64 ((hi1, lo1), (hi2, lo2)) =
	    InLine.i32gt(w2i hi1, w2i hi2) orelse ((hi1 == hi2) andalso (lo1 > lo2))

      fun ge64 ((hi1, lo1), (hi2, lo2)) =
	    InLine.i32gt(w2i hi1, w2i hi2) orelse ((hi1 == hi2) andalso (lo1 >= lo2))

      fun abs64 (hi, lo) = if isneg hi then neg64 (hi, lo) else (hi, lo)
    in
      val extern = InLine.i64p
      val intern = InLine.p64i

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
      val abs = lift1 abs64
    end (* local *)

  end
