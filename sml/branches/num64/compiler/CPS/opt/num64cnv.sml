(* num64cnv.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This module supports the 64-bit int/word types on 32-bit machines
 * by expanding them to pairs of 32-bit words and replacing the primitive
 * operations with 32-bit code.
 *)

structure Num64Cnv : sig

  (* eliminate 64-bit literals and operations on a 32-bit machine; this function is
   * the identity on 64-bit machines.
   *)
    val elim : CPS.function -> CPS.function

  end = struct

    structure C = CPS
    structure P = C.P
    structure LV = LambdaVar

    fun bug s = ErrorMsg.impossible ("Num64Cnv: " ^ s)

    val pairTy = C.PTRt(C.RPT 2)
    val box32Ty = C.PTRt C.VPT
    val raw32Ty = C.NUMt{sz = 32, tag = false}	(* assuming a 32-bit machine *)
    val tagNumTy = C.NUMt{sz = 31, tag = true}
    val si32 = P.INT 32
    val ui32 = P.UINT 32

  (* split a 64-bit integer/word literal into two 32-bit unsigned values.  We assume
   * that the argument is in the range -2^63 .. 2^64-1, which is the union of the ranges
   * of Int64.int and Word64.word.
   *)
    fun split (n : IntInf.int) = let
	  val n = if (n < 0) then 0x10000000000000000 + n else n
	  val hi = C.NUM{ival = IntInf.~>>(n, 0w32), ty = {sz = 32, tag = false}}
	  val lo = C.NUM{ival = IntInf.andb(n, 0xffffffff), ty = {sz = 32, tag = false}}
	  in
	    (hi, lo)
	  end

  (* short names for various CPS constructs *)
    val zero = C.NUM{ival=0, ty={sz = 32, tag = false}}
    val one = C.NUM{ival=1, ty={sz = 32, tag = false}}
    fun num n = C.NUM{ival=n, ty={sz = 32, tag = false}}
    fun tagNum n = C.NUM{ival=n, ty={sz = 31, tag = true}}
    fun uIf (oper, a, b, tr, fl) = (* unsigned conditional *)
	  C.BRANCH(P.CMP{oper=oper, kind=ui32}, [a, b], LV.mkLvar(), tr, fl)
    fun sIf (oper, a, b, tr, fl) = (* signed conditional *)
	  C.BRANCH(P.CMP{oper=oper, kind=si32}, [a, b], LV.mkLvar(), tr, fl)
    fun ifzero (v, tr, fl) = uIf(P.EQL, v, zero, tr, fl)
    fun pure (rator, args, ty, k) = let
	  val x = LV.mkLvar()
	  in
	    C.PURE(rator, args, x, ty, k(C.VAR x))
	  end
    fun pure_arith32 (oper, args, k) =
	  pure (P.PURE_ARITH{oper=oper, kind=ui32}, args, raw32Ty, k)
    fun taggedArith (oper, args, k) =
	  pure (P.PURE_ARITH{oper=oper, kind=P.UINT 31}, args, tagNumTy, k)
    fun iarith32 (oper, args, k) = let
	  val x = LV.mkLvar()
	  in
	    C.ARITH(P.IARITH{oper=oper, sz=32}, args, x, raw32Ty, k(C.VAR x))
	  end

  (* bitwise equivalence *)
    fun bitEquiv (a, b, k) =
	  pure_arith32 (P.XORB, [a, b], fn a_xor_b =>
	  pure_arith32 (P.NOTB, [a_xor_b], k))

  (* bind a continuation around a cexp to avoid code duplication; `res` is the variable
   * to use as a parameter for the code `cexp` (we assume that it is a wrapped 64-bit
   * integer)
   *)
    fun join (res, cexp, k) = let
	  val fnId = LV.mkLvar()
	  in
	    C.FIX([(C.CONT, fnId, [res], [pairTy], cexp)],
	      k (fn v => C.APP(C.VAR fnId, [v])))
	  end

  (* given two 32-bit values that comprise a 64-bit number and a continuation `k`,
   * make code to create the 64-bit object
   *)
    fun to64 (hi, lo, k) = let
	  val pair = LV.mkLvar()
	  in
	    pure(P.WRAP(P.UINT 32), [lo], box32Ty, fn lo' =>
	    pure(P.WRAP(P.UINT 32), [hi], box32Ty, fn hi' =>
	      C.RECORD(C.RK_RECORD, [(hi', C.OFFp 0), (lo', C.OFFp 0)],
		pair, k(C.VAR pair))))
	  end

  (* given a 64-bit object and a continuation `k`, make code to unpackage the value into
   * two 32-bit values, which are passed to `k`.
   *)
    fun from64 (n, k) = let
	  val hi = LV.mkLvar()
	  val lo = LV.mkLvar()
	  in
	    C.SELECT(0, n, hi, box32Ty,
	    pure(P.UNWRAP(P.UINT 32), [C.VAR hi], raw32Ty, fn hi' =>
	    C.SELECT(1, n, lo, box32Ty,
	    pure(P.UNWRAP(P.UINT 32), [C.VAR lo], raw32Ty, fn lo' =>
	      k (hi', lo')))))
	  end

  (* given a 64-bit object and a continuation `k`, make code to unpackage the low 32 word,
   * which is passed to `k`.
   *)
    fun getLo32 (n, k) = let
	  val lo = LV.mkLvar()
	  in
	    C.SELECT(1, n, lo, box32Ty,
	    pure(P.UNWRAP(P.UINT 32), [C.VAR lo], raw32Ty, fn lo' =>
	      k lo'))
	  end

  (* given a 64-bit object and a continuation `k`, make code to unpackage the high 32 word,
   * which is passed to `k`.
   *)
    fun getHi32 (n, k) = let
	  val hi = LV.mkLvar()
	  in
	    C.SELECT(0, n, hi, box32Ty,
	    pure(P.UNWRAP(P.UINT 32), [C.VAR hi], raw32Ty, fn hi' =>
	      k hi'))
	  end

  (* split a 32-bit value into two 16-bit values *)
    fun split32 (n, k) =
	  pure_arith32(P.RSHIFT, [n, num 16], fn hi =>
	  pure_arith32(P.ANDB, [n, num 0xffff], fn lo =>
	    k (hi, lo)))

  (***** Word64 primitive operations *****)

  (*
   * fun add2 ((hi1, lo1), (hi2, lo2)) = let
   *       val hi = hi1 + hi2
   *       val lo = lo1 + lo2
   *     (* from "Hacker's Delight": c = ((lo1 & lo2) | ((lo1 | lo2) & ¬lo)) >> 31 *)
   *       val carry = (((lo1 ++ lo2) & ~~lo) ++ (lo1 & lo2)) >> 0w31
   *       val hi = hi + carry
   *       in
   *         (hi, lo)
   *       end
   *)
    fun w64Add (n1, n2, res, cexp) = join (res, cexp, fn k =>
	  from64 (n1, fn (hi1, lo1) =>
	  from64 (n2, fn (hi2, lo2) =>
	  pure_arith32(P.ADD, [hi1, hi2], fn hi =>
	  pure_arith32(P.ADD, [lo1, lo2], fn lo =>
	  pure_arith32(P.ORB, [lo1, lo2], fn lo1_or_lo2 =>
	  pure_arith32(P.NOTB, [lo], fn not_lo =>
	  pure_arith32(P.ANDB, [lo1_or_lo2, not_lo], fn tmp1 =>
	  pure_arith32(P.ANDB, [lo1, lo2], fn lo1_and_lo2 =>
	  pure_arith32(P.ORB, [lo1_and_lo2, tmp1], fn tmp2 =>
	  pure_arith32(P.RSHIFT, [tmp2, tagNum 31], fn carry =>
	  pure_arith32(P.ADD, [hi, carry], fn hi =>
	    to64(hi, lo, k)))))))))))))

  (*
   * fun sub ((hi1, lo1), (hi2, lo2)) = let
   *       val hi = hi1 - hi2 - b
   *       val lo = lo1 - lo2
   *     (* from "Hacker's Delight": b = ((¬lo1 & lo2) | ((lo1 ≡ lo2) & lo)) >> 31 *)
   *       val b = (((lo1 ^= lo2) & lo) ++ (~~lo1 & lo2)) >> 0w31
   *       val hi = hi - b
   *       in
   *         (hi, lo)
   *       end
   *)
    fun w64Sub (n1, n2, res, cexp) = join (res, cexp, fn k =>
	  from64 (n1, fn (hi1, lo1) =>
	  from64 (n2, fn (hi2, lo2) =>
	  pure_arith32(P.SUB, [hi1, hi2], fn hi =>
	  pure_arith32(P.SUB, [lo1, lo2], fn lo =>
	  bitEquiv(lo1, lo2, fn lo1_eqv_lo2 =>
	  pure_arith32(P.ANDB, [lo1_eqv_lo2, lo], fn tmp1 =>
	  pure_arith32(P.NOTB, [lo1], fn not_lo1 =>
	  pure_arith32(P.ANDB, [not_lo1, lo2], fn tmp2 =>
	  pure_arith32(P.ORB, [tmp1, tmp2], fn tmp3 =>
	  pure_arith32(P.RSHIFT, [tmp3, tagNum 31], fn borrow =>
	  pure_arith32(P.SUB, [hi, borrow], fn hi =>
	    to64(hi, lo, k)))))))))))))

  (*
   * fun orb (W64(hi1, lo1), W64(hi2, lo2)) = W64(hi1 ++ hi2, lo1 ++ lo2)
   *)
    fun w64Orb (n1, n2, res, cexp) = join (res, cexp, fn k =>
	  from64 (n1, fn (hi1, lo1) =>
	  from64 (n2, fn (hi2, lo2) =>
	  pure_arith32(P.ORB, [lo1, lo2], fn lo =>
	  pure_arith32(P.ORB, [hi1, hi2], fn hi =>
	    to64 (hi, lo, k))))))

  (*
   * fun xorb (W64(hi1, lo1), W64(hi2, lo2)) = W64(hi1 ^^ hi2, lo1^^ lo2)
   *)
    fun w64Xorb (n1, n2, res, cexp) = join (res, cexp, fn k =>
	  from64 (n1, fn (hi1, lo1) =>
	  from64 (n2, fn (hi2, lo2) =>
	  pure_arith32(P.XORB, [lo1, lo2], fn lo =>
	  pure_arith32(P.XORB, [hi1, hi2], fn hi =>
	    to64 (hi, lo, k))))))

  (*
   * fun andb (W64(hi1, lo1), W64(hi2, lo2)) = W64(hi1 & hi2, lo1 & lo2)
   *)
    fun w64Andb (n1, n2, res, cexp) = join (res, cexp, fn k =>
	  from64 (n1, fn (hi1, lo1) =>
	  from64 (n2, fn (hi2, lo2) =>
	  pure_arith32(P.ANDB, [lo1, lo2], fn lo =>
	  pure_arith32(P.ANDB, [hi1, hi2], fn hi =>
	    to64 (hi, lo, k))))))

  (*
   * fun notb (W64(hi, lo)) = W64(Word32.notb hi, Word32.notb lo)
   *)
    fun w64Notb (n, res, cexp) = join (res, cexp, fn k =>
	  from64 (n, fn (hi, lo) =>
	  pure_arith32(P.NOTB, [hi], fn hi' =>
	  pure_arith32(P.NOTB, [lo], fn lo' =>
	    to64 (hi', lo', k)))))

  (*
    fun neg (hi, 0w0) = (W32.~ hi, 0w0)
      | neg (hi, lo) = (W32.notb hi, W32.~ lo)
   *)
    fun w64Neg (n, res, cexp) = join (res, cexp, fn k =>
	  from64(n, fn (hi, lo) =>
	    ifzero(lo,
	      pure_arith32(P.NEG, [hi], fn hi' => to64 (hi', zero, k)),
	      (* else *)
	      pure_arith32(P.NOTB, [hi], fn hi' =>
	      pure_arith32(P.NEG, [lo], fn lo' =>
		to64 (hi', lo', k))))))

  (* logical shift-right, where we know that amt < 0w64
   *
   * fun w63RShiftL ((hi, lo), amt) =
   *	   if (amt < 32)
   *	     then let
   *	       val hi' = (hi >> amt)
   *           val lo' = (lo >> amt) | (hi << (0w32 - amt))
   *           in
   *             (hi', lo')
   *           end
   *         else (0, (hi >> (amt - 0w32)))
   *
   * Note, that while there is a branch-free version of this, it does not work
   * on the x86 architecture, which uses mod-32 shift amounts.
   *)
    fun w64RShiftL (n, amt, res, cexp) = join (res, cexp, fn k =>
	  from64(n, fn (hi, lo) =>
	    sIf(P.LT, amt, tagNum 32,
	      pure_arith32(P.RSHIFTL, [hi, amt], fn hi' =>
	      pure_arith32(P.RSHIFTL, [lo, amt], fn tmp1 =>
	      taggedArith(P.SUB, [tagNum 32, amt], fn tmp2 =>
	      pure_arith32(P.LSHIFT, [hi, tmp2], fn tmp3 =>
	      pure_arith32(P.ORB, [tmp1, tmp2], fn lo' =>
		to64(hi', lo', k)))))),
	      (* else *)
	      taggedArith(P.SUB, [amt, tagNum 32], fn tmp4 =>
	      pure_arith32(P.RSHIFTL, [hi, tmp4], fn lo' =>
		to64(zero, lo', k))))))

  (*arithmetic shift-right, where we know that amt < 0w64
   *
   * fun w63RShift ((hi, lo), amt) =
   *	   if (amt < 32)
   *	     then let
   *	       val hi' = (hi ~>> amt)
   *           val lo' = (lo >> amt) | (hi << (0w32 - amt))
   *           in
   *             (hi', lo')
   *           end
   *         else (hi ~>> 0w31, (hi ~>> (amt - 0w32)))
   *
   * Note, that while there is a branch-free version of this, it does not work
   * on the x86 architecture, which uses mod-32 shift amounts.
   *)
    fun w64RShift (n, amt, res, cexp) = join (res, cexp, fn k =>
	  from64(n, fn (hi, lo) =>
	    sIf(P.LT, amt, tagNum 32,
	      pure_arith32(P.RSHIFT, [hi, amt], fn hi' =>
	      pure_arith32(P.RSHIFT, [lo, amt], fn tmp1 =>
	      taggedArith(P.SUB, [tagNum 32, amt], fn tmp2 =>
	      pure_arith32(P.LSHIFT, [hi, tmp2], fn tmp3 =>
	      pure_arith32(P.ORB, [tmp1, tmp2], fn lo' =>
		to64(hi', lo', k)))))),
	      (* else *)
	      pure_arith32(P.RSHIFT, [hi, tagNum 31], fn hi' =>
	      taggedArith(P.SUB, [amt, tagNum 32], fn tmp4 =>
	      pure_arith32(P.RSHIFTL, [hi, tmp4], fn lo' =>
		to64(hi', lo', k)))))))

  (* shift-left, where we know that amt < 0w64
   *
   * fun w64LShift ((hi, lo), amt) =
   *	   if (amt < 0w32)
   *	     then let
   *	       val hi' = (hi << amt) | (lo >> (0w32 - amt))
   *           val lo' = (lo << amt)
   *           in
   *             (hi', lo')
   *           end
   *         else (lo << (amt - 0w32), 0)
   *
   * Note, that while there is a branch-free version of this, it does not work
   * on the x86 architecture, which uses mod-32 shift amounts.
   *)
    fun w64LShift (n, amt, res, cexp) = join (res, cexp, fn k =>
	  from64(n, fn (hi, lo) =>
	    sIf(P.LT, amt, tagNum 32,
	      pure_arith32(P.LSHIFT, [hi, amt], fn tmp1 =>
	      taggedArith(P.SUB, [tagNum 32, amt], fn tmp2 =>
	      pure_arith32(P.RSHIFTL, [lo, tmp2], fn tmp3 =>
	      pure_arith32(P.ORB, [tmp1, tmp2], fn hi' =>
	      pure_arith32(P.LSHIFT, [lo, amt], fn lo' =>
		to64(hi', lo', k)))))),
	      (* else *)
	      taggedArith(P.SUB, [amt, tagNum 32], fn tmp4 =>
	      pure_arith32(P.LSHIFT, [lo, tmp4], fn hi' =>
		to64(hi', zero, k))))))

  (*
   * fun w64Eql ((hi1, lo1), (hi2, lo2)) =
   *       (W32.orb(W32.xorb(hi1, hi2), W32.xorb(lo1, lo2)) = 0)
   *)
    fun w64Eql (n1, n2, tr, fl) =
	  from64(n1, fn (hi1, lo1) =>
	  from64(n2, fn (hi2, lo2) =>
	    pure_arith32(P.XORB, [hi1, hi2], fn hi' =>
	    pure_arith32(P.XORB, [lo1, lo2], fn lo' =>
	    pure_arith32(P.ORB, [hi', lo'], fn res =>
	      ifzero (res, tr, fl))))))

  (* the basic pattern for comparisons is
   *   fun cmp ((hi1, lo1), (hi2, lo2)) =
   *         OP(hi1, hi2) orelse ((hi1 = hi2) andalso OP(lo1, lo2))
   *)
    local
      fun w64Cmp oper (n1, n2, tr, fl) = let
	  (* continuations for the branches so that we can avoid code duplication *)
	    val trFnId = LV.mkLvar()
	    val tr' = C.APP(C.VAR trFnId, [])
	    val flFnId = LV.mkLvar()
	    val fl' = C.APP(C.VAR flFnId, [])
	    in
	      C.FIX([(C.CONT, trFnId, [], [], tr), (C.CONT, flFnId, [], [], fl)],
	      (* (hi1 < hi2) orelse ((hi1 = hi2) andalso (lo1 < lo2)) *)
		getHi32(n1, fn hi1 =>
		getHi32(n2, fn hi2 =>
		  uIf(oper, hi1, hi2,
		    tr',
		    uIf(P.EQL, hi1, hi2,
		      getLo32(n1, fn lo1 =>
		      getLo32(n2, fn lo2 =>
			uIf(oper, lo1, lo2, tr', fl'))),
		      fl')))))
	    end
    in
    val w64Less = w64Cmp P.LT
    val w64LessEq = w64Cmp P.LTE
    val w64Greater = w64Cmp P.GT
    val w64GreaterEq = w64Cmp P.GTE
    end (* local *)

  (***** Int64 primitive operations *****)

  (*
      fun add64 ((hi1, lo1), (hi2, lo2)) = let
	    val lo = lo1 + lo2
	  (* from "Hacker's Delight": c = ((lo1 & lo2) | ((lo1 | lo2) & ¬lo)) >> 31 *)
	    val carry = ((lo1 & lo2) ++ ((lo1 ++ lo2) & not lo)) >> 0w31
	  (* we add the carry to the smaller hi component before add them; this
	   * check is needed to get Overflow right in the edge cases
	   *)
	    val hi = if InLine.int32_le(hi1, hi2)
		  then InLine.int32_add(InLine.int32_add(hi1, c), hi2)
		  else InLine.int32_add(InLine.int32_add(hi2, c), hi1)
	    in
	      (hi, lo)
	    end
   *)
    fun i64Add (n1, n2, res, cexp) = let
	  val hi = LV.mkLvar()
	  in
	    join (res, cexp, fn k =>
	      from64(n1, fn (hi1, lo1) =>
	      from64(n2, fn (hi2, lo2) =>
	      pure_arith32(P.ADD, [lo1, lo2], fn lo =>
	      pure_arith32(P.ORB, [lo1, lo2], fn lo1_or_lo2 =>
	      pure_arith32(P.NOTB, [lo], fn not_lo =>
	      pure_arith32(P.ANDB, [lo1_or_lo2, not_lo], fn tmp1 =>
	      pure_arith32(P.ANDB, [lo1, lo2], fn lo1_and_lo2 =>
	      pure_arith32(P.ORB, [lo1_and_lo2, tmp1], fn tmp2 =>
	      pure_arith32(P.RSHIFT, [tmp2, tagNum 31], fn carry =>
	      join (hi,
		to64(C.VAR hi, lo, k),
		fn k' =>
		  sIf(P.LTE, hi1, hi2,
		    iarith32(P.IADD, [hi1, carry], fn tmp1 =>
		    iarith32(P.IADD, [tmp1, hi2], k')),
		    (* else *)
		    iarith32(P.IADD, [hi2, carry], fn tmp2 =>
		    iarith32(P.IADD, [tmp2, hi1], k'))))))))))))))
	  end

  (*
      fun sub64 ((hi1, lo1), (hi2, lo2)) = let
	    val lo = lo1 - lo2
	  (* from "Hacker's Delight": b = ((¬lo1 & lo2) | ((lo1 ≡ lo2) & lo)) >> 31 *)
	    val b = ((InLine.word32_notb lo1 & lo2) ++ ((lo1 ^= lo2) & lo)) >> 0w31
	  (* we need this test to get Overflow right in the edge cases *)
	    val hi = if InLine.int32_le(hi1, hi2)
		  then InLine.int32_sub(InLine.int32_sub(hi1, hi2), b)
		  else InLine.int32_sub(InLine.int32_sub(hi1, b), hi2)
	    in
	      (hi, lo)
	    end
   *)
    fun i64Sub (n1, n2, res, cexp) = let
	  val hi = LV.mkLvar()
	  in
	    join (res, cexp, fn k =>
	      from64(n1, fn (hi1, lo1) =>
	      from64(n2, fn (hi2, lo2) =>
	      pure_arith32(P.SUB, [lo1, lo2], fn lo =>
	      bitEquiv(lo1, lo2, fn lo1_eqv_lo2 =>
	      pure_arith32(P.ANDB, [lo1_eqv_lo2, lo], fn tmp1 =>
	      pure_arith32(P.NOTB, [lo1], fn not_lo1 =>
	      pure_arith32(P.ANDB, [not_lo1, lo2], fn tmp2 =>
	      pure_arith32(P.ORB, [tmp1, tmp2], fn tmp3 =>
	      pure_arith32(P.RSHIFT, [tmp3, tagNum 31], fn borrow =>
	      join (hi,
		to64(C.VAR hi, lo, k),
		fn k' =>
		  sIf(P.LTE, hi1, hi2,
		    iarith32(P.IADD, [hi1, hi2], fn tmp1 =>
		    iarith32(P.IADD, [tmp1, borrow], k')),
		    (* else *)
		    iarith32(P.IADD, [hi1, borrow], fn tmp2 =>
		    iarith32(P.IADD, [tmp2, hi2], k'))))))))))))))
	  end

  (*
   * fun neg (hi, 0w0) = (I32.~ hi, 0w0)
   *   | neg (hi, lo) = (W32.notb hi, W32.~ lo)
   *)
    fun i64Neg (n, res, cexp) = join (res, cexp, fn k =>
	  from64(n, fn (hi, lo) =>
	    ifzero(lo,
	      iarith32(P.INEG, [hi], fn hi' => to64 (hi', zero, k)),
	      (* else *)
	      pure_arith32(P.NOTB, [hi], fn hi' =>
	        pure_arith32(P.NEG, [lo], fn lo' =>
		  to64 (hi', lo', k))))))

    val i64Eql = w64Eql

  (* the basic pattern for comparisons is
   *   fun cmp ((hi1, lo1), (hi2, lo2)) =
   *         OP(hi1, hi2) orelse ((hi1 = hi2) andalso OP(lo1, lo2))
   *)
    local
      fun i64Cmp oper (n1, n2, tr, fl) = let
	  (* continuations for the branches so that we can avoid code duplication *)
	    val trFnId = LV.mkLvar()
	    val tr' = C.APP(C.VAR trFnId, [])
	    val flFnId = LV.mkLvar()
	    val fl' = C.APP(C.VAR flFnId, [])
	    in
	      C.FIX([(C.CONT, trFnId, [], [], tr), (C.CONT, flFnId, [], [], fl)],
	      (* (hi1 < hi2) orelse ((hi1 = hi2) andalso (lo1 < lo2)) *)
		getHi32(n1, fn hi1 =>
		getHi32(n2, fn hi2 =>
		  sIf(oper, hi1, hi2,
		    tr',
		    sIf(P.EQL, hi1, hi2,
		      getLo32(n1, fn lo1 =>
		      getLo32(n2, fn lo2 =>
			sIf(oper, lo1, lo2, tr', fl'))),
		      fl')))))
	    end
    in
    val i64Less = i64Cmp P.LT
    val i64LessEq = i64Cmp P.LTE
    val i64Greater = i64Cmp P.GT
    val i64GreaterEq = i64Cmp P.GTE
    end (* local *)

  (***** conversions *****)

(*
P.TRUNC(64, 31)
P.EXTEND(31, 64)
P.COPY(31, 64)
P.TRUNC(64, 32)
P.COPY(32, 64)
P.EXTEND(32, 64)
*)
  (***** main function *****)

  (* check if an expression needs rewriting *)
    fun needsRewrite func = let
	  fun chkValue (C.NUM{ival, ty={sz=64, ...}}) = true
	    | chkValue _ = false
	  fun chkExp (C.RECORD(_, vs, _, e)) =
		List.exists (chkValue o #1) vs orelse chkExp e
	    | chkExp (C.SELECT(_, v, _, _, e)) = chkValue v orelse chkExp e
	    | chkExp (C.OFFSET(_, v, _, e)) = chkValue v orelse chkExp e
	    | chkExp (C.APP(_, vs)) = List.exists chkValue vs
	    | chkExp (C.FIX(fns, e)) = List.exists chkFun fns orelse chkExp e
	    | chkExp (C.SWITCH(v, _, es)) = chkValue v orelse List.exists chkExp es
	    | chkExp (C.BRANCH(P.CMP{kind=P.INT 64, ...}, _, _, _, _)) = true
	    | chkExp (C.BRANCH(P.CMP{kind=P.UINT 64, ...}, _, _, _, _)) = true
	    | chkExp (C.BRANCH(_, _, _, e1, e2)) = chkExp e1 orelse chkExp e2
	    | chkExp (C.SETTER(_, vs, e)) = List.exists chkValue vs orelse chkExp e
	    | chkExp (C.LOOKER(_, _, _, _, e)) = chkExp e
	    | chkExp (C.ARITH(P.IARITH{sz=64, ...}, _, _, _, _)) = true
	    | chkExp (C.ARITH(_, _, _, _, e)) = chkExp e
	    | chkExp (C.PURE(P.PURE_ARITH{kind=P.UINT 64, ...}, _, _, _, _)) = true
	    | chkExp (C.PURE(_, _, _, _, e)) = chkExp e
	    | chkExp (C.RCC(_, _, _, vs, _, e)) = List.exists chkValue vs orelse chkExp e
(* QUESTION: do we need to check the tys?
 * For example `(fn (x : Int64.int) => x)` might take two arguments for `x`!
 *)
	  and chkFun (_, _, _, _, e) = chkExp e
	  in
	    (not Target.is64) andalso (chkFun func)
	  end

    fun elim cfun = let
	  fun value (C.NUM{ival, ty={sz=64, ...}}, k) = let
		val (hi, lo) = split ival
		in
		  to64 (hi, lo, k)
		end
	    | value (v, k) = k v
	  and values (vl, k) = let
		fun f ([], vl') = k (List.rev vl')
		  | f (C.NUM{ival, ty={sz=64, ...}}::vs, vl') = let
		      val (hi, lo) = split ival
		      in
			to64 (hi, lo, fn v => f (vs, v::vl'))
		      end
		  | f (v::vs, vl') = f (vs, v::vl')
		in
		  f (vl, [])
		end
	  fun cexp (C.RECORD (rk, xl, v, e)) = let
(* QUESTION: can we use the values function here? *)
		fun f ([], args') = C.RECORD (rk, List.rev args', v, cexp e)
		  | f ((C.NUM{ival, ty={sz=64, ...}}, offp)::args, args') = let
		      val (hi, lo) = split ival
		      in
			to64 (hi, lo, fn v => f (args, (v, offp)::args'))
		      end
		  | f (arg::args, args') = f (args, arg::args')
		in
		  f (xl, [])
		end
	    | cexp (C.SELECT(i, x, v, t, e)) = C.SELECT(i, x, v, t, cexp e)
	    | cexp (C.OFFSET(i, v, x, e)) = C.OFFSET(i, v, x, cexp e)
	    | cexp (C.APP(f, xl)) = values (xl, fn xl' => C.APP (f, xl'))
	    | cexp (C.FIX(fl, e)) = C.FIX(List.map function fl, cexp e)
	    | cexp (C.SWITCH(x, v, el)) =
		value (x, fn x' => C.SWITCH(x', v, List.map cexp el))
	    | cexp (C.BRANCH(P.CMP{oper, kind=P.INT 64}, args, _, e1, e2)) =
		values (args, fn args' => (case (oper, args')
		   of (P.GT, [a, b]) => i64Greater(a, b, cexp e1, cexp e2)
		    | (P.GTE, [a, b]) => i64GreaterEq(a, b, cexp e1, cexp e2)
		    | (P.LT, [a, b]) => i64Less(a, b, cexp e1, cexp e2)
		    | (P.LTE, [a, b]) => i64LessEq(a, b, cexp e1, cexp e2)
		    | (P.EQL, [a, b]) => i64Eql(a, b, cexp e1, cexp e2)
		    | (P.NEQ, [a, b]) => i64Eql(a, b, cexp e2, cexp e1)
		    | _ => bug "impossible BRANCH; INT 64"
		  (* end case *)))
	    | cexp (C.BRANCH(P.CMP{oper, kind=P.UINT 64}, args, _, e1, e2)) =
		values (args, fn args' => (case (oper, args')
		   of (P.GT, [a, b]) => w64Greater(a, b, cexp e1, cexp e2)
		    | (P.GTE, [a, b]) => w64GreaterEq(a, b, cexp e1, cexp e2)
		    | (P.LT, [a, b]) => w64Less(a, b, cexp e1, cexp e2)
		    | (P.LTE, [a, b]) => w64LessEq(a, b, cexp e1, cexp e2)
		    | (P.EQL, [a, b]) => w64Eql(a, b, cexp e1, cexp e2)
		    | (P.NEQ, [a, b]) => w64Eql(a, b, cexp e2, cexp e1)
		    | _ => bug "impossible BRANCH; UINT 64"
		  (* end case *)))
	    | cexp (C.BRANCH(rator, args, v, e1, e2)) =
		cexp (C.BRANCH(rator, args, v, cexp e1, cexp e2))
	    | cexp (C.SETTER(rator, xl, e)) =
		values (xl, fn xl' => C.SETTER (rator, xl', cexp e))
	    | cexp (C.LOOKER (rator, xl, v, ty, e)) =
		values (xl, fn xl' => C.LOOKER (rator, xl', v, ty, cexp e))
	    | cexp (C.ARITH(P.IARITH{oper, sz=64}, args, res, _, e)) =
		values (args, fn args' => (case (oper, args')
		   of (P.IADD, [a, b]) => i64Add(a, b, res, cexp e)
		    | (P.ISUB, [a, b]) => i64Sub(a, b, res, cexp e)
		    | (P.IMUL, [a, b, f]) => mkApply(f, [a, b], res, e)
		    | (P.IDIV, [a, b, f]) => mkApply(f, [a, b], res, e)
		    | (P.IMOD, [a, b, f]) => mkApply(f, [a, b], res, e)
		    | (P.IQUOT, [a, b, f]) => mkApply(f, [a, b], res, e)
		    | (P.IREM , [a, b, f]) => mkApply(f, [a, b], res, e)
		    | (P.INEG, [a]) => i64Neg(a, res, e)
		    | _ => bug "impossible IARITH; sz=64"
		  (* end case *)))
	    | cexp (C.ARITH(rator, args, res, ty, e)) =
		C.ARITH(rator, args, res, ty, cexp e)
	    | cexp (C.PURE(P.PURE_ARITH{oper, kind=P.UINT 64}, args, res, _, e)) =
		values (args, fn args' => (case (oper, args')
		   of (P.ADD, [a, b]) => w64Add(a, b, res, cexp e)
		    | (P.SUB, [a, b]) => w64Sub(a, b, res, cexp e)
		    | (P.MUL, [a, b, f]) => mkApply(f, [a, b], res, e)
		    | (P.QUOT, [a, b, f]) => mkApply(f, [a, b], res, e)
		    | (P.REM , [a, b, f]) => mkApply(f, [a, b], res, e)
		    | (P.NEG, [a]) => i64Neg(a, res, e)
		    | (P.ORB, [a, b]) => w64Orb(a, b, res, cexp e)
		    | (P.XORB, [a, b]) => w64Xorb(a, b, res, cexp e)
		    | (P.ANDB, [a, b]) => w64Andb(a, b, res, cexp e)
		    | (P.NOTB, [a, b]) => w64Notb(a, res, e)
		    | (P.RSHIFT, [a, b]) => w64RShift(a, b, res, cexp e)
		    | (P.RSHIFTL, [a, b]) => w64RShiftL(a, b, res, cexp e)
		    | (P.LSHIFT, [a, b]) => w64LShift(a, b, res, cexp e)
		    | _ => bug "impossible PURE_ARITH; UINT 64"
		  (* end case *)))
	    | cexp (C.PURE(rator, args, res, ty, e)) =
		C.PURE(rator, args, res, ty, cexp e)
	    | cexp (C.RCC(rk, cf, proto, args, res, e)) =
		C.RCC(rk, cf, proto, args, res, cexp e)
	(* make an application of the function `f`, where `exp` is the continuation
	 * of the original primop and we assume the result type is a pair of
	 * 32-bit integers.
	 *)
	  and mkApply (f, args, res, exp) = let
		val k = LV.mkLvar()
		in
		  C.FIX([(C.CONT, k, [res], [pairTy], cexp exp)],
		    values (args, fn args' => C.APP(f, C.VAR k :: args')))
		end
	  and function (fk, f, params, tys, body) =
(* QUESTION: do we need to convert the tys? *)
		(fk, f, params, tys, cexp body)
	  in
	    if needsRewrite cfun
	      then function cfun
	      else cfun
	  end (* elim *)

  end
