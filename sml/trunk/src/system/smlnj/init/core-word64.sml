structure CoreWord64 = struct

  local
      fun notyet () = raise Assembly.Overflow

      type w32 = PrimTypes.word32

      infix o val op o = InLine.compose
      val not = InLine.inlnot
      infix 7 * val op * = InLine.w32mul
      infix 6 + - val op + = InLine.w32add val op - = InLine.w32sub
      infix 5 << >> val op << = InLine.w32lshift val op >> = InLine.w32rshiftl
      infix 5 & val op & = InLine.w32andb
      infix 4 < val op < = InLine.w32lt

      fun lift1' f = f o InLine.w64p
      fun lift1 f = InLine.p64w o lift1' f
      fun lift2' f (x, y) = f (InLine.w64p x, InLine.w64p y)
      fun lift2 f = InLine.p64w o lift2' f

      fun split16 w32 = (w32 >> 0w16, w32 & 0wxffff)

      fun neg64 (hi, 0w0) = (InLine.w32neg hi, 0w0)
	| neg64 (hi, lo) = (InLine.w32notb hi, InLine.w32neg lo)
	      
      fun add64 ((hi1, lo1), (hi2, lo2)) =
	  let val (lo, hi) = (lo1 + lo2, hi1 + hi2)
	  in (if lo < lo1 then hi + 0w1 else hi, lo)
	  end

      fun sub64 ((hi1, lo1), (hi2, lo2)) =
	  let val (lo, hi) = (lo1 - lo2, hi1 - hi2)
	  in (if lo1 < lo then hi - 0w1 else hi, lo)
	  end

      fun mul64 ((hi1, lo1), (hi2, lo2)) =
	  let val ((a1, b1), (c1, d1)) = (split16 hi1, split16 lo1)
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
	  in (hi, lo)
	  end

      fun div64 (_, (0w0, 0w0)) = raise Assembly.Div
	| div64 ((0w0: w32, lo1), (0w0: w32, lo2)) = (0w0: w32, InLine.w32div (lo1, lo2))
	| div64 _ = notyet ()
	  
      fun mod64 (x, y) = sub64 (x, mul64 (div64 (x, y), y))

      fun swap (x, y) = (y, x)

      fun lt64 ((hi1, lo1), (hi2, lo2)) =
	  InLine.w32lt (hi1, hi2) orelse
	  (InLine.w32eq (hi1, hi2) andalso InLine.w32lt (lo1, lo2))
      val gt64 = lt64 o swap
      val le64 = not o gt64
      val ge64 = not o lt64
  in
      val extern = InLine.w64p
      val intern = InLine.p64w

      val ~ = lift1 neg64
      val op + = lift2 add64
      val op - = lift2 sub64
      val op * = lift2 mul64
      val div = lift2 div64
      val mod = lift2 mod64
      val op < = lift2' lt64
      val <= = lift2' le64
      val > = lift2' gt64
      val >= = lift2' ge64
  end
end
