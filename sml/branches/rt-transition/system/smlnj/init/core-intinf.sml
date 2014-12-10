(*
 * Basic IntInf functionality for translating certain primops and
 * IntInf.int literals within the compiler.
 *
 *   (C) 2003 The SML/NJ Fellowship.
 *
 * Author: Matthias Blume (blume@tti-c.org)
 *)
structure CoreIntInf :> sig

    (* We use a 30-bit representation, stored in 31-bit words for digits.
     * This way we avoid the extra boxing that would come with 32-bit values
     * and also have the benefit of an extra bit that can be used to store
     * carries. *)
    datatype rep = BI of { negative : bool, digits : word list }

    (* This is the abstract built-in type "intinf".  It comes with no
     * operations of its own.  We use casts to go back and forth between
     * the representation type "rep" and the abstract type "intinf". *)
    type intinf = PrimTypes.intinf

    (* Here are the "cast" operations: *)
    val abstract : rep    -> intinf
    val concrete : intinf -> rep

    (* the number of bits in one "big digit" *)
    val baseBits : word

    (* the actual base: *)
    val base : word

    (* maximum value of a "big digit": *)
    val maxDigit : word

    (* The following three functions are copied into structure _Core
     * from where the compiler's "translate" phase will pick them to
     * implement precision-extending and precision-shrinking conversions
     * that involve intinf.  We only provide operations between
     * int32 and intinf.  For precisions less than 32 bits the compiler
     * must insert an additional conversion: *)

    (* fit value (2's complement) in int32, raise Overflow if too large *)
    val testInf   : intinf -> int32
    (* truncate value (2's complement repr) to fit in int32: *)
    val truncInf  : intinf -> int32
    (* copy bits from int32 into (non-negative) intinf: *)
    val copyInf   : int32 -> intinf
    (* sign-extend int32: *)
    val extendInf : int32 -> intinf
    (* combined (sign-extension takes place when second argument is true): *)
    val finToInf  : int32 * bool -> intinf

    (* fit value (2's complement) in "int64", raise Overflow if too large *)
    val testInf64   : intinf -> word32 * word32
    (* truncate value (2's complement repr) to fit in "int64": *)
    val truncInf64  : intinf -> word32 * word32
    (* copy bits from "int64" into (non-negative) intinf: *)
    val copyInf64   : word32 * word32 -> intinf
    (* sign-extend "int64": *)
    val extendInf64 : word32 * word32 -> intinf
    (* combined (sign-extension takes place when second argument is true): *)
    val finToInf64  : word32 * word32 * bool -> intinf

    (* These two directly take the list of digits
     * to be used by the internal representation: *)
    val makeNegInf : word list -> intinf
    val makePosInf : word list -> intinf

    (* In the common case where only one big digit is involved, use
     * a shortcut without list allocation: *)
    val makeSmallNegInf : word -> intinf
    val makeSmallPosInf : word -> intinf 

    (* For ~base < i < base we have lowValue i = i.
     * For other i we have lowValue i = ~base (= neg_base_as_int).
     * This can be used to implement faster pattern-match code for
     * the common case that the pattern consists of small values. *)
    val lowValue : intinf -> int
    val neg_base_as_int : int

  (* add one to a list of digits *)
    val natinc : word list -> word list

    (* Various primitive operations for use by the pervasive environment,
     * plus stuff that we have to implement here anyway, so the
     * real structure IntInf can pick them up: *)
    val ~ : intinf -> intinf
    val + : intinf * intinf -> intinf
    val - : intinf * intinf -> intinf
    val * : intinf * intinf -> intinf
    val div : intinf * intinf -> intinf
    val mod : intinf * intinf -> intinf
    val quot : intinf * intinf -> intinf
    val rem : intinf * intinf -> intinf
    val < : intinf * intinf -> bool
    val <= : intinf * intinf -> bool
    val > : intinf * intinf -> bool
    val >= : intinf * intinf -> bool
    val compare : intinf * intinf -> Order.order
    val abs : intinf -> intinf
    val pow : intinf * int -> intinf

    val divMod : intinf * intinf -> intinf * intinf
    val quotRem : intinf * intinf -> intinf * intinf

    (* support for scanning and formatting: *)
    val natdivmodd : word list * word -> word list * word
    val natmadd : word * word list * word -> word list

end = struct

    infixr 5 ::
    val not : bool -> bool = InLine.inlnot

    val w31ToI32 : word -> int32  = InLine.copy_31_32_i
    val w32ToI32 : word32 -> int32 = InLine.copy_32_32_wi
    val w31ToW32 : word -> word32 = InLine.copy_31_32_w
    val i32ToW31 : int32 -> word = InLine.trunc_32_31_i
    val i32ToW32 : int32 -> word32 = InLine.copy_32_32_iw
    val w32ToW31 : word32 -> word = InLine.trunc_32_31_w
    val ~ : int32 -> int32 = InLine.i32neg
    infix || && >> ^ <<
    val op || : word32 * word32 -> word32 = InLine.w32orb
    val op ^ : word32 * word32 -> word32 = InLine.w32xorb
    val op && : word32 * word32 -> word32 = InLine.w32andb
    val op >> : word32 * word -> word32 = InLine.w32rshiftl
    val op << : word32 * word -> word32 = InLine.w32lshift

    (* ******************* *)
    type intinf = PrimTypes.intinf

    (* assuming 30-bit digits (must match actual implementation),
     * least significant digit first,
     * normalized (last digit <> 0) *)

    datatype rep = BI of { negative : bool, digits : word list }

    fun abstract (r : rep) : intinf = InLine.cast r
    fun concrete (i : intinf) : rep = InLine.cast i

    val hBaseBits : word = 0w15
    val baseBits : word = InLine.w31lshift (hBaseBits, 0w1)
    val maxDigit : word = 0wx3fffffff
    val maxDigit32 = w31ToW32 maxDigit
    val maxDigitL : word = 0wx7fff	(* lower half of maxDigit *)
    val maxDigitL32 = w31ToW32 maxDigitL
    val base : word = 0wx40000000
    val base32 = w31ToW32 base
    val neg_base_as_int : int = ~0x40000000

    val gap : word = InLine.w31sub (0w32, baseBits) (* 32 - baseBits *)
    val slc : word = InLine.w31sub (baseBits, gap)  (* baseBits - gap *)

    fun neg64 (hi, 0w0) : word32 * word32 = (InLine.w32neg hi, 0w0)
      | neg64 (hi, lo) = (InLine.w32notb hi, InLine.w32neg lo)
	      
    fun testInf i =
	let val BI { negative, digits } = concrete i
	    fun negif i32 = if negative then ~i32 else i32
	in case digits of
	       [] => 0
	     | [d] => negif (w31ToI32 d)
	     | [d0, 0w1] => negif (w32ToI32 (w31ToW32 d0 || base32))
	     | [0w0, 0w2] => if negative then ~0x80000000
			     else raise Assembly.Overflow
	     | _ => raise Assembly.Overflow
	end

    fun testInf64 i =
	let val BI { negative, digits } = concrete i
	    fun negif hilo = if negative then neg64 hilo else hilo
	in case digits of
	       [] => (0w0, 0w0)
	     | [d] => negif (0w0, w31ToW32 d)
	     | [d0, d1] =>
	       let val (w0, w1) = (w31ToW32 d0, w31ToW32 d1)
	       in negif (w1 >> gap, w0 || (w1 << baseBits))
	       end
	     | [0w0, 0w0, 0w8] =>
	         if negative then (0wx80000000, 0w0)
		 else raise Assembly.Overflow
	     | [d0, d1, d2] =>
	         if InLine.w31ge (d2, 0w8) then raise Assembly.Overflow
		 else let val (w0, w1, w2) =
			      (w31ToW32 d0, w31ToW32 d1, w31ToW32 d2)
		      in negif ((w1 >> gap) || (w2 << slc),
				w0 || (w1 << baseBits))
		      end
	     | _ => raise Assembly.Overflow
	end

    fun truncInf i =
	let val BI { negative, digits } = concrete i
	    val b =
		case digits of
		    [] => 0w0
		  | [d] => w31ToW32 d
		  | d0 :: d1 :: _ => w31ToW32 d0 || (w31ToW32 d1 << baseBits)
	in w32ToI32 (if negative then InLine.w32neg b else b)
	end

    fun truncInf64 i =
	let val BI { negative, digits } = concrete i
	    val hilo =
		case digits of
		    [] => (0w0, 0w0)
		  | [d0] => (0w0, w31ToW32 d0)
		  | [d0, d1] =>
		    let val (w0, w1) = (w31ToW32 d0, w31ToW32 d1)
		    in (w1 >> gap, w0 || (w1 << baseBits))
		    end
		  | d0 :: d1 :: d2 :: _ =>
		    let val (w0, w1, w2) =
			    (w31ToW32 d0, w31ToW32 d1, w31ToW32 d2)
		    in ((w1 >> gap) || (w2 << slc), w0 || (w1 << baseBits))
		    end
	in if negative then neg64 hilo else hilo
	end

    fun extendInf i32 = let
	fun e (_, 0w0) = BI { negative = false, digits = [] }
	  | e (negative, w31) =
	      BI { negative = negative,
		   digits = if InLine.w31ge (w31, base) then
				[InLine.w31sub (w31, base), 0w1]
			    else [w31] }
    in
	abstract (if InLine.i32eq (i32, ~0x80000000) then
		      BI { negative = true, digits = [0w0, 0w2 ] }
		  else if InLine.i32lt (i32, 0) then e (true, i32ToW31 (~i32))
		  else e (false, i32ToW31 i32))
    end

    fun copyInf64' (_, (0w0, 0w0)) = abstract (BI { negative = false,
						 digits = [] })
      | copyInf64' (negative, (hi, lo)) = 
	let infix <> 
            val op <> : word * word -> bool = InLine.w31ne
	    val d0 = w32ToW31 (lo && 0wx3fffffff)
	    val d1 = w32ToW31 (((hi && 0wxfffffff) << 0w2) || (lo >> 0w30))
	    val d2 = w32ToW31 (hi >> 0w28)
	in abstract (BI { negative = negative,
			  digits = if d2 <> 0w0 then [d0, d1, d2]
				   else if d1 <> 0w0 then [d0, d1]
				   else if d0 <> 0w0 then [d0]
				   else [] })
	end

    fun copyInf64 hilo = copyInf64' (false, hilo)

    fun extendInf64 (hi, lo) =
	if InLine.w32ne (hi && 0wx80000000, 0w0) then
	    copyInf64' (true, neg64 (hi, lo))
	else copyInf64' (false, (hi, lo))

    fun copyInf i32 =
	let val w32 = i32ToW32 i32
	    val digits =
		if InLine.w32eq (w32, 0w0) then []
		else if InLine.w32ge (w32, base32) then
		    [w32ToW31 (w32 && maxDigit32), w32ToW31 (w32 >> baseBits)]
		else [w32ToW31 w32]
	in abstract (BI { negative = false, digits = digits })
	end

    fun finToInf (i32, extend) =
	if extend then extendInf i32 else copyInf i32

    fun finToInf64 (hi, lo, extend) =
	if extend then extendInf64 (hi, lo) else copyInf64 (hi, lo)

    fun makeInf negative digits =
	abstract (BI { negative = negative, digits = digits })
    val makeNegInf = makeInf true
    val makePosInf = makeInf false

    fun makeSmallInf _ 0w0 =
	  abstract (BI { negative = false, digits = [] })
      | makeSmallInf negative digit =
	  abstract (BI { negative = negative, digits = [digit] })
    val makeSmallNegInf = makeSmallInf true
    val makeSmallPosInf = makeSmallInf false

    fun lowValue i =
	case concrete i of
	    BI { digits = [], ... } => 0
	  | BI { digits = [d], negative = false } =>
	      InLine.copy_31_31_wi d
	  | BI { digits = [d], negative = true } =>
	      InLine.i31neg (InLine.copy_31_31_wi d)
	  | _ => neg_base_as_int

    (* concrete->abstract wrappers for unary and binary functions *)
    fun fabs1 f x = abstract (f (concrete x))
    fun fabs2 f (x, y) = abstract (f (concrete x, concrete y))
    fun fabs2c f (x, y) = f (concrete x, concrete y)
    fun fabs22 f (x, y) = let
	val (a, b) = f (concrete x, concrete y)
    in
	(abstract a, abstract b)
    end

    (* like BI, but make sure that digits = [] implies not negative *)
    fun bi { digits = [], ... } = BI { digits = [], negative = false }
      | bi arg = BI arg

    fun abs' (BI { negative, digits }) =
	BI { negative = false, digits = digits }

    fun neg (BI { digits, negative }) =
	bi { digits = digits, negative = not negative }

    open Order
	     
    fun dcmp (x, y) =
	if InLine.w31lt (x, y) then LESS else
	if InLine.w31gt (x, y) then GREATER else
	EQUAL

    fun natcmp ([], []) = EQUAL
      | natcmp ([], _) = LESS
      | natcmp (_, []) = GREATER
      | natcmp (x :: xs, y :: ys) =
	  (case natcmp (xs, ys) of
	       EQUAL => dcmp (x, y)
	     | unequal => unequal)

    fun lt (BI { negative = false, ... }, BI { negative = true, ... }) = false
      | lt (BI { negative = true, ... }, BI {negative = false, ...}) = true
      | lt (BI { negative, digits = d1 }, BI { digits = d2, ... }) =
	  InLine.= (natcmp (d1, d2), if negative then GREATER else LESS)

    fun gt (x, y) = lt (y, x)
    fun ge (x, y) = not (lt (x, y))
    fun le (x, y) = not (gt (x, y))

    fun adddig (d1, d2) : {carry: bool, res: word} =
        let val sum = InLine.w31add (d1, d2)
        in
	    {carry = InLine.w31ge (sum, base),
	     res = InLine.w31andb (sum, maxDigit) }
        end

    (* add one to nat *)
    fun natinc [] = [0w1]
      | natinc (x :: xs) =
	  if InLine.w31eq (x, maxDigit) then 0w0 :: natinc xs
	  else InLine.w31add (x, 0w1) :: xs

    fun natdec (0w0 :: xs) = maxDigit :: natdec xs
      | natdec [0w1] = []
      | natdec (x :: xs) = InLine.w31sub (x, 0w1) :: xs
      | natdec [] = raise Assembly.Overflow (* should never happen! *)

    (* add two nats plus 1 (carry) *)
    fun natadd1 (x, []) = natinc x
      | natadd1 ([], y) = natinc y
      | natadd1 (x :: xs, y :: ys) = let
	    val { carry, res } = adddig (x, y)
	    val (carry, res) =
		if InLine.w31eq (res, maxDigit) then (true, 0w0)
		else (carry, InLine.w31add (res, 0w1))
	in
	    res :: natadd01 (carry, xs, ys)
	end

    (* add two nats *)
    and natadd0 (x, []) = x
      | natadd0 ([], y) = y
      | natadd0 (x :: xs, y :: ys) = let
	    val { carry, res } = adddig (x, y)
	in
	    res :: natadd01 (carry, xs, ys)
	end

    (* add two nats with optional carry *)
    and natadd01 (carry, xs, ys) =
	if carry then natadd1 (xs, ys) else natadd0 (xs, ys)

    exception Negative

    (* natsub hopes that xs >= ys + carry, raises Negative otherwise *)
    fun natsub (xs, [], false) = xs
      | natsub (xs, [], true) = natsub (xs, [0w0], true)
      | natsub ([], _, _) = raise Negative
      | natsub (x :: xs, y :: ys, c) = let
	    val y' = if c then InLine.w31add (y, 0w1) else y
	    val (res, carry) =
		if InLine.w31lt (x, y') then
		    (InLine.w31sub (InLine.w31add (x, base), y'), true)
		else (InLine.w31sub (x, y'), false)
	in
	    case natsub (xs, ys, carry) of
		[] => if InLine.w31eq (res, 0w0) then [] else [res]
	      | more => res :: more
	end

    fun natsub0 (xs, ys) = natsub (xs, ys, false)

    fun sub0 (xs, ys) =
	BI { negative = false, digits = natsub0 (xs, ys) }
	handle Negative => bi { negative = true, digits = natsub0 (ys, xs) }

    fun add0 (false, xs1, false, xs2) =
	  BI { negative = false, digits = natadd0 (xs1, xs2) }
      | add0 (true, xs1, true, xs2) =
	  BI { negative = true, digits = natadd0 (xs1, xs2) }
      | add0 (false, xs1, true, xs2) =
	  sub0 (xs1, xs2)
      | add0 (true, xs1, false, xs2) =
	  sub0 (xs2, xs1)

    fun add (BI b1, BI b2) =
	add0 (#negative b1, #digits b1, #negative b2, #digits b2)

    fun sub (BI b1, BI b2) =
	add0 (#negative b1, #digits b1, not (#negative b2), #digits b2)

    fun compare' (BI { negative = true, ... }, BI { negative = false, ... }) =
	  LESS
      | compare' (BI { negative = false, ... }, BI { negative = true, ... }) =
	  GREATER
      | compare' (BI { negative, digits = d1 }, BI { digits = d2, ... }) =
	  if negative then natcmp (d2, d1) else natcmp (d1, d2)

    fun ddmul (x, y) = let
	fun high w32 = w32 >> hBaseBits
	fun low w32 = w32 && maxDigitL32
	fun hl w32 = (high w32, low w32)
	val (xh, xl) = hl (w31ToW32 x)
	val (yh, yl) = hl (w31ToW32 y)
	val a = InLine.w32mul (xh, yh)
	val c = InLine.w32mul (xl, yl)
	(* b = b' - a - c = xh * yl + xl * yh *)
	val b' = InLine.w32mul (InLine.w32add (xh, xl), InLine.w32add (yh, yl))
	val b = InLine.w32sub (b', InLine.w32add (a, c))
	val (bh, bl) = hl b
	val l0 = InLine.w32add (c, InLine.w32lshift (bl, hBaseBits))
	val l32 = l0 && maxDigit32
	val lc = l0 >> baseBits
	val h32 = InLine.w32add (InLine.w32add (a, bh), lc)
    in
	(w32ToW31 h32, w32ToW31 l32)
    end

    fun natmadd (0w0, _, 0w0) = []
      | natmadd (0w0, _, c) = [c]
      | natmadd (_, [], 0w0) = []
      | natmadd (_, [], c) = [c]
      | natmadd (0w1, xs, 0w0) = xs
      | natmadd (0w1, xs, c) = natadd0 (xs, [c])
      | natmadd (w, x :: xs, c) = let
	    val (h, l) = ddmul (w, x)
	    val { carry,  res = l' } = adddig (l, c)
	    val h' = if carry then InLine.w31add (h, 0w1) else h
	in
	    l' :: natmadd (w, xs, h')
	end

    fun natmul ([], _) = []
      | natmul (_, []) = []
      | natmul (xs, [0w1]) = xs
      | natmul ([0w1], ys) = ys
      | natmul (x :: xs, ys) =
	  natadd0 (natmadd (x, ys, 0w0), 0w0 :: natmul (xs, ys))

    fun mul (BI x, BI y) =
	bi { negative = InLine.<> (#negative x, #negative y),
	     digits = natmul (#digits x, #digits y) }

    val one = BI { negative = false, digits = [0w1] }
    val zero = BI { negative = false, digits = [] }
    fun posi digits = BI { digits = digits, negative = false }
    fun negi digits = BI { digits = digits, negative = true }
    fun zneg digits = bi { digits = digits, negative = true }

    fun consd (0w0, []) = []
      | consd (x, xs) = x :: xs

    fun scale w = InLine.w31div (base, InLine.w31add (w, 0w1))

    (* returns length-1 and last element *)
    fun length'n'last [] = (0, 0w0)	(* should not happen *)
      | length'n'last [x] = (0, x)
      | length'n'last (_ :: l) = let
	    val (len, last) = length'n'last l
	in
	    (InLine.i31add (len, 1), last)
	end

    fun nth (_, []) = 0w0
      | nth (0, x :: _) = x
      | nth (n, _ :: xs) = nth (InLine.i31sub (n, 1), xs)

    (* divide DP number by digit; assumes u < i , i >= base/2 *)
    fun natdivmod2 ((u,v), i) =
	let fun low w = InLine.w31andb (w, maxDigitL)
	    fun high w = InLine.w31rshiftl (w, hBaseBits)
	    val (vh, vl) = (high v, low v)
	    val (ih, il) = (high i, low i)

	    fun adj (q, r, vx) =
		let val x = InLine.w31add (InLine.w31lshift (r, hBaseBits), vx)
		    val y = InLine.w31mul (q, il)
		    fun loop (q, x) =
			if InLine.w31ge (x, y) then (q, InLine.w31sub (x, y))
			else loop (InLine.w31sub (q, 0w1), InLine.w31add (x, i))
		in loop (q, x)
		end

	    val q1 = InLine.w31div (u, ih)
	    val r1 = InLine.w31mod (u, ih)
	    val (q1, r1) = adj (q1, r1, vh)
	    val q0 = InLine.w31div (r1, ih)
	    val r0 = InLine.w31mod (r1, ih)
	    val (q0, r0) = adj (q0, r0, vl)
	in (InLine.w31add (InLine.w31lshift (q1, hBaseBits), q0), r0)
	end

    (* divide bignat by digit>0 *)
    fun natdivmodd (m, 0w1) = (m, 0w0) (* speedup *)
      | natdivmodd (m, i) = let
            val scale = scale i
            val i' = InLine.w31mul (i, scale)
            val m' = natmadd (scale, m, 0w0)
            fun dmi [] = ([], 0w0)
              | dmi (d::r) = let 
                    val (qt,rm) = dmi r
                    val (q1,r1) = natdivmod2 ((rm,d), i')
                in
		    (consd (q1,qt), r1)
		end
            val (q,r) = dmi m'
        in
	    (q, InLine.w31div (r, scale))
	end

    (* From Knuth Vol II, 4.3.1, but without opt. in step D3 *)
    fun natdivmod (m, []) = raise Assembly.Div
      | natdivmod ([], n) = ([], []) (* speedup *)
      | natdivmod (d::r, 0w0::s) = let 
            val (qt,rm) = natdivmod (r,s)
        in (qt, consd (d, rm)) end (* speedup *)
      | natdivmod (m, [d]) = let 
            val (qt, rm) = natdivmodd (m, d)
        in (qt, if InLine.w31eq (rm, 0w0) then [] else [rm]) end
      | natdivmod (m, n) = let
	    val (ln, last) = length'n'last n (* ln >= 1 *)
	    val scale = scale last
            val m' = natmadd (scale, m, 0w0)
            val n' = natmadd (scale, n, 0w0)
            val n1 = nth (ln, n')	(* >= base/2 *)
            fun divl [] = ([], [])
              | divl (d::r) = let
                    val (qt,rm) = divl r
                    val m = consd (d, rm)
                    fun msds ([],_) = (0w0,0w0)
                      | msds ([d],0) = (0w0,d)
                      | msds ([d2,d1],0) = (d1,d2)
                      | msds (d::r,i) = msds (r,InLine.i31sub (i, 1))
                    val (m1,m2) = msds (m, ln)
                    val tq = if InLine.= (m1, n1) then maxDigit
                             else #1 (natdivmod2 ((m1,m2), n1))
                    fun try (q,qn') = (q, natsub0 (m,qn'))
                	handle Negative => try (InLine.w31sub (q,0w1),
						natsub0 (qn', n'))
                    val (q,rr) = try (tq, natmadd (tq,n',0w0))
                in (consd (q,qt), rr) end
            val (qt,rm') = divl m'
            val (rm,_(*0*)) = natdivmodd (rm',scale)
        in
	    (qt,rm)
	end

    fun quotRem' (_, BI { digits = [], ... }) = raise Assembly.Div
      | quotRem' (BI { digits = [], ... }, _) = (zero, zero)
      | quotRem' (BI x, BI y) = let
	    val (q, r) = natdivmod (#digits x, #digits y)
	in
	    (if InLine.<> (#negative x, #negative y) then zneg q else posi q,
	     if #negative x then zneg r else posi r)
	end

    fun divMod' (_, BI { digits = [], ... }) = raise Assembly.Div
      | divMod' (BI { digits = [], ... }, _) = (zero, zero)
      | divMod' (BI x, BI y) =
	  if InLine.= (#negative x, #negative y) then
	      let val (q, r) = natdivmod (#digits x, #digits y)
	      in
		  (posi q, if #negative x then zneg r else posi r)
	      end
	  else
	      let val (m, n) = (#digits x, #digits y)
		  val (q, r) = natdivmod (natdec m, n)
		  val mdd = natsub (n, r, true)
	      in
		  (negi (natinc q),
		   if #negative x then posi mdd else zneg mdd)
	      end

    fun div' arg = #1 (divMod' arg)
    fun quot' arg = #1 (quotRem' arg)

    (* For div and mod we special-case a divisor of 2 (common even-odd test) *)
    fun mod' (BI { digits = [], ... }, _) = zero
      | mod' (BI { digits = low :: _, ... },
	      BI { digits = [0w2], negative }) =
	  if InLine.w31eq (InLine.w31andb (low, 0w1), 0w0) then zero
	  else BI { digits = [0w1], negative = negative }
      | mod' arg = #2 (divMod' arg)

    fun rem' (BI { digits = [], ... }, _) = zero
      | rem' (BI { digits = low :: _, negative },
	      BI { digits = [0w2], ... }) =
	  if InLine.w31eq (InLine.w31andb (low, 0w1), 0w0) then zero
	  else BI { digits = [0w1], negative = negative }
      | rem' arg = #2 (quotRem' arg)

    fun natpow (_, 0) = [0w1]
      | natpow ([], n) = if InLine.i31lt (n, 0) then raise Assembly.Div else []
      | natpow (x, n) =
	  if InLine.i31lt (n, 0) then []
	  else let fun exp (m, 0w0) = [0w1]
		     | exp (m, 0w1) = m
		     | exp (m, n) = let
			   val x = exp (m, InLine.w31rshiftl (n, 0w1))
			   val y = natmul (x, x)
		       in
			   if InLine.w31eq (InLine.w31andb (n, 0w1), 0w0)
                           then y
			   else natmul (y, m)
		       end
	       in
		   exp (x, InLine.copy_31_31_iw n)
	       end

    fun pow (_, 0) = abstract (BI { negative = false, digits = [0w1] })
      | pow (i, n) = let
	    val BI { negative, digits } = concrete i
	in
	    abstract (bi { negative = negative andalso
	                              InLine.i31eq (InLine.i31rem (n, 2), 1),
			   digits = natpow (digits, n) })
	end

    val ~ = fabs1 neg
    val - = fabs2 sub
    val + = fabs2 add
    val * = fabs2 mul
    val div = fabs2 div'
    val mod = fabs2 mod'
    val quot = fabs2 quot'
    val rem = fabs2 rem'
    val divMod = fabs22 divMod'
    val quotRem = fabs22 quotRem'
    val < = fabs2c lt
    val > = fabs2c gt
    val <= = fabs2c le
    val >= = fabs2c ge
    val abs = fabs1 abs'
    val compare = fabs2c compare'
end
