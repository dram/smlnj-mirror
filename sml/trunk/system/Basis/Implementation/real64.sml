(* real64.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure Real64Imp : REAL =
  struct
    structure I = InlineT.Int

    structure Math = Math64

    infix 4 == !=
    type real = real

    fun *+(a:real,b,c) = a*b+c
    fun *-(a:real,b,c) = a*b-c

    val op == = InlineT.Real64.==
    val op != = InlineT.Real64.!=

    fun unordered(x:real,y) = Bool.not(x>y orelse x <= y)
    fun ?= (x, y) = (x == y) orelse unordered(x, y)

    val wordToReal = InlineT.Real64.from_word

    val rbase = wordToReal CoreIntInf.base
    val baseBits = InlineT.Word.toIntX CoreIntInf.baseBits

  (* maximum finite 64-bit real value *)
    val maxFinite = Real64Values.maxFinite
  (* minimum normalized positive real value *)
    val minNormalPos = Real64Values.minNormalPos
  (* minimum positive real value (denormalized) *)
    val minPos = Real64Values.minPos
  (* positive infinity *)
    val posInf = Real64Values.posInf
  (* negative infinity *)
    val negInf = Real64Values.negInf

  (* these functions are implemented in base/system/smlnj/init/pervasive.sml *)
    val floor = floor
    val ceil = ceil
    val trunc = trunc
    val round = round

  (* This is the IEEE double-precision maxint *)
    val maxInt = 4503599627370496.0

    local
    (* realround mode x returns x rounded to the nearest integer using the
     * given rounding mode.
     * May be applied to inf's and nan's.
     *)
      fun realround mode x = let
	    val saveMode = IEEEReal.getRoundingMode ()
	    in
	      IEEEReal.setRoundingMode mode;
	      (if x>=0.0 then x+maxInt-maxInt else x-maxInt+maxInt)
		before IEEEReal.setRoundingMode saveMode
	    end
    in
    val realFloor = realround IEEEReal.TO_NEGINF
    val realCeil = realround IEEEReal.TO_POSINF
    val realTrunc = realround IEEEReal.TO_ZERO
    val realRound = realround IEEEReal.TO_NEAREST
    end

    val abs : real -> real = InlineT.Real64.abs
    val fromInt : int -> real = InlineT.Real64.from_int

    fun toInt IEEEReal.TO_NEGINF = floor
      | toInt IEEEReal.TO_POSINF = ceil
      | toInt IEEEReal.TO_ZERO = trunc
      | toInt IEEEReal.TO_NEAREST = round

    fun toLarge x = x
    fun fromLarge _ x = x

    fun sign x = if (x < 0.0) then ~1
	  else if (x > 0.0) then 1
	  else if (x != x) then raise Domain
	  else 0
    val signBit : real -> bool = InlineT.Real64.signBit

    fun sameSign (x, y) = signBit x = signBit y

    fun copySign (x, y) = (* may not work if x is Nan *)
           if sameSign(x,y) then x else ~x

    fun compare (x, y) =
	  if x<y then General.LESS
	  else if x>y then General.GREATER
	  else if x == y then General.EQUAL
	  else raise IEEEReal.Unordered

    fun compareReal (x, y) =
	  if x<y then IEEEReal.LESS
	  else if x>y then IEEEReal.GREATER
	  else if x == y then IEEEReal.EQUAL
	  else IEEEReal.UNORDERED

  (* classification of IEEE reals *)
    local
      structure W = InlineT.Word
      structure W64 = InlineT.Word64
    in

    fun isFinite x = negInf < x andalso x < posInf
    fun isNan x = (x != x)

    fun isNormal x = let
	  val biasExp = W.andb(
		W.fromLarge(W64.rshiftl(InlineT.Real64.toBits x, 0w52)),
		0wx7ff)
	  in
	    (0w0 < biasExp) andalso (biasExp < 0w2047)
	  end

    fun class x = let (* does not distinguish between quiet and signalling NaN *)
	  val bits = InlineT.Real64.toBits x
	  in
	    if (W64.andb(0wx7fffffffffffffff, bits) = 0w0)
	      then IEEEReal.ZERO
	    else let
	      val signAndExp = W.fromLarge(W64.rshiftl(bits, 0w52))
	      in
		case W.andb(signAndExp, 0wx7ff)
		 of 0w0 => IEEEReal.SUBNORMAL
		  | 0w2047 => if (W64.andb(0wxfffffffffffff, bits) = 0w0)
		      then IEEEReal.INF
		      else IEEEReal.NAN IEEEReal.QUIET
		  | _ => IEEEReal.NORMAL
		(* end case *)
	      end
	  end

    fun toManExp x = let
	  val bits = InlineT.Real64.toBits x
	  in
	    if (W64.andb(0wx7fffffffffffffff, bits) = 0w0)
	      then {man = x, exp = 0}
	      else (case W.andb(W.fromLarge(W64.rshiftl(bits, 0w52)), 0wx7ff)
		 of 0w0 => let (* subnormal *)
		      val {man, exp} = toManExp(1048576.0 * x)
		      in
			{man = man, exp = exp - 20}
		      end
		  | 0w2047 => {man = x, exp = 0}	(* both NaNs and infinities *)
		  | biasExp => let
		      val exp = W.toIntX biasExp - 1023
		      in
			{man = Assembly.A.scalb(x, ~exp), exp=exp}
		      end
		(* end case *))
	  end

    end (* local *)

    val radix = 2
    val precision = 53			(* hidden bit gets counted, too *)

    val two_to_the_neg_1000 = let
          fun f(i,x) = if i=0 then x else f(i - 1, x*0.5)
	  in
	    f(1000, 1.0)
          end

    fun fromManExp {man=m, exp=e:int} =
	  if (m >= 0.5 andalso m <= 1.0  orelse m <= ~0.5 andalso m >= ~1.0)
	    then if e > 1020
	      then if e > 1050 then if m>0.0 then posInf else negInf
		   else let fun f(i,x) = if i=0 then x else f(i-1,x+x)
			   in f(e-1020,  Assembly.A.scalb(m,1020))
			  end
	      else if e < ~1020
		   then if e < ~1200 then 0.0
		     else let fun f(i,x) = if i=0 then x else f(i-1, x*0.5)
			   in f(1020-e, Assembly.A.scalb(m, ~1020))
			  end
		   else Assembly.A.scalb(m,e)  (* This is the common case! *)
	  else let val {man=m',exp=e'} = toManExp m
		in fromManExp { man = m', exp = e'+ e }
	       end

  (* some protection against insanity... *)
    val _ = if baseBits < 18  (* i.e., 3 * baseBits < 53 *)
	  then raise Fail "big digits in intinf implementation do not have enough bits"
	  else ()

    fun fromLargeInt(x : IntInf.int) = let
	  val CoreIntInf.BI { negative, digits } = CoreIntInf.concrete x
	  val w2r = fromInt o InlineT.Word.toIntX
	(* Looking at at most 3 "big digits" is always enough to
	 * get 53 bits of precision...
	 * (See insanity insurance above.)
	 *)
	  fun dosign (x: real) = if negative then ~x else x
	  fun calc (k, d1, d2, d3, []) =
		dosign (Assembly.A.scalb (w2r d1 +
					  rbase * (w2r d2 + rbase * w2r d3),
					  k))
	    | calc (k, _, d1, d2, d3 :: r) = calc (k + baseBits, d1, d2, d3, r)
	  in
	    case digits
	     of [] => 0.0
	      | [d] => dosign (w2r d)
	      | [d1, d2] => dosign (rbase * w2r d2 + w2r d1)
	      | d1 :: d2 :: d3 :: r => calc (0, d1, d2, d3, r)
	    (* end case *)
	  end

  (* whole and split could be implemented more efficiently if we had
   * control over the rounding mode; but for now we don't.
   *)
    fun whole x = if x>0.0
	    then if x > 0.5
	      then x-0.5+maxInt-maxInt
	      else whole(x+1.0)-1.0
	  else if x<0.0
	    then if x < ~0.5
	      then x+0.5-maxInt+maxInt
	      else whole(x-1.0)+1.0
	    else x

    fun split x = let
	  val w = whole x
	  val f = x-w
	  in
	    if abs f == 1.0
	      then {whole=w+f, frac=0.0}
	      else {whole=w, frac=f}
	  end

    fun realMod x = let
	  val f = x - whole x
	  in
	    if abs f == 1.0 then 0.0 else f
	  end

    fun rem(x,y) = y * #frac(split(x/y))

    fun checkFloat x = if x>negInf andalso x<posInf then x
                       else if isNan x then raise General.Div
			 else raise General.Overflow

    fun toLargeInt mode x =
	if isNan x then raise Domain
	else if x == posInf orelse x == negInf then raise Overflow
	else let val (negative, x) =
		     if x < 0.0 then (true, ~x) else (false, x)
		 fun feven x = #frac (split (x / 2.0)) == 0.0
	     in
		 (* if the magnitude is less than 1.0, then
		  * we just have to figure out whether to return ~1, 0, or 1
		  *)
		 if x < 1.0 then
		     case mode of
			 IEEEReal.TO_ZERO => 0
		       | IEEEReal.TO_POSINF =>
			   if negative then 0 else 1
		       | IEEEReal.TO_NEGINF =>
			   if negative then ~1 else 0
		       | IEEEReal.TO_NEAREST =>
			   if x < 0.5 then 0
			   else if x > 0.5 then
			       if negative then ~1 else 1
			   else 0	(* 0 is even *)
		 else
		     (* Otherwise we start with an integral value,
		      * suitably adjusted according to fractional part
		      * and rounding mode: *)
		     let val { whole, frac } = split x
			 val start =
			     case mode of
				 IEEEReal.TO_NEGINF =>
				   if frac > 0.0 andalso negative then
				       whole + 1.0
				   else whole
			       | IEEEReal.TO_POSINF =>
				   if frac > 0.0 andalso not negative then
				       whole + 1.0
				   else whole
			       | IEEEReal.TO_ZERO => whole
			       | IEEEReal.TO_NEAREST =>
				   if frac > 0.5 then whole + 1.0
				   else if frac < 0.5 then whole
				   else if feven whole then whole
				   else whole + 1.0

			 (* Now, for efficiency, we construct a
			  * fairly "small" whole number with
			  * all the significant bits.  First
			  * we get mantissa and exponent: *)
			 val { man, exp } = toManExp start
			 (* Then we adjust both to make sure the mantissa
			  * is whole:
			  * We know that man is between .5 and 1, so
			  * multiplying man by 2^53 will guarantee wholeness.
			  * However, exp might be < 53 -- which would be
			  * bad.  The correct solution is to multiply
			  * by 2^min(exp,53) and adjust exp by subtracting
			  * min(exp,53): *)
			 val adj = IntImp.min (precision, exp)
			 val man = fromManExp { man = man, exp = adj }
			 val exp = exp - adj

			 (* Now we can construct our bignum digits by
			  * repeated div/mod using the bignum base.
			  * This loop will terminate after two rounds at
			  * the most because we chop off 30 bits each
			  * time: *)
			 fun loop x =
			     if x == 0.0 then []
			     else
				 let val { whole, frac } = split (x / rbase)
				     val dig = InlineT.Word.fromInt
						   (Assembly.A.floor
							(frac * rbase))
				 in
				     dig :: loop whole
				 end
			 (* Now we make a bignum out of those digits: *)
			 val iman =
			     CoreIntInf.abstract
				 (CoreIntInf.BI { negative = negative,
						  digits = loop man })
		     in
			 (* Finally, we have to put the exponent back
			  * into the picture: *)
			 IntInfImp.<< (iman, InlineT.Word.fromInt exp)
		     end
	     end

    fun nextAfter _ = raise Fail "Real.nextAfter unimplemented"

    val min : real * real -> real = InlineT.Real64.min
    val max : real * real -> real = InlineT.Real64.max

    fun toDecimal _ = raise Fail "Real.toDecimal unimplemented"
    fun fromDecimal _ = raise Fail "Real.fromDecimal unimplemented"

    val fmt = RealFormat.fmtReal
    val toString = fmt (StringCvt.GEN NONE)
    val scan = RealScan.scanReal
    val fromString = StringCvt.scanString scan

    val ~ = InlineT.Real64.~
    val op +  = InlineT.Real64.+
    val op -  = InlineT.Real64.-
    val op *  = InlineT.Real64.*
    val op /  = InlineT.Real64./

    val op >  = InlineT.Real64.>
    val op <  = InlineT.Real64.<
    val op >= = InlineT.Real64.>=
    val op <= = InlineT.Real64.<=

  end (* Real64 *)
