(* real64.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

structure Real64 : REAL = 
  struct
    structure I = InlineT.DfltInt

    structure Math = Math64

    infix 4 == !=
    type real = real
    val ~ = InlineT.Real64.~
    val op +  = InlineT.Real64.+
    val op -  = InlineT.Real64.-
    val op *  = InlineT.Real64.*
    val op /  = InlineT.Real64./
    fun *+(a,b,c) = a*b+c
    fun *-(a,b,c) = a*b-c

    val op >  = InlineT.Real64.>
    val op <  = InlineT.Real64.<
    val op >= = InlineT.Real64.>=
    val op <= = InlineT.Real64.<=

    val op == = InlineT.Real64.==
    val op != = InlineT.Real64.!=

    fun unordered(x,y) = Bool.not(x>y orelse x <= y)
    fun ?= (x, y) = (x == y) orelse unordered(x, y)

    fun isNormal x = (case Assembly.A.logb x
	   of ~1023 => false	(* 0.0 or subnormal *)
	    | 1024 => false	(* inf or nan *)
	    | _ => true
	  (* end case *))

  (* The next three values are computed laboriously, partly to
   * avoid problems with inaccurate string->float conversions
   * in the compiler itself.
   *)
    val maxFinite = let
	  fun f(x,i) = if i=1023 then x else f(x*2.0, I.+(i, 1))
	  val y = f(1.0,0)
	  fun g(z, y, 0) = z
	    | g(z, y, i) = g(z+y, y*0.5, I.-(i, 1))
	  in
	    g(0.0,y,53)
	  end

    val minNormalPos = let
	  fun f(x) = let
		val y = x * 0.5
		in
		  if isNormal y then f y else x
		end
	  in
	    f 1.0
	  end

    local
      (* The x86 uses extended precision (80 bits) internally, therefore 
       * it is necessary to write out the result of r * 0.5 to get 
       * 64 bit precision.
       *)
      val mem = InlineT.PolyArray.array(1, minNormalPos)
      val update = InlineT.PolyArray.update
      val subscript = InlineT.PolyArray.chkSub
      fun f () = let
	val r = subscript(mem, 0)
	val y = r * 0.5
      in
	 update(mem, 0, y);
	 if subscript(mem, 0) == 0.0 then r else f ()
      end
    in
      val minPos = f()
    end

    val posInf = maxFinite * maxFinite
    val negInf = ~posInf

    fun isFinite x = negInf < x andalso x < posInf
    fun isNan x = Bool.not(x==x)
    fun floor x = if x < 1073741824.0 andalso x >= ~1073741824.0
	           then Assembly.A.floor x
		  else if isNan x then raise General.Domain
		  else raise General.Overflow

    fun ceil n = I.-(~1,floor(~(n+1.0)))
    fun trunc n = if n < 0.0 then ceil n else floor n
    fun round x = floor(x+0.5)  (* bug: does not do round-to-nearest *)

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
	      if x>=0.0 then x+maxInt-maxInt else x-maxInt+maxInt
		before IEEEReal.setRoundingMode saveMode
	    end
    in
    val realFloor = realround IEEEReal.TO_NEGINF
    val realCeil = realround IEEEReal.TO_POSINF
    val realTrunc = realround IEEEReal.TO_ZERO
    end

    val abs : real -> real = InlineT.Real64.abs
    val fromInt : int -> real = InlineT.real

    (* bug: operates correctly but slowly *)
    fun fromLargeInt(x : Int32.int) =
       let val i = Int32.quot(x,2)
           val j = Int32.-(x,Int32.+(i,i))
           val i' = Int32.toInt i
	   val j' = Int32.toInt j
        in fromInt(i')*2.0+fromInt(j')
       end    

     (* bug: only one rounding mode implemented *)
    fun toInt IEEEReal.TO_NEGINF = floor
      | toInt _ = raise Fail "toInt supports only NEGINF rounding mode now"

      (* bug: doesn't support full range of large ints *)
    fun toLargeInt mode x = Int32.fromInt(toInt mode x)

    fun toLarge x = x
    fun fromLarge _ x = x       

    fun sign x = if (x < 0.0) then ~1 else if (x > 0.0) then 1 
                  else if isNan x then raise Domain else 0
    fun signBit x = (* Bug: negative zero not handled properly *)
                   Assembly.A.scalb(x, I.~(Assembly.A.logb x)) < 0.0

    fun sameSign (x, y) = signBit x = signBit y

    fun copySign(x,y) = (* may not work if x is Nan *)
           if sameSign(x,y) then x else ~x

    fun compare(x,y) = if x<y then General.LESS else if x>y then General.GREATER
                       else if x == y then General.EQUAL 
			    else raise IEEEReal.Unordered
    
    fun compareReal(x,y) = 
           if x<y then IEEEReal.LESS else if x>y then IEEEReal.GREATER
                       else if x == y then IEEEReal.EQUAL 
			    else IEEEReal.UNORDERED
    

(** This proably needs to be reorganized **)
    fun class x =  (* does not distinguish between quiet and signalling NaN *)
      if signBit x
       then if x>negInf then if x == 0.0 then IEEEReal.ZERO
	                     else if Assembly.A.logb x = ~1023
			          then IEEEReal.SUBNORMAL
			          else IEEEReal.NORMAL
	                else if x==x then IEEEReal.INF
			             else IEEEReal.NAN IEEEReal.QUIET
       else if x<posInf then if x == 0.0 then IEEEReal.ZERO
	                     else if Assembly.A.logb x = ~1023
			          then IEEEReal.SUBNORMAL
			          else IEEEReal.NORMAL
	                else if x==x then IEEEReal.INF
			             else IEEEReal.NAN IEEEReal.QUIET

    val radix = 2
    val precision = 52

    val two_to_the_54 = 18014398509481984.0

    val two_to_the_neg_1000 =
      let fun f(i,x) = if i=0 then x else f(I.-(i,1), x*0.5)
       in f(1000, 1.0)
      end

 (* AARGH!  Our version of logb gives a value that's one less than the
        rest of the world's logb functions.
        We should fix this systematically some time. *)

    fun toManExp x = 
      case I.+(Assembly.A.logb x, 1)
	of ~1023 => if x==0.0 then {man=x,exp=0}
		    else let val {man=m,exp=e} = toManExp(x*1048576.0)
		              in {man=m,exp=I.-(e,20)}
			 end
         | 1024 => {man=x,exp=0}
         | i => {man=Assembly.A.scalb(x,I.~ i),exp=i}

    fun fromManExp {man=m,exp=e:int} =
      if (m >= 0.5 andalso m <= 1.0  orelse m <= ~0.5 andalso m >= ~1.0)
	then if I.>(e, 1020)
	  then if I.>(e, 1050) then if m>0.0 then posInf else negInf
	       else let fun f(i,x) = if i=0 then x else f(I.-(i,1),x+x)
		       in f(I.-(e,1020),  Assembly.A.scalb(m,1020))
		      end
	  else if I.<(e, I.~ 1020)
	       then if I.<(e, I.~ 1200) then 0.0
		 else let fun f(i,x) = if i=0 then x else f(I.-(i,1), x*0.5)
		       in f(I.-(1020,e), Assembly.A.scalb(m,I.~ 1020))
		      end
	       else Assembly.A.scalb(m,e)  (* This is the common case! *)
      else let val {man=m',exp=e'} = toManExp m
            in fromManExp{man=m', exp=I.+(e',e)}
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

    fun split x = let val w = whole x 
                      val f = x-w
		   in if abs(f)==1.0
		     then {whole=w+f,frac=0.0}
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

(** NOTE logb and scalb are also defined in math64.sml; do we need both??? **)
    fun logb x = (case Assembly.A.logb x
	   of ~1023 => (* denormalized number *)
		I.-(Assembly.A.logb(x * two_to_the_54), 54)
	    | i => i
	  (* end case *))

    fun scalb (x, k) = if I.ltu(I.+(k,1022),2046)
	  then Assembly.A.scalb(x,k)
          else let val k1 = I.div(k, 2)
	    in
	      scalb(scalb(x, k1), I.-(k, k1))
	    end
  
    fun nextAfter _ = raise Fail "Real.nextAfter unimplemented"

    fun min(x,y) = if x<y orelse isNan y then x else y
    fun max(x,y) = if x>y orelse isNan y then x else y

    fun toDecimal _ = raise Fail "Real.toDecimal unimplemented"
    fun fromDecimal _ = raise Fail "Real.fromDecimal unimplemented"

    val fmt = RealFormat.fmtReal
    val toString = fmt (StringCvt.GEN NONE)
    val scan = NumScan.scanReal
    val fromString = StringCvt.scanString scan

  end (* Real64 *)


(*
 * $Log$
 *)
