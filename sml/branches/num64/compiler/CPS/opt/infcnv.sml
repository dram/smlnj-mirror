(* infcnv.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Expand out any remaining occurences of test_inf, trunc_inf, extend_inf,
 * and copy_inf.  These primops carry a second argument, which is the
 * function that performs the operation.
 *)

structure IntInfCnv : sig

    val elim : CPS.function -> CPS.function

end = struct

    structure C = CPS
    structure LV = LambdaVar

    fun bug msg = ErrorMsg.impossible ("IntInfCnv: " ^ msg)

    val boxNumSz = Target.mlValueSz	(* 32 or 64 *)

    val tagNumTy = C.NUMt{tag = true, sz = Target.defaultIntSz}
    val boxNumTy = C.NUMt{tag = false, sz = boxNumSz}

  (* generate code to convert a fixed-size number to an intinf.  The arguments
   * are:
   *	prim		-- the primop (COPY or EXTEND) for converting a tagged
   *			   value to the boxed number type
   *	sz		-- the size of the source value
   *	x		-- the value being converted
   *	f		-- the conversion function from the "_Core" structure.
   *	v, t, e		-- \v:t.e is the continuation of the conversion.
   *)
    fun toInf (prim, sz, [x, f], v, t, e) = let
	  val k = LV.mkLvar ()
	  val body = if (sz <= Target.defaultIntSz)
		  then let
		  (* for tagged values, we promote to the boxed type before calling
		   * the conversion function.
		   *)
		    val v' = LV.mkLvar ()
		    in
		      C.PURE (prim{from=sz, to=boxNumSz}, [x], v', boxNumTy,
			C.APP (f, [C.VAR k, C.VAR v']))
		    end
		else if (sz = boxNumSz)
		  then C.APP (f, [C.VAR k, x])
		  else let
		  (* for a 64-bit argument on 32-bit target, we need to extern the
		   * argument to a pair of 32-bit words, before calling the
		   * conversion function.
		   *)
		    val hi = LV.mkLvar ()
		    val lo = LV.mkLvar ()
		    in
		      C.SELECT(0, x, hi, boxNumTy,
		      C.SELECT(1, x, lo, boxNumTy,
			C.APP (f, [C.VAR k, C.VAR hi, C.VAR lo])))
		    end
	  in
	    C.FIX ([(C.CONT, k, [v], [t], e)], body)
	  end
      | toInf _ = bug "toInf: incorrect number of arguments"

  (* generate code to convert an intinf to a fixed-size number.  The arguments
   * are:
   *    mkExp           -- constructor to make CPS expression (ARITH or PURE)
   *	prim		-- the primop (TRUNC or TEST) for converting a boxed
   *			   value to the tagged type
   *	sz		-- the size of the source value
   *	x		-- the value being converted
   *	f		-- the conversion function from the "_Core" structure.
   *	v, t, e		-- \v:t.e is the continuation of the conversion, where
   *			   the expression e should have already been converted.
   *)
    fun fromInf (mkExp, prim, sz, [x, f], v, t, e) = let
	  val k = LV.mkLvar ()
	  in
	    if (sz <= Target.defaultIntSz)
	      then let
		val v' = LV.mkLvar ()
		val retContBody =
		      mkExp (prim{from=boxNumSz, to=sz}, [C.VAR v'], v, t, e)
		in
		  C.FIX (
		    [(C.CONT, k, [v'], [boxNumTy], retContBody)],
		    C.APP (f, [C.VAR k, x]))
		end
	    else if (sz = boxNumSz)
	      then C.FIX ([(C.CONT, k, [v], [t], e)], C.APP (f, [C.VAR k, x]))
	      else let
	      (* for a 64-bit result on 32-bit target, we need to intern the
	       * result, which will be a packed pair of 32-bit words.
	       *)
		val hi = LV.mkLvar ()
		val lo = LV.mkLvar ()
		val retContBody = C.RECORD(C.RK_RAWBLOCK, [
			(C.VAR hi, C.OFFp 0), (C.VAR lo, C.OFFp 0)
		      ], v, e)
		in
		  C.FIX (
		    [(C.CONT, k, [hi, lo], [boxNumTy, boxNumTy], retContBody)],
		    C.APP (f, [C.VAR k, x]))
		end
	  end
      | fromInf _ = bug "toInf: incorrect number of arguments"

    fun elim cfun = let
	  fun cexp (C.RECORD (rk, xl, v, e)) =
		C.RECORD (rk, xl, v, cexp e)
	    | cexp (C.SELECT (i, x, v, t, e)) =
		C.SELECT (i, x, v, t, cexp e)
	    | cexp (C.OFFSET (i, v, x, e)) =
		C.OFFSET (i, v, x, cexp e)
	    | cexp (C.APP (x, xl)) =
		C.APP (x, xl)
	    | cexp (C.FIX (fl, e)) =
		C.FIX (map function fl, cexp e)
	    | cexp (C.SWITCH (x, v, el)) =
		C.SWITCH (x, v, map cexp el)
	    | cexp (C.BRANCH (b, xl, v, e1, e2)) =
		C.BRANCH (b, xl, v, cexp e1, cexp e2)
	    | cexp (C.SETTER (s, xl, e)) =
		C.SETTER (s, xl, cexp e)
	    | cexp (C.LOOKER (l, xl, v, t, e)) =
		C.LOOKER (l, xl, v, t, cexp e)
	    | cexp (C.PURE (C.P.COPY_INF sz, args, v, t, e)) =
		toInf (C.P.COPY, sz, args, v, t, cexp e)
	    | cexp (C.PURE (C.P.EXTEND_INF sz, args, v, t, e)) =
		toInf (C.P.EXTEND, sz, args, v, t, cexp e)
	    | cexp (C.PURE (C.P.TRUNC_INF sz, args, v, t, e)) =
		fromInf (C.PURE, C.P.TRUNC, sz, args, v, t, cexp e)
	    | cexp (C.ARITH (C.P.TEST_INF sz, args, v, t, e)) =
		fromInf (C.ARITH, C.P.TEST, sz, args, v, t, cexp e)
	    | cexp (C.ARITH (a, xl, v, t, e)) = C.ARITH (a, xl, v, t, cexp e)
	    | cexp (C.PURE (p, xl, v, t, e)) = C.PURE (p, xl, v, t, cexp e)
	    | cexp (C.RCC (k, s, p, xl, vtl, e)) = C.RCC (k, s, p, xl, vtl, cexp e)

	  and function (fk, f, vl, tl, e) = (fk, f, vl, tl, cexp e)
	  in
	    function cfun
	  end

  end
