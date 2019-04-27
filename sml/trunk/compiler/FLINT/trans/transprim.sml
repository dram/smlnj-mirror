(* transprim.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Translation of primops to PLambda.
 *)

structure TransPrim : sig

    val trans : {
	    coreAcc : string -> PLambda.lexp,
	    coreExn : string list -> PLambda.lexp option,
	    mkv : unit -> PLambda.lvar,
	    mkRaise : PLambda.lexp * PLambda.lty -> PLambda.lexp
	  } -> Primop.primop * PLambda.lty * PLambda.tyc list -> PLambda.lexp

  end = struct

    structure PO = Primop
    structure L = PLambda
    structure LT = PLambdaType   (* = LtyExtern *)
    structure Tgt = Target

    fun bug msg = ErrorMsg.impossible("TransPrim: " ^ msg)

    fun warn s = Control.Print.say(concat["*** WARNING: ", s, "\n"])

  (* various useful PLambda types *)
    val lt_tyc = LT.ltc_tyc
    val lt_arw = LT.ltc_parrow
    val lt_tup = LT.ltc_tuple
    val lt_int = LT.ltc_int
  (* the largest fixed-precision int type *)
    val lt_fixed_int = LT.ltc_num Tgt.fixedIntSz
    val lt_bool = LT.ltc_bool
    val lt_unit = LT.ltc_unit

    val lt_ipair = lt_tup [lt_int, lt_int]
    val lt_fixed_pair = lt_tup [lt_fixed_int, lt_fixed_int]
    val lt_icmp = lt_arw (lt_ipair, lt_bool)
    val lt_ineg = lt_arw (lt_int, lt_int)
    val lt_intop = lt_arw (lt_ipair, lt_int)

    val unitLexp = L.RECORD[]

    val boolsign = BasicTypes.boolsign
    val (trueDcon', falseDcon') = let
	  val lt = LT.ltc_parrow(LT.ltc_unit, LT.ltc_bool)
	  fun mk (Types.DATACON{name, rep, typ,...}) = (name, rep, lt)
	  in
	    (mk BasicTypes.trueDcon, mk BasicTypes.falseDcon)
          end

    val trueLexp = L.CON(trueDcon', [], unitLexp)
    val falseLexp = L.CON(falseDcon', [], unitLexp)

  (* unsigned comparison on tagged integers used for bounds checking *)
    val LESSU = L.PRIM(PO.CMP{oper=PO.LTU, kind=PO.UINT Tgt.defaultIntSz}, lt_icmp, [])

    val lt_len = LT.ltc_poly([LT.tkc_mono], [lt_arw(LT.ltc_tv 0, lt_int)])
    val lt_upd = let
	  val x = LT.ltc_ref (LT.ltc_tv 0)
          in
	    LT.ltc_poly([LT.tkc_mono], [lt_arw(lt_tup [x, lt_int, LT.ltc_tv 0], LT.ltc_unit)])
          end

  (* get length of sequence *)
    fun lenOp seqtc = L.PRIM(PO.LENGTH, lt_len, [seqtc])

 (* inline operators for numeric types *)
    fun inlops nk = let
	  val (lt_arg, zero, overflow) = (case nk
		 of PO.INT sz => (LT.ltc_num sz, L.INT{ival = 0, ty = sz}, true)
		  | PO.UINT sz => (LT.ltc_num sz, L.WORD{ival = 0, ty = sz}, false)
(* REAL64: type will depend on size *)
		  | PO.FLOAT sz => (LT.ltc_real, L.REAL{rval = RealLit.zero false, ty = sz}, false)
		(* end case *))
	  val lt_argpair = lt_tup [lt_arg, lt_arg]
	  val lt_cmp = lt_arw (lt_argpair, lt_bool)
	  val lt_neg = lt_arw (lt_arg, lt_arg)
	  val less = L.PRIM (PO.CMP { oper = PO.LT, kind = nk }, lt_cmp, [])
	  val greater = L.PRIM (PO.CMP { oper = PO.GT, kind = nk }, lt_cmp, [])
	  val equal = L.PRIM (PO.CMP { oper = PO.EQL, kind = nk }, lt_cmp, [])
	  val negate =
		L.PRIM (PO.ARITH { oper = PO.NEG, overflow = overflow, kind = nk }, lt_neg, [])
	  in {
	    lt_arg = lt_arg, lt_argpair = lt_argpair, lt_cmp = lt_cmp,
	    less = less, greater = greater, equal = equal,
	    zero = zero, negate = negate
	  } end

  (* shift primops *)
    fun rshiftOp k = PO.ARITH{oper=PO.RSHIFT, overflow=false,  kind=k}
    fun rshiftlOp k = PO.ARITH{oper=PO.RSHIFTL, overflow=false, kind=k}
    fun lshiftOp k = PO.ARITH{oper=PO.LSHIFT,  overflow=false, kind=k}

  (* zero literal for fiven word type*)
    fun lword0 (PO.UINT sz) = L.WORD{ival = 0, ty = sz}

    fun baselt (PO.UINT sz) = LT.ltc_num sz

  (* type of a shift operation where `k` is the kind of value being shifted *)
    fun shiftTy k = let
	  val elem = baselt k
          in
	    lt_arw(lt_tup [elem, lt_int], elem)
          end

  (* trans : Primop.primop * Lty.lty * Lty.tyc list
   *
   * Translate Absyn primop to PLambda form using given
   * intrinsic PLambda type and type parameters
   *)
    fun trans { coreAcc, coreExn, mkv, mkRaise } (prim, lt, ts) = let
	(* make a function expression *)
	  fun mkFn ty body = let
		val x = mkv()
		in
		  L.FN(x, ty, body(L.VAR x))
		end
	(* make a let expression *)
	  fun mkLet rhs body = let
		val x = mkv()
		in
		  L.LET(x, rhs, body(L.VAR x))
		end
	(* make an application to two arguments *)
	  fun mkApp2 (rator, a, b) = L.APP(rator, L.RECORD[a, b])
	(* if-then-else *)
	  fun mkCOND (a, b, c) = L.SWITCH(a, boolsign, [
		  (L.DATAcon(trueDcon', [], mkv()), b),
		  (L.DATAcon(falseDcon', [], mkv()), c)
		],
		NONE)
	(* expand an inline shift operation.*)
	  fun inlineShift (shiftOp, kind, clear) = let
		val shiftLimit = (case kind
		       of (PO.UINT lim | PO.INT lim) =>
			    L.WORD{ival = IntInf.fromInt lim, ty = Tgt.defaultIntSz}
			| _ => bug "unexpected kind in inlineShift"
		      (* end case *))
		val argt = lt_tup [baselt kind, lt_int]
		val cmpShiftAmt =
		      L.PRIM(PO.CMP{oper=PO.LEU, kind=PO.UINT Tgt.defaultIntSz}, lt_icmp, [])
		in
		  mkFn argt (fn p =>
		    mkLet (L.SELECT(0, p)) (fn w =>
		      mkLet (L.SELECT(1, p)) (fn cnt =>
			mkCOND(
			  L.APP(cmpShiftAmt, L.RECORD[shiftLimit, cnt]),
			  clear w,
			  L.APP(L.PRIM(shiftOp kind, shiftTy kind, []), L.RECORD[w, cnt])))))
		end
	(* bounds check for vector/array access *)
	  fun boundsChk (ix, seq, seqtc, t) body = (
		case coreExn ["Subscript"]
		 of SOME ssexn =>
		      mkCOND(L.APP(LESSU, L.RECORD[ix, L.APP(lenOp seqtc, seq)]),
			body,
			mkRaise(ssexn, t))
		  | NONE => (
		      warn "no access to exn Subscript for inline subscript";
		      body)
		(* end case *))
	(* inline subscript for vectors and arrays *)
	  fun inlSubscript (subOp, argt, seqtc, t) = let
		val oper = L.PRIM (subOp, lt, ts)
		in
		  case coreExn ["Subscript"]
		   of SOME ssexn =>
			mkFn argt (fn p =>
			  mkLet (L.SELECT(0, p)) (fn a =>
			    mkLet (L.SELECT(1, p)) (fn i =>
			      boundsChk (i, a, seqtc, t) (mkApp2(oper, a, i)))))
		     | NONE => (
			warn "no access to exn Subscript for inline subscript";
			oper)
		  (* end case *)
		end
	(* division operators with an explicit test for a zero divisor *)
	  fun inldiv (nk, po, lt, ts) = let
		val oper = L.PRIM (po, lt, ts)
		in
		  case coreExn ["Assembly", "Div"]
		   of SOME divexn => let
			val { lt_arg, lt_argpair, lt_cmp, zero, equal, ... } = inlops nk
			in
			  mkFn lt_argpair (fn z =>
			    mkLet (L.SELECT(1, z)) (fn y =>
			      mkCOND (
				mkApp2 (equal, y, zero),
				mkRaise (divexn, lt_arg),
				L.APP(oper, z))))
			end
		   | NONE => (warn "no access to Div exception"; oper)
		end
	(* inline min/max *)
	  fun inlminmax (nk, ismax) = let
		val { lt_argpair, less, greater, lt_cmp, ... } = inlops nk
		val cmpop = if ismax then greater else less
		in
		  mkFn lt_argpair (fn z =>
		    mkLet (L.SELECT(0, z)) (fn x =>
		      mkLet (L.SELECT(1, z)) (fn y =>
			mkCOND (
			  mkApp2 (cmpop, x, y),
			  x,
			  case nk
			   of PO.FLOAT _ => let (* testing for NaN *)
				val fequal = L.PRIM (PO.CMP { oper = PO.EQL, kind = nk }, lt_cmp, [])
				in
				  mkCOND (mkApp2 (fequal, y, y), y, x)
				end
			    | _ => y))))
		end
	(* inline absolute value for integer types *)
	  fun inlabs nk = let
		val { lt_arg, greater, zero, negate, ... } = inlops nk
		in
		  mkFn lt_arg (fn x =>
		    mkCOND (mkApp2 (greater, x, zero), x, L.APP(negate, x)))
		end
	(** Precision converting translation using a conversion
	 *  primitive named in the second argument.
	 *
	 *  Examples:
	 *	inlToInfPrec ("EXTEND_INF", "finToInf", p, lt)
	 *	inlToInfPrec ("COPY", "finToInf", p, lt)
	 *
	 *  where "finToInf" is defined at
	 *
	 *	system/smlnj/init/core-intinf.sml:51:    val finToInf  : int32 * bool -> intinf
	 *)
	  fun inlToInfPrec (opname: string, coerceFnName: string, primop, primoplt) = let
		val (orig_arg_lt, res_lt) = (
		      case LT.ltd_arrow primoplt handle LT.DeconExn => bug "inlToInfPrec"
		       of (_, [a], [r]) => (a, r)
			| _ => bug ("unexpected type of " ^ opname)
		      (* end case *))
		val extra_arg_lt = if coerceFnName = "finToInf"
		      then LT.ltc_arrow(LT.ffc_var(true, false),
				 [lt_fixed_int ,LT.ltc_bool], [res_lt])
		      else LT.ltc_parrow(lt_fixed_int, res_lt)
		val new_arg_lt = LT.ltc_tuple [orig_arg_lt, extra_arg_lt]
		val new_lt = LT.ltc_parrow (new_arg_lt, res_lt)
		in
		  mkFn orig_arg_lt (fn x =>
		    mkApp2 (L.PRIM (primop, new_lt, []), x, coreAcc coerceFnName))
		end
	  fun inlFromInfPrec (opname, coerceFnName, primop, primoplt) = let
		val (orig_arg_lt, res_lt) = (
		      case LT.ltd_arrow primoplt handle LT.DeconExn => bug "inlFromInfPrec"
		       of (_, [a], [r]) => (a, r)
			| _ => bug ("unexpected type of " ^ opname)
		      (* end case *))
		val extra_arg_lt = LT.ltc_parrow (orig_arg_lt, lt_fixed_int)
		val new_arg_lt = LT.ltc_tuple [orig_arg_lt, extra_arg_lt]
		val new_lt = LT.ltc_parrow (new_arg_lt, res_lt)
		in
		  mkFn orig_arg_lt (fn x =>
		    mkApp2 (L.PRIM (primop, new_lt, []), x, coreAcc coerceFnName))
		end
	  in
	    case prim
	     of PO.INLLSHIFT k => inlineShift(lshiftOp, k, fn _ => lword0 k)
	      | PO.INLRSHIFTL k => inlineShift(rshiftlOp, k, fn _ => lword0 k)
	      | PO.INLRSHIFT k => let
		(* preserve sign bit with arithmetic rshift *)
		  val shiftWidth = L.WORD{ival = Int.toLarge Tgt.defaultIntSz, ty = Tgt.defaultIntSz}
		  fun clear w = mkApp2(L.PRIM(rshiftOp k, shiftTy k, []), w, shiftWidth)
		  in
		    inlineShift(rshiftOp, k, clear)
		  end
	      | PO.INLMIN nk => inlminmax (nk, false)
	      | PO.INLMAX nk => inlminmax (nk, true)
	      | PO.INLABS nk => inlabs nk

	      | PO.ARITH { oper = (PO.DIV | PO.QUOT | PO.MOD | PO.REM),
				 kind = nk as (PO.INT _ | PO.UINT _),
				 overflow } =>
		  inldiv (nk, prim, lt, ts)

	      | PO.INLNOT => mkFn lt_bool (fn x => mkCOND(x, falseLexp, trueLexp))
	      | PO.INLCOMPOSE => let
		  val (t1, t2, t3) = (case ts
			 of [a,b,c] => (lt_tyc a, lt_tyc b, lt_tyc c)
			  | _ => bug "unexpected type for INLCOMPOSE"
			(* end case *))
		  val argt = lt_tup [lt_arw(t2, t3), lt_arw(t1, t2)]
		  val f = mkv() and g = mkv()
		  in
		    mkFn argt (fn z =>
		      mkLet (L.SELECT(0, z)) (fn f =>
			mkLet (L.SELECT(1, z)) (fn g =>
			  mkFn t1 (fn x => L.APP(f, L.APP(g, x))))))
		  end
	      | PO.INLBEFORE => let
		  val (t1, t2) = (case ts
			 of [a,b] => (lt_tyc a, lt_tyc b)
			  | _ => bug "unexpected type for INLBEFORE"
			(* end case *))
		  val argt = lt_tup [t1, t2]
		  in
		    mkFn argt (fn x => L.SELECT(0, x))
		  end
	      | PO.INLIGNORE => let
		  val argt = (case ts
			 of [a] => lt_tyc a
			  | _ => bug "unexpected type for INLIGNORE"
			(* end case *))
		  in
		    mkFn argt (fn _ => unitLexp)
		  end
	      | PO.INLIDENTITY => let
		  val argt = (case ts
			 of [a] => lt_tyc a
			  | _ => bug "unexpected type for INLIDENTITY"
			(* end case *))
		  in
		    mkFn argt (fn v => v)
		  end
(* 64BIT: FIXME *)
	      | PO.CVT64 => mkFn (lt_tup[LT.ltc_num 32, LT.ltc_num 32]) (fn v => v) (* int64 in 32BIT *)
	      | PO.INLSUBSCRIPTV => let
		  val (tc1, t1) = (case ts
			 of [z] => (z, lt_tyc z)
			  | _ => bug "unexpected ty for INLSUBSCRIPTV"
			(* end case *))
		  val seqtc = LT.tcc_vector tc1
		  val argt = lt_tup [lt_tyc seqtc, lt_int]
		  in
		    inlSubscript (PO.SUBSCRIPT, argt, seqtc, t1)
		  end
	      | PO.INLSUBSCRIPT => let
		  val (tc1, t1) = (case ts
			 of [z] => (z, lt_tyc z)
			  | _ => bug "unexpected ty for INLSUBSCRIPT"
			(* end case *))
		  val seqtc = LT.tcc_array tc1
		  val argt = lt_tup [lt_tyc seqtc, lt_int]
		  in
		    inlSubscript (PO.SUBSCRIPT, argt, seqtc, t1)
		  end
	      | PO.NUMSUBSCRIPT{kind, checked=true, immutable} => let
		  val (tc1, t1, t2) = (case ts
			 of [a, b] => (a, lt_tyc a, lt_tyc b)
			  | _ => bug "unexpected type for NUMSUBSCRIPT"
			(* end case *))
		  val argt = lt_tup [t1, lt_int]
		  in
		    inlSubscript (
		      PO.NUMSUBSCRIPT{kind=kind, checked=false, immutable=immutable},
		      argt, tc1, t2)
		  end
	      | PO.INLUPDATE => let
		  val oper = L.PRIM(PO.UPDATE, lt, ts)
		  val (tc1, t1) = (case ts
			 of [z] => (z, lt_tyc z)
			  | _ => bug "unexpected ty for INLUPDATE"
			(* end case *))
		  val seqtc = LT.tcc_array tc1
		  val argt = lt_tup [lt_tyc seqtc, lt_int, t1]
		  in
		    mkFn argt (fn x =>
		      mkLet (L.SELECT(0, x)) (fn a =>
			mkLet (L.SELECT(1, x)) (fn i =>
			  mkLet (L.SELECT(2, x)) (fn v =>
			    boundsChk (i, a, seqtc, LT.ltc_unit) (L.APP(oper, L.RECORD[a, i, v]))))))
		  end
	      | PO.NUMUPDATE{kind, checked=true} => let
		  val oper = L.PRIM(PO.NUMUPDATE{ kind = kind, checked = false }, lt, ts)
		  val (tc1, t1, t2) = (case ts
			 of [a, b] => (a, lt_tyc a, lt_tyc b)
			  | _ => bug "unexpected type for NUMUPDATE"
			(* end case *))
		  val argt = lt_tup [t1, lt_int, t2]
		  in
		    mkFn argt (fn x =>
		      mkLet (L.SELECT(0, x)) (fn a =>
			mkLet (L.SELECT(1, x)) (fn i =>
			  mkLet (L.SELECT(2, x)) (fn v =>
			    boundsChk (i, a, tc1, LT.ltc_unit) (L.APP(oper, L.RECORD[a, i, v]))))))
		  end
	    (* Precision-conversion operations involving IntInf.
	     * These need to be translated specially by providing
	     * a second argument -- the routine from _Core that
	     * does the actual conversion to or from IntInf.
	     *)
	      | PO.TEST_INF prec => inlFromInfPrec ("TEST_INF", "testInf", prim, lt)
	      | PO.TRUNC_INF prec => inlFromInfPrec ("TRUNC_INF", "truncInf", prim, lt)
	      | PO.EXTEND_INF prec => inlToInfPrec ("EXTEND_INF", "finToInf", prim, lt)
	      | PO.COPY_INF prec => inlToInfPrec ("COPY", "finToInf", prim, lt)
	    (* default handling for all other primops *)
	      | p => L.PRIM(p, lt, ts)
	    (* end case *)
	  end (* trans *)

  end (* PrimTrans *)
