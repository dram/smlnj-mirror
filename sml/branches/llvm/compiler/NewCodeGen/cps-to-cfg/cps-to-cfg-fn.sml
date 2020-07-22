(* cps-to-cfg-fn.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Translate the first-order CPS IR to the CFG IR.
 *)

functor CPStoCFGFn (MS : MACH_SPEC) : sig

    val translate : {
	    source : string,
	    funcs : CPS.function list,
	    limits :  CPS.lvar -> int * int
	  } -> CFG.cluster list

  end = struct

    structure LV = LambdaVar
    structure LTbl = LV.Tbl
    structure LMap = LV.Map

  (* source primops *)
    structure P = CPS.P
  (* target primops *)
    structure TP = CFG_Prim
  (* object descriptors *)
    structure D = MS.ObjDesc

  (* import CPS expression constructors *)
    datatype value = datatype CPS.value
    datatype cexp = datatype CPS.cexp

    fun error msg = ErrorMsg.impossible(String.concat("CPStoCFGFn: " :: msg))

    val defaultIntSz = Target.defaultIntSz

  (* These are the bit widths of ML values *)
    val ity = MS.wordBitWidth (* size of ML's pointer/value *)
    val fty = 64 (* size in bits of ML's real number *)			(* REAL32: FIXME *)
    val ws = MS.wordByteWidth
    val valueSzb = IntInf.fromInt MS.valueSize
    val realSzb = IntInf.fromInt MS.realSize
    val addrTy = MS.addressBitWidth	(* naturalsize of address arithmetic *)
    val wordsPerDbl = 8 div ws
    val wordsPerDbl' = IntInf.fromInt wordsPerDbl
    val cfgITy = CFG.NUMt ity

  (* return true if integers of `sz` bits are represented as tagged values *)
    fun isTaggedInt sz = (sz <= defaultIntSz)

  (* normalize an integer size to a native machine-size *)
    fun normSz sz = if sz = defaultIntSz then ity else sz

  (* pointers to unknown objects *)
    val BOGty = CPSUtil.BOGt

  (* CFG integer constants *)
    fun num iv = CFG.NUM{iv = iv, sz = ity}
    fun num' iv = CFG.NUM{iv = IntInf.fromInt iv, sz = ity}
    fun szNum sz iv = CFG.NUM{iv = iv, sz = sz}
    fun w2Num iv = num(Word.toLargeInt iv)
    fun mlInt' n = num(n+n+1)
    fun mlInt n = mlInt'(IntInf.fromInt n)

  (* some useful constants *)
    fun zero sz = szNum sz 0
    val one = num 1
    fun allOnes sz = num(ConstArith.bNot(sz, 0)) (* sz-wide 1's *)
    val signBit = num(IntInf.<<(1, Word.fromInt ity - 0w1))

(* FIXME: add support for branch probabilities *)
  (* unknown branch probability *)
    val unkProb = 0

  (* convert CPS types to CFG types *)
    fun cvtTy cpsTy = (case cpsTy
	   of CPS.NUMt{sz, tag=true} => cfgITy
	    | CPS.NUMt{sz, ...} => CFG.NUMt sz
	    | CPS.FLTt sz => CFG.FLTt sz
	    | CPS.PTRt _ => CFG.PTRt
	    | CPS.FUNt => CFG.FUNt
	    | CPS.CNTt => CFG.CNTt
	  (* end case *))

  (* Tracking expressions that are bound to variables that are used more than
   * once.
   *)

  (* abstraction of a use count: a TREE variable has just one use, and
   * a BOUND variable has multiple uses.  Unused variables do not appear
   * in the table.
   *)
    datatype use_mode = TREE | BOUND

    datatype info = Info of {
	modes : use_mode LTbl.hash_table,	(* maps LVars to uses *)
	typs : CPS.cty LTbl.hash_table,
	attrs : CFG.attrs			(* cluster attributes *)
      }

    fun lookupMode (Info{modes, ...}) = LTbl.lookup modes

    fun typeOfVar (Info{typs, ...}) = LTbl.lookup typs

    fun typeOf info = let
	  val typeOfVar = typeOfVar info
	  in
	    fn (C.VAR x) => typeOfVar x
	     | (C.LABEL lab) => typeOfVar lab
	     | (C.NUM{ty, ...}) => C.NUMt ty
	     | v => error ["unexpected ", PPCps.value2str v]
	  end

    fun numVars (Info{modes, ...}) = LTbl.numItems modes

  (* initialize a table that maps variables to use modes for a CPS expression *)
    fun initUseModes cexp = let
	  val modes = LTbl.mkTable(32, Fail "modes")
	  val getMode = LTbl.find modes
	  val setMode = LTbl.insert modes
	  fun useVar x = (case getMode x
		 of NONE => setMode (x, TREE)
		  | SOME TREE => setMode (x, BOUND)
		  | _ => ()
		(* end case *))
	  fun useVal (VAR x) = useVar x
	    | useVal _ = ()
	(* mark a variable as "BOUND" since it will map to a CFG variable *)
	  fun bindVar x = setMode (x, BOUND)
	  val useVals = List.app useVal
(* QUESTION: should we track CFG types instead of CPS types? *)
	  val typs = LTbl.mkTable(32, Fail "typs")
	  val recordTy = LTbl.insert typs
	  val needsBasePtr = ref false
	  val hasRCC = ref false
	  val alignHP = ref MS.valueSize
	  fun align sz = if !alignHP < sz then alignHP := sz else ()
	  fun init cexp = (case cexp
		 of RECORD(rk, flds, x, k) => let
		      fun useField (VAR x, _) = useVar x
			| useField (LABEL l, _) = (needsBasePtr := true)
			| useField _ = ()
		      in
			case rk
			 of CPS.RK_FCONT => align 8
			  | CPS.RK_RAW64BLOCK => align 8
			  | _ => ()
			(* end case *);
		        recordTy (x, BOGty);
			List.app useField flds;
			init k
		      end
		  | SELECT(_, v, x, ty, k) => (recordTy (x, ty); useVal v; init k)
		  | OFFSET(_, v, x, k) => (recordTy (x, BOGty); useVal v; init k)
		  | APP(f, vs) => (useVal f; useVals vs)
		  | FIX _ => raise Fail "unexpected FIX"
		  | SWITCH(v, _, ks) => (
		      needsBasePtr := true;
		      useVal v; List.app init ks)
		  | BRANCH(_, vs, _, k1, k2) => (useVals vs; init k1; init k2)
		  | SETTER(_, vs, k) => (List.app useVal vs; init k)
		  | LOOKER(_, vs, x, ty, k) => (recordTy (x, ty); useVals vs; init k)
		  | ARITH(_, vs, x, ty, k) => (
		      bindVar x;
		      recordTy (x, ty); useVals vs; init k)
		  | PURE(P.WRAP(P.FLOAT _), vs, x, ty, k) => (
		      align 8; recordTy (x, ty); useVals vs; init k)
		  | PURE(rator, vs, x, ty, k) => (
		    (* check for pure operations that map to other forms in CFG *)
		      case rator
		       of P.MAKEREF => bindVar x
			| P.NEWARRAY0 => bindVar x
			| P.WRAP(P.INT sz) => bindVar x
			| P.WRAP(P.FLOAT sz) => (align 8; bindVar x)
			| P.RAWRECORD(SOME CPS.RK_RAW64BLOCK) => (
			    align 8; bindVar x)
			| P.RAWRECORD _ => bindVar x
			| _ => ()
		      (* end case *);
		      recordTy (x, ty); useVals vs; init k)
		  | RCC(_, _, _, vs, xs, k) => (
		      hasRCC := true;
		      List.app (bindVar o #1) xs;
		      List.app recordTy xs; useVals vs; init k)
		(* end case *))
	  in
	    init cexp;
	    Info{
		modes = modes, typs = typs,
		attrs = {
		    alignHP = !alignHP,
		    needsBasePtr = !needsBasePtr,
		    hasRCC = !hasRCC
		  }
	      }
	  end

    fun record desc = TP.RECORD{desc=desc, mut=false}
    fun mutRecord desc = TP.RECORD{desc=desc, mut=true}

    fun pureOp (oper, sz, args) = CFG.PURE(TP.PURE_ARITH{oper=oper, sz=sz}, args)

    fun signExtendTo (from, to, arg) =
	  CFG.PURE(TP.EXTEND{signed=true, from=from, to=to}, [arg])
    fun signExtend (from, arg) = signExtendTo (from, ity, arg)

    fun zeroExtendTo (from, to, arg) =
	  CFG.PURE(TP.EXTEND{signed=false, from=from, to=to}, [arg])
    fun zeroExtend (from, arg) = zeroExtendTo (from, ity, arg)

  (* get the descriptor of a heap object *)
    fun getDescriptor obj =
	  CFG.LOOKER(TP.RAW_LOAD{kind=TP.INT, sz=ity}, [obj, num'(~ws)])

  (* get length field of a heap object as tagged integer *)
    fun getObjLength obj =
	  pureOp (TP.ORB, ity, [
	      pureOp (TP.RSHIFTL, ity, [getDescriptor obj, w2Num(D.tagWidth - 0w1)]),
	      one
	    ])

  (* get the data pointer of a sequence (vector, array, string, ...) *)
    fun getSeqData obj = CFG.SELECT(0, obj)
  (* get the length field of a sequence *)
    fun getSeqLen obj = CFG.SELECT(1, obj)

  (* translate CPS RAWLOAD primop based on kind *)
    fun rawLoad (P.INT sz, args) = let
	  val load = CFG.LOOKER(TP.RAW_LOAD{kind=TP.INT, sz = sz}, args)
	  in
	    if (sz < ity)
		then signExtend (sz, load)
		else load
	  end
      | rawLoad (P.UINT sz, args) = let
	  val load = CFG.LOOKER(TP.RAW_LOAD{kind=TP.INT, sz = sz}, args)
	  in
	    if (sz < ity)
		then zeroExtend (sz, load)
		else load
	  end
      | rawLoad (P.FLOAT sz, args) = CFG.LOOKER(TP.RAW_LOAD{kind=TP.FLT, sz = sz}, args)

    fun rawStore (P.INT sz) = TP.RAW_STORE{kind=TP.INT, sz = sz}
      | rawStore (P.UINT sz) = TP.RAW_STORE{kind=TP.INT, sz = sz}
      | rawStore (P.FLOAT sz) = TP.RAW_STORE{kind=TP.FLT, sz = sz}

    fun gen info = let
	  val lookupMode = lookupMode info
	  val typeOfVar = typeOfVar info
	  val typeOf = typeOf info
	  val exps = LTbl.mkTable (numVars info, Fail "exps")
	  val binding = LTbl.lookup exps
	  val bind = LTbl.insert exps
	(* convert a CPS value to a CFG expression *)
	  fun genV (VAR x) = binding x
	    | genV (LABEL lab) = CFG.LABEL lab
	    | genV (NUM{ty={tag=true, ...}, ival}) = mlInt' ival
	    | genV (NUM{ty={sz, ...}, ival}) = szNum sz ival
	    | genV v = error ["unexepected ", PPCps.value2str v]
	  val genPureTagged = TaggedArith.pure genV
	  val genTagged = TaggedArith.trapping genV
	  fun genCont (cfgExp, x, ty, k) = (case lookupMode x
		 of TREE => (bind (x, cfgExp); genE k)
		  | BOUND => CFG.LET(cfgExp, (x, cvtTy ty), bindVarIn(x, k))
		(* end case *))
	  and bindVarIn (x, k) = (bind (x, CFG.VAR x); genE k)
	  and genE cexp = (case cexp
		 of RECORD(CPS.RK_VECTOR, flds, x, k) => let
		    (* A vector has a data record and a header record *)
		      val len = length flds
		      val dataDesc = D.makeDesc'(len, D.tag_vec_data)
		      val dataP = LV.mkLvar()
		      in
			allocRecord (dataDesc, flds, dataP,
			  CFG.ALLOC(record D.desc_polyvec, [CFG.VAR dataP, mlInt len], x,
			    bindVarIn(x, k)))
		      end
(* REAL32: FIXME *)
		  | RECORD(CPS.RK_FCONT, flds, x, k) => allocFltRecord (flds, x, k)
(* REAL32: FIXME *)
		  | RECORD(CPS.RK_RAW64BLOCK, flds, x, k) => allocFltRecord (flds, x, k)
		  | RECORD(CPS.RK_RAWBLOCK, flds, x, k) => allocIntRecord (flds, x, k)
		  | RECORD(_, flds, x, k) => allocRecord (
		      D.makeDesc' (length flds, D.tag_record),
		      flds, x, bindVarIn(x, k))
		  | SELECT(i, v, x, ty, k) =>
		      genCont (CFG.SELECT(i, genV v), x, ty, k)
		  | OFFSET(i, v, x, k) =>
		      genCont (CFG.OFFSET(i, genV v), x, BOGty, k)
		  | APP(LABEL lab, vs) =>
??
		  | APP(f, vs) =>
??
		  | FIX _ => error ["unexpected FIX"]
		  | SWITCH(v, _, cases) =>
		      CFG.SWITCH(genV v, List.map genE cases)
		  | BRANCH(test, vs, _, k1, k2) =>
		      genBranch (test, vs, genE k1, genE k2)
		  | SETTER(oper, vs, k) =>
		      genSetter (oper, vs, genE k)
		  | LOOKER(oper, vs, x, ty, k) =>
		      genCont (genLooker (oper, vs), x, ty, k)
		  | ARITH(P.TEST{from, to}, [v], x, ty, k) =>
		      if (from = ity) andalso (to = defaultIntSz)
			then let
			  val tmp = LV.mkLvar()
			  fun tag v =
				CFG.ARITH(TP.ARITH{oper=TP.IADD, sz=ity}, [v, v], (tmp, cfgITy),
				  genCont (pureOp(TP.ADD, ity, [CFG.VAR tmp, one]),
				    x, ty, k))
			  in
			    case genV v
			     of v' as CFG.VAR _ => tag v'
			      | v' as CFG.NUM _ => tag v'
			      | v' => let
				  val tmp' = LV.mkLvar()
				  in
				    CFG.LET(v', (tmp', cfgITy), tag (CFG.VAR tmp'))
				  end
			    (* end case *)
			  end
			else error ["unsupported sizes for TEST"]
		  | ARITH(P.TESTU{from, to}, [v], x, ty, k) =>
		      if (from = to) andalso ((from = ity) orelse (from = defaultIntSz))
			then let
			(* we implement the test by adding 2^(ity-1) to the value;
			 * if the value is negative (which fails the TESTU condition),
			 * then this addition will cause a trap.
			 *)
			  val dummy = LV.mkLvar()
			  val x' = CFG.VAR x
			  in
			    bind (x, x');
			    CFG.LET(genV v, (x, cvtTy ty),
			      CFG.ARITH(TP.ARITH{oper=TP.IADD, sz=ity}, [x', signBit],
				(dummy, cfgITy), genE k))
			  end
			else error ["unsupported sizes for TESTU"]
		  | ARITH(P.IARITH{oper, sz}, vs, x, ty, k) => if isTaggedInt sz
		      then let
			fun continue (CFG.VAR _) = bindVarIn (x, k)
			  | continue exp =
			      genCont(exp, x, CPS.NUMt{sz=defaultIntSz, tag=true}, k)
			in
			  genTagged (oper, vs, x, continue)
			end
		      else let
			fun arith (oper, vs) =
			      CFG.ARITH(TP.ARITH{oper=oper, sz=sz}, vs, (x, cvtTy ty),
				bindVarIn (x, k))
			in
			  case (oper, List.map genV vs)
			   of (P.IADD, args) => arith (TP.IADD, args)
			    | (P.ISUB, args) => arith (TP.ISUB, args)
			    | (P.IMUL, args) => arith (TP.IMUL, args)
			    | (P.IQUOT, args) => arith (TP.IDIV, args)
			    | (P.IREM, args) => arith (TP.IREM, args)
			    | (P.INEG, [a]) => arith (TP.ISUB, [szNum sz 0, a])
			    | _ => error ["bogus ", PPCps.arithopToString oper]
			  (* end case *)
			end
		(* handle pure operators that are actually allocations *)
		  | PURE(P.MAKEREF, [v], x, _, k) =>
		      CFG.ALLOC(mutRecord D.desc_ref, [genV v], x, bindVarIn(x, k))
		  | PURE(P.NEWARRAY0, [], x, _, k) => let
		      val dataP = LV.mkLvar()
		      in
			CFG.ALLOC(mutRecord D.desc_ref, [mlInt' 0], dataP,
			CFG.ALLOC(record D.desc_polyarr, [mlInt' 0, CFG.VAR dataP], x,
			  bindVarIn(x, k)))
		      end
		  | PURE(P.WRAP(P.INT sz), [v], x, _, k) => if (sz = ity)
			then let
			  val oper = TP.RAW_RECORD{
				  desc = D.makeDesc' (1, D.tag_raw),
				  kind = TP.INT,
				  sz = ity
				}
			  in
			    CFG.ALLOC(oper, [genV v], x, bindVarIn(x, k))
			  end
		      else if (sz < ity)
			then error ["wrap for tagged ints is not implemented"]
			else error ["wrap(INT ", Int.toString sz, ") is not implemented"]
		  | PURE(P.WRAP(P.FLOAT 32), [v], x, _, k) => (* REAL32: FIXME *)
		      error ["wrap for 32-bit floats is not implemented"]
		  | PURE(P.WRAP(P.FLOAT 64), [v], x, _, k) => let
		      val oper = TP.RAW_RECORD{
			      desc = D.makeDesc'(wordsPerDbl, D.tag_raw),
			      kind = TP.INT,
			      sz = 64
			    }
		      in
			CFG.ALLOC(oper, [genV v], x, bindVarIn(x, k))
		      end
		  | PURE(P.RAWRECORD rk, [NUM{ty={tag=true, ...}, ival}], x, _, k) =>
		      let
		      val n = Int.fromLarge ival (* number of elements *)
		      fun mkDesc (tag, n) = SOME(D.makeDesc' (n, tag))
		      val (desc, scale) = (case rk
			     of NONE => (NONE, MS.valueSize)
			      | SOME CPS.RK_FCONT =>
				  (mkDesc(D.tag_raw64, wordsPerDbl * n), MS.realSize)
			      | SOME CPS.RK_RAW64BLOCK =>
				  (mkDesc(D.tag_raw64, wordsPerDbl * n), MS.realSize)
			      | SOME CPS.RK_RAWBLOCK =>
				  (mkDesc(D.tag_raw, n), MS.valueSize)
			      | _ => error ["bogus raw record kind"]
			    (* end case *))
		      val len = n * scale
		      val oper = TP.RAW_ALLOC{desc = desc, align = scale, len = len}
		      in
			CFG.ALLOC(oper, [], x, bindVarIn(x, k))
		      end
		(* handle the non-allocating pure operators *)
		  | PURE(oper, vs, x, ty, k) =>
		      genCont (genPure (oper, vs), x, ty, k)
		  | RCC(reentrant, linkage, proto, args, results, k) =>
		      raise Fail "FIXME: RCC not implemented yet"
		(* end case *))
	(***** ALLOCATION *****)
	  and getField (v, CPS.OFFp 0) = genV v
	    | getField (v, p) = let
		fun getPath (v, CPS.OFFp n) = CFG.SELECT(n, v)
		  | getPath (v, CPS.SELp(n, CPS.OFFp 0)) = CFG.SELECT(n, v)
		  | getPath (v, CPS.SELp(n, p)) = getPath (CFG.SELECT(n, v), p)
		in
		  getPath (genV v, p)
		end
	  and allocRecord (desc, fields, x, k) =
		CFG.ALLOC(record desc, List.map getField fields, x, k)
(* REAL32: FIXME *)
	(* Allocate a record with real components *)
	  and allocFltRecord (fields, x, k) = let
		val desc = D.makeDesc'(wordsPerDbl * length fields, D.tag_raw64)
		val oper = TP.RAW_RECORD{desc = desc, kind = TP.FLT, sz = 64}
		in
		  CFG.ALLOC(oper, List.map getField fields, x, bindVarIn(x, k))
		end
	(* Allocate a record with machine-int-sized components *)
	  and allocIntRecord (fields, x, k) = let
		val desc = D.makeDesc' (length fields, D.tag_raw)
		val oper = TP.RAW_RECORD{desc = desc, kind = TP.INT, sz = 64}
		in
		  CFG.ALLOC(oper, List.map getField fields, x, bindVarIn(x, k))
		end
	(***** SETTER *****)
	  and genSetter (oper, args : value list, k) = (case (oper, args)
		 of (P.NUMUPDATE{kind}, [arr, ix, v]) => let
		      fun set oper = CFG.SETTER(
			    oper, [getSeqData(genV arr), untagSigned ix, genV v],
			    k)
		      in
			case kind
			 of P.INT sz => set (TP.RAW_UPDATE{kind = TP.INT, sz = sz})
			  | P.UINT sz => set (TP.RAW_UPDATE{kind = TP.INT, sz = sz})
			  | P.FLOAT sz => set (TP.RAW_UPDATE{kind = TP.FLT, sz = sz})
			(* end case *)
		      end
		  | (P.UNBOXEDUPDATE, [arr, ix, v]) =>
		      CFG.SETTER(TP.UNBOXED_UPDATE,
			[getSeqData(genV arr), untagSigned ix, genV v],
			k)
		  | (P.UPDATE, [arr, ix, v]) =>
		      CFG.SETTER(TP.UPDATE,
			[getSeqData(genV arr), untagSigned ix, genV v],
			k)
		  | (P.UNBOXEDASSIGN, [r, v]) =>
		      CFG.SETTER(TP.UNBOXED_ASSIGN, [genV r, genV v], k)
		  | (P.ASSIGN, [r, v]) =>
		      CFG.SETTER(TP.ASSIGN, [genV r, genV v], k)
		  | (P.SETHDLR, [v]) =>
		      CFG.SETTER(TP.SET_HDLR, [genV v], k)
		  | (P.SETVAR, [v]) =>
		      CFG.SETTER(TP.SET_VAR, [genV v], k)
		  | (P.SETSPECIAL, [v]) => let
		      fun set desc =
			    CFG.SETTER(
			      TP.RAW_STORE{kind=TP.INT, sz=ity},
			      [genV v, num'(~ws), desc],
			      k)
		      in
			case v
			 of NUM{ty={tag=true, ...}, ival} =>
			      set (num (D.makeDesc(ival, D.tag_special)))
			  | _ => set (pureOp(TP.ORB, ity, [
				pureOp(TP.LSHIFT, ity, [untagSigned v, w2Num D.tagWidth]),
				num D.desc_special
			      ]))
			(* end case *)
		      end
		  | (P.RAWSTORE{kind}, [adr, v]) =>
		      CFG.SETTER(rawStore kind, [genV adr, zero ity, genV v], k)
		  | (P.RAWSTORE{kind}, [adr, offset, v]) =>
		      CFG.SETTER(rawStore kind, [genV adr, genV offset, genV v], k)
		  | (P.RAWUPDATE(CPS.FLTt 64), [v, i, w]) =>
		      CFG.SETTER(
			TP.RAW_UPDATE{kind=TP.FLT, sz=64}, [genV v, genV i, genV w],
			k)
		  | (P.RAWUPDATE _, [v, i, w]) =>
		      CFG.SETTER(
			TP.RAW_UPDATE{kind=TP.INT, sz=ity}, [genV v, genV i, genV w],
			k)
		  | _ => error ["bogus setter: ", PPCps.setterToString oper]
		(* end case *))
	(***** LOOKER *****)
	  and genLooker (oper, args : value list) = (case (oper, args)
		 of (P.DEREF, _) => CFG.LOOKER(TP.DEREF, List.map genV args)
		  | (P.SUBSCRIPT, [arr, ix]) =>
		      CFG.LOOKER(TP.SUBSCRIPT, [
			  CFG.SELECT(0, genV arr), untagSigned ix
			])
		  | (P.NUMSUBSCRIPT{kind=P.INT sz}, [arr, ix]) =>
		      genRawIntSubscript (sz, arr, ix)
		  | (P.NUMSUBSCRIPT{kind=P.UINT sz}, [arr, ix]) =>
		      genRawWordSubscript (sz, arr, ix)
		  | (P.NUMSUBSCRIPT{kind=P.FLOAT sz}, [arr, ix]) =>
		      genRawSubscript (TP.FLT, sz, arr, ix)
		  | (P.GETSPECIAL, [obj]) => getObjLength (genV obj)
		  | (P.GETHDLR, _) => CFG.LOOKER(TP.GET_HDLR, List.map genV args)
		  | (P.GETVAR, _)=> CFG.LOOKER(TP.GET_VAR, List.map genV args)
		  | (P.RAWLOAD{kind}, [adr]) => rawLoad (kind, [genV adr, zero ity])
		  | (P.RAWLOAD{kind}, [adr, ix]) => rawLoad (kind, [genV adr, genV ix])
		(* end case *))
	(***** PURE *****)
	  and genPure (p, vs : value list) = (case (p, vs)
		 of (P.PURE_ARITH{oper, kind=P.INT sz}, _) =>
		      if isTaggedInt sz
			then genPureTagged (oper, true, sz, vs)
			else (case (oper, List.map genV vs)
			   of (P.NEG, [v]) => pureOp (TP.SUB, sz, [zero sz, v])
			    | (P.ADD, vs) => pureOp (TP.ADD, sz, vs)
			    | (P.SUB, vs) => pureOp (TP.SUB, sz, vs)
			    | (P.MUL, vs) => pureOp (TP.SMUL, sz, vs)
			    | (P.QUOT, vs) => pureOp (TP.SDIV, sz, vs)
			    | (P.REM, vs) => pureOp (TP.SREM, sz, vs)
			    | (P.LSHIFT, vs) => pureOp (TP.LSHIFT, sz, vs)
			    | (P.RSHIFT, vs) => pureOp (TP.RSHIFT, sz, vs)
			    | (P.RSHIFTL, vs) => pureOp (TP.RSHIFTL, sz, vs)
			    | (P.ORB, vs) => pureOp (TP.ORB, sz, vs)
			    | (P.XORB, vs) => pureOp (TP.XORB, sz, vs)
			    | (P.ANDB, vs) => pureOp (TP.ANDB, sz, vs)
			    | _ => error ["genPure: ", PPCps.pureToString p]
			  (* end case *))
		  | (P.PURE_ARITH{oper, kind=P.UINT sz}, _) =>
		      if isTaggedInt sz
			then genPureTagged (oper, false, sz, vs)
			else (case (oper, List.map genV vs)
			   of (P.NEG, [v]) => pureOp (TP.SUB, sz, [zero sz, v])
			    | (P.NOTB, [v]) => pureOp (TP.XORB, sz, [v, allOnes sz])
			    | (P.ADD, vs) => pureOp (TP.ADD, sz, vs)
			    | (P.SUB, vs) => pureOp (TP.SUB, sz, vs)
			    | (P.MUL, vs) => pureOp (TP.UMUL, sz, vs)
			    | (P.QUOT, vs) => pureOp (TP.UDIV, sz, vs)
			    | (P.REM, vs) => pureOp (TP.UREM, sz, vs)
			    | (P.LSHIFT, vs) => pureOp (TP.LSHIFT, sz, vs)
			    | (P.RSHIFT, vs) => pureOp (TP.RSHIFT, sz, vs)
			    | (P.RSHIFTL, vs) => pureOp (TP.RSHIFTL, sz, vs)
			    | (P.ORB, vs) => pureOp (TP.ORB, sz, vs)
			    | (P.XORB, vs) => pureOp (TP.XORB, sz, vs)
			    | (P.ANDB, vs) => pureOp (TP.ANDB, sz, vs)
			    | _ => error ["genPure: ", PPCps.pureToString p]
			  (* end case *))
		  | (P.PURE_ARITH{oper, kind=P.FLOAT sz}, _) => (
		      case (oper, List.map genV vs)
		       of (P.ADD, args) => pureOp (TP.FADD, sz, args)
			| (P.SUB, args) => pureOp (TP.FSUB, sz, args)
			| (P.MUL, args) => pureOp (TP.FMUL, sz, args)
			| (P.FDIV, args) => pureOp (TP.FDIV, sz, args)
			| (P.FABS, args) => pureOp (TP.FABS, sz, args)
			| (P.FSQRT, args) => pureOp (TP.FSQRT, sz, args)
			| _ => error ["genPure: ", PPCps.pureToString p]
		      (* end case *))
		  | (P.PURE_NUMSUBSCRIPT{kind=P.INT sz}, [v1, v2]) =>
		      genRawIntSubscript (sz, v1, v2)
		  | (P.PURE_NUMSUBSCRIPT{kind=P.UINT sz}, [v1, v2]) =>
		      genRawWordSubscript (sz, v1, v2)
		  | (P.PURE_NUMSUBSCRIPT{kind=P.FLOAT sz}, [v1, v2]) =>
		      genRawSubscript (TP.FLT, sz, v1, v2)
		  | (P.LENGTH, [v]) => getSeqLen (genV v)
		  | (P.OBJLENGTH, [v]) => getObjLength (genV v)
		  | (P.COPY{from, to}, [v]) =>
		      if (from = to)
			then genV v
		      else if (from = defaultIntSz) andalso (to = ity)
			then untagUnsigned v
		      else if (from < defaultIntSz)
			then if (to <= defaultIntSz)
			  then zeroExtend(from, genV v)
(* QUESTION: do we need to zero extend v before untagging it? *)
			  else untagUnsigned v
			else error [".genPure: ", PPCps.pureToString p]
		  | (P.EXTEND{from, to}, [v]) =>
		      if (from = to)
			then genV v
		      else if (from = defaultIntSz) andalso (to = ity)
			then pureOp (TP.RSHIFT, ity, [genV v, one])
		      else if (from < defaultIntSz)
			then let
			(* shift left so that sign bit is leftmost bit *)
			  val exp = pureOp (TP.LSHIFT, ity, [genV v, num'(defaultIntSz - from)])
			  in
			    if (to <= defaultIntSz)
			      then addTag (
				pureOp (TP.RSHIFT, ity, [exp, num'(ity - from)]),
				num'(defaultIntSz - from)
			      ]))
			else pureOp(TP.EXTEND{from=from, to=to}, [genV v]
		  | (P.TRUNC{from, to}, [v]) =>
??
		  | (P.INT_TO_REAL{from, to}, [v]) => let
		      val e = if isTaggedInt from
			    then untagSigned v
			    else genV v
		      in
			CFG.PURE(TP.INT_TO_REAL{from=ity, to=to}, [e])
		      end
		  | (P.SUBSCRIPTV, [v1, v2]) =>
		      CFG.PURE(TP.PURE_SUBSCRIPT, [
			  getSeqData (genV v1),
			  untagSigned v2
			])
		  | (P.GETTAG, [v]) =>
		      toMLWord (pureOp (TP.ANDB, ity, [
			  getDescriptor(genV v),
			  num(D.powTagWidth-1)
			]))
		  | (P.CAST, [v]) => genV v
		  | (P.GETCON, [v]) => CFG.SELECT(0, genV v)
		  | (P.GETEXN, [v]) => CFG.SELECT(0, genV v)
		  | (P.BOX, [v]) => genV v	(* does this operation ever occur? *)
		  | (P.UNBOX, [v]) => genV v	(* does this operation ever occur? *)
		  | (P.UNWRAP(P.INT sz), [v]) =>
		      CFG.LOOKER(TP.RAW_LOAD{sz=sz, kind=TP.INT}, [genV v, zero ity])
		  | (P.UNWRAP(P.FLOAT sz), [v]) =>
		      CFG.LOOKER(TP.RAW_LOAD{sz=sz, kind=TP.FLT}, [genV v, zero ity])
		  | (P.GETSEQDATA, [v]) => getSeqData (genV v)
		  | (P.RECSUBSCRIPT, [v1, NUM{ty={tag=true, ...}, ival}]) =>
		      CFG.SELECT(IntInf.toInt ival, genV v1)
		  | (P.RECSUBSCRIPT, [v1, v2]) =>
		      CFG.PURE(TP.PURE_SUBSCRIPT, [genV v1, untagSigned v2])
		  | (P.RAW64SUBSCRIPT, [v1, v2]) =>
(* REAL32: FIXME *)
		      CFG.PURE(TP.PURE_RAW_SUBSCRIPT{kind=TP.FLT, sz=64},
			[genV v1, untagSigned v2])
		  | _ => error[".genPure: ", PPCps.pureToString p]
		(* end case *))
	(***** BRANCH *****)
	  and genBranch (test, args, k1, k2) = let
		fun mkBr test' = CFG.BRANCH(test', List.map genV args, unkProb, k1, k2)
	      (* translate a boxity test *)
		fun boxedTest (v, kBoxed, kUnboxed) =
		      CFG.BRANCH(
			TP.CMP{oper=P.EQL, signed=false, sz=ity},
			  [pureOp(TP.ANDB, ity, [v, one]), zero ity],
			unkProb,
			kBoxed,
			kUnboxed)
		in
		  case (test, args)
		   of (P.CMP{oper, kind=CPS.P.INT sz}, _) =>
			mkBr (TP.CMP{oper=oper, signed=true, sz=normSz sz})
		    | (P.CMP{oper, kind=CPS.P.UINT sz}, _) =>
			mkBr (TP.CMP{oper=oper, signed=false, sz=normSz sz})
		    | (P.FCMP{oper, size}, _) =>
			mkBr (TP.FCMP{oper=oper, sz=size})
		    | (P.FSGN sz, _) => mkBr (TP.FSGN sz)
		    | (P.BOXED, [v]) => boxedTest (genV v, k1, k2)
		    | (P.UNBOXED, [v]) => boxedTest (genV v, k2, k1)
		    | (P.PEQL, _) => mkBr TP.PEQL
		    | (P.PNEQ, _) => mkBr TP.PNEQ
		    | _ => error [".branch: bogus test ", PPCps.branchToString test]
		  (* end case *)
		end
	(* subscript from packed numeric vector *)
	  and genRawSubscript (kind, sz, vec, idx) =
		CFG.PURE(TP.PURE_RAW_SUBSCRIPT{kind=kind, sz=sz}, [
		    getSeqData(genV vec), untagSigned idx
		  ])
	  and genRawIntSubscript (sz, vec, idx) =
		if (sz < defaultIntSz)
		  then toMLWord (signExtend (sz, genRawSubscript (TP.INT, sz, vec, idx)))
		else if (sz = defaultIntSz)
		(* for default-size ints, we use the native size subscript *)
		  then toMLWord (genRawSubscript (TP.INT, ity, vec, idx))
		  else genRawSubscript (TP.INT, sz, vec, idx)
	  and genRawWordSubscript (sz, vec, idx) =
		if (sz < defaultIntSz)
		  then toMLWord (zeroExtend (sz, genRawSubscript (TP.INT, sz, vec, idx)))
		else if (sz = defaultIntSz)
		(* for default-size ints, we use the native size subscript *)
		  then toMLWord (genRawSubscript (TP.INT, ity, vec, idx))
		  else genRawSubscript (TP.INT, sz, vec, idx)
	  and untagSigned (NUM{ival, ...}) = num ival
	    | untagSigned v = pureOp (TP.RSHIFT, ity, [genV v, one])
	  and untagUnsigned (NUM{ival, ...}) = num ival
	    | untagUnsigned v = pureOp(TP.RSHIFTL, ity, [genV v, one])
	(* convert a raw integer value to a tagged integer w/o trapping *)
	  and toMLWord exp = (* `(exp << 1) + 1` *)
		pureOp(TP.ADD, ity, [pureOp(TP.LSHIFT, ity, [exp, one]), one])
	  in
	    genE
	  end

  (* convert a single cluster of CPS functions to a CFG cluster. *)
    fun doCluster (entryFn :: frags) = let
	  in
??
	  end

    fun translate {source, funcs, limits} = let
	  in
??
	  end

  end
