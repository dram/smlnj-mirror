(* cps-to-cfg.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

functor CPSToCFGFn (MS : MACH_SPEC) : sig

    val translate :  : {
	    source : string,
	    funcs : CPS.function list,
	    limits :  CPS.lvar -> int * int,
	  } -> {
	    source : string,
	    funcs : CFG.cluster list
	  }

  end = struct

    structure LV = LambdaVar
    structure LTbl = LV.Tbl
    structure LMap = LV.Map

  (* source primops *)
    structure SP = CPS.P
  (* target primops *)
    structure TP = CFG_Prim
  (* object descriptors *)
    structure D  = MS.ObjDesc

    datatype value = datatype CPS.value
    datatype cexp = datatype CPS.cexp

  (* pointers to unknown objects *)
    val BOGty = CPSUtil.BOGt

  (* CFG integer constants *)
    fun sNUM iv = CFG.NUM{iv = iv, signed = true, sz = ity}
    fun uNUM iv = CFG.NUM{iv = iv, signed = false, sz = ity}
    fun w2NUM iv = uNUM(Word.toLargeInt iv)
    val one = sNUM 1

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
      }

    fun numVars (Info{modes, ...}) = LTbl.numItems modes

(* TODO: other properties that we may want to compute:
 * hasFloats (for alignment) and needsBasePtr (for computing label values)
 *)

  (* initialize a table that maps variables to use modes for a CPS expression *)
    fun initUseModes cexp = let
	  val modes = LTbl.mkTable(32, Fail "modes")
	  fun getMode = LTbl.find modes
	  fun setMode = LTbl.insert modes
	  fun useVar x = (case getMode x
		 of NONE => setMode (x, TREE)
		  | SOME TREE => setMode (x, BOUND)
		  | _ => ()
		(* end case *))
	  fun useVal (VAR x) = useVar x
	    | useVal _ => ()
	  val useVals = List.app useVal
	  val typs = LTbl.mkTable(32, Fail "typs")
	  val recordTy = LTbl.insert typs
	  val needsBasePtr = ref false
	  fun init cexp = (case cexp
		 of RECORD(_, flds, x, k) => let
		      fun useField (VAR x, _) = useVar x
			| useField (LABEL l, _) = (needsBasePtr := true)
			| useField _ = ()
		      in
		        recordTy (x, BOGty);
			List.app useField flds;
			init k)
		  | SELECT(_, v, x, ty, k) => (recordTy (x, ty); useVal v; init k)
		  | OFFSET(_, v, x, k) => (recordTy (x, BOGty); useVal v; init k)
		  | APP(f, vs) => (addVal f; addVals vs)
		  | FIX _ => raise Fail "unexpected FIX"
		  | SWITCH(v, _, ks) => (
		      needsBasePtr := true; useVal v; List.app init ks)
		  | BRANCH(_, vs, _, k1, k2) => (useVals vs; init k1; init k2)
		  | SETTER(_, vs, k) => (List.app useVal vs; init k)
		  | LOOKER(_, vs, x, ty, k) => (recordTy (x, ty); useVals vs; init k)
		  | ARITH(_, vs, x, ty, k) => (recordTy (x, ty); useVals vs; init k)
		  | PURE(_, vs, x, ty, k) => (recordTy (x, ty); useVals vs; init k)
		  | RCC(_, _, _, vs, xs, k) => (
		      List.app recordTy xs; addVals vs; init k)
		(* end case *))
	  in
	    init cexp;
	    Info{ modes = modes, typs = typs }
	  end

    fun gen info = let
	  val exps = LTbl.mkTable (numVars info, Fail "exps")
	  val binding = LTbl.lookup exps
	  val bind = LTbl.insert exps
	  fun genE cexp = (case cexp
		 of RECORD(rk, flds, x, k) =>
		      allocRecord (rk, flds. x, fn stm => gen(k, stm::stms))
		  | SELECT(i, v, x, ty, k) =>
		      gen (bind(exps, x, ty, CFG.SELECT(i, lookup(exps, v))), k, stms)
		  | OFFSET of int * value * lvar * cexp
		  | APP(f, vs) =>
		  | FIX _ => raise Fail "unexpected FIX"
		  | SWITCH of value * lvar * cexp list
		  | BRANCH of P.branch * value list * lvar * cexp * cexp
		  | SETTER of P.setter * value list * cexp
		  | LOOKER of P.looker * value list * lvar * cty * cexp
		  | ARITH(p, vs, x, ty, k) =>
		(* handle pure operators that are actually allocations *)
		  | PURE(P.MAKEREF, [v], x, _, k) =>
		  | PURE(P.NEWARRAY0, [], x, _, k) =>
		  | PURE(P.WRAP(P.INT sz), [v]) =>
		  | PURE(P.WRAP(P.FLOAT sz), [v]) =>
		  | PURE(P.RAWRECORD rk, vs, x, _, k) =>
		(* handle the non-allocating pure operators *)
		  | PURE(rator, vs, x, _, k) =>
		      bindInExp (genPure (rator, List.map genVal vs), x, k)
		  | RCC of rcc_kind * string * CTypes.c_proto * value list *
		(* end case *))
	  and genPure (p, vs) = (case p
		 of (P.PURE_ARITH{oper, kind=P.INT sz}, _) =>
		      if isTaggedInt sz
			then PureArith.tagged (fn v => genE (trees, v)) (oper, vs)
			else PureArith.signed (fn v => genE (trees, v)) (oper, vs)
		  | (P.PURE_ARITH{oper, kind=P.UINT sz}, _) =>
		      if isTaggedInt sz
			then PureArith.tagged (fn v => genE (trees, v)) (oper, vs)
			else PureArith.unsigned (fn v => genE (trees, v)) (oper, vs)
		  | (P.PURE_ARITH{oper, kind=P.FLOAT sz}, _) =>
		      PureArith.float (fn v => genE (trees, v)) (oper, vs)
		  | (P.PURE_NUMSUBSCRIPT{kind=P.INT sz}, [v1, v2]) =>
		  | (P.PURE_NUMSUBSCRIPT{kind=P.UINT sz}, [v1, v2]) =>
		  | (P.PURE_NUMSUBSCRIPT{kind=P.FLOAT sz}, [v1, v2]) =>
		  | (P.LENGTH, [v]) =>
		      CFG.PURE(TP.LOAD_WORD(1, tagIntCTy), [genE v])
		  | (P.OBJLENGTH, [v]) =>
		      CFG.PURE(pOp(TP.RSHIFTL, [
			  getDescriptor v,
			  w2NUM(D.tagWidth - 0w1)
			])
		  | (P.COPY{from, to}, [v]) =>
		  | (P.EXTEND{from, to}, [v]) =>
		  | (P.TRUNC{from, to}, [v]) =>
		  | (P.INT_TO_REAL{from, to}, [v]) => let
		      val e = if isTaggedInt sz
			    then genE (trees, unTagSignedVal v)
			    else genE (trees, v)
		      in
			CFG.PURE(TP.INT_TO_REAL{from=ity, to=to}, [e])
		      end
		  | (P.SUBSCRIPTV, [v1, v2]) =>
		  | (P.GETTAG, [v]) =>
		      tagUnsigned(pOp(TP.ANDB, [getDescriptor v, uNUM(D.powTagWidth-1)]))
		  | (P.CAST, [v]) => genE v
		  | (P.GETCON, [v]) => CFG.PURE(TP.LOAD_WORD(0, intCTy), [genE v])
		  | (P.GETEXN, [e]) => CFG.PURE(TP.LOAD_WORD(0, intCTy), [genE v])
		  | (P.BOX, [v]) => genE v	(* does this operation ever occur? *)
		  | (P.UNBOX, [v]) => genE v	(* does this operation ever occur? *)
		  | (P.UNWRAP(P.INT sz), [v]) => CFG.PURE(CFG.P.IUNWRAP sz, [genE v])
		  | (P.UNWRAP(P.FLOAT sz), [v]) => CFG.PURE(CFG.P.FUNWRAP sz, [genE v])
		  | (P.GETSEQDATA, [v]) => CFG.PURE(TP.LOAD_WORD(0, intCTy), [genE v])
		  | (P.RECSUBSCRIPT, [v1, v2]) =>
		  | (P.RAW64SUBSCRIPT, [v1, v2]) =>
		  | _ => error("genPure: ", PPCps.pureopToString rator)
		(* end case *))
	  and getDescriptor v = CFG.PURE(TP.LOAD_WORD(~1, intCTy), [genE v])
	  and unTagSignedVal (NUM{ival, ...}) = sNUM ival
	    | unTagSignedVal v = pureOp(TP.RSHIFT, genE v, one)
	  and unTagUnsignedVal (NUM{ival, ...}) = uNUM ival
	    | unTagUnsignedVal v = pureOp(TP.RSHIFTL, genE v, one)
	  and bindInExp (cfgExp, x, k) = (case modeOf x
		 of TREE => (bind (x, cfgExp); gen k)
		  | BOUND => (
		      bind (x, CFG.VALUE(CFG.VAR x));
		      CFG.LET(cfgExp, x, gen k))
		(* end case *))
	  and genVal (VAR x) = binding x
	    | genVal (LABEL l) = ??
	    | genVal (NUM{ty, ival}) = if isTaggedInt sz
		then sNUM(ival+ival+1)
		else CFG.NUM{iv = ival, signed = true, sz = ty}
	    | genVal _ = raise Fail "unexpected value"
	  in
	  end

  (* convert a single cluster of CPS functions to a CFG cluster. *)
    fun doCluster (entryFn :: frags) = let
	  in
	  end

    fun translate {source, funcs, limits} = let
	  in
	  end

  end
