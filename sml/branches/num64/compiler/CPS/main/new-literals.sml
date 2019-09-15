(* new-literals.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This file implements support for heap-allocated literals.  Our approach
 * is to split out the literals from the CPS representation and create a
 * bytecode program that the runtime execures to allocate the literals.
 *
 * The implementation of the bytecode interpreter for the literal language
 * is in base/runtime/gc/build-literals.c.  A description of the bytecode
 * language is in dev-notes/new-literals.md.
 *)

signature LITERALS =
  sig

  (** `litsplit f` takes a CPS function and splits out the heap-allocated
   * literal values from it.  At runtime, these literals will be accessed via
   * a record of literals that is allocated by the runtime system.  This
   * function returns a rewriten version of its argument that accesses
   * literals from the record and a byte-vector that encodes the program
   * for generating the literals.
   *)
    val split : CPS.function -> CPS.function * Word8Vector.vector

  end;

structure NewLiterals : LITERALS =
  struct

    structure W8V = Word8Vector
    structure LV = LambdaVar
    structure IntTbl = IntHashTable
    structure C = CPS

    fun bug msg = ErrorMsg.impossible ("Literals: "^msg)

  (****************************************************************************
   *                         A MINI-LITERAL LANGUAGE                          *
   ****************************************************************************)

    datatype lit_val
      = LI_INT of IntInf.int	(* default-size tagged integer literal *)
      | LI_INT32 of IntInf.int	(* boxed 32-bit integer literal on 32-bit target *)
      | LI_INT64 of IntInf.int	(* boxed 64-bit integer literal on 64-bit target *)
(* REAL32: add LI_REAL32 *)
      | LI_REAL64 of RealLit.t	(* 64-bit real literal *)
      | LI_STRING of string
      | LI_VAR of lvar

    datatype lit_exp
      = LI_TOP of lit_val list
      | LI_RECORD of lit_val list * lvar * lit_exp
      | LI_VECTOR of lit_val list * lvar * lit_exp
      | LI_RAWBLOCK of IntInf.int list * lvar * lit_exp
      | LI_F64BLOCK of RealLit.t list * lvar * lit_exp

    fun val2lit (CPS.VAR v) = LI_VAR v
      | val2lit (CPS.NUM{ival, ty={tag=true, ...}}) = LI_INT ival
      | val2lit (CPS.NUM{ival, ty={sz=32, tag=false}}) = LI_INT32 ival
      | val2lit (CPS.NUM{ival, ty={sz=64, tag=false, ...}}) = LI_INT64 ival
      | val2lit (CPS.STRING s) = LI_STRING s
      | val2lit _ = bug "unexpected case in val2lit"

 (* the main function *)
    fun split (fk, f, vl as [_,x], [CNTt, t as PTRt(RPT n)], body) = let
	  val nt = PTRt(RPT (n+1))
	  val (nbody, lit) = liftlits(body, VAR x, n)
	  in
	    ((fk, f, vl, [CNTt, nt], nbody), litToBytes lit)
	  end
      | split _ = bug "unexpected CPS header in split"

  (****************************************************************************
   *                 TRANSLATING THE LITERAL EXP TO BYTES                     *
   ****************************************************************************)

  (* Literals are encoded as instructions for a "literal machine."  The abstract
   * description of these instructions is given in dev-notes/new-literals.md
   *)

  (* `INT` opcodes *)
    fun opINT_0_10 n = Word8.fromLargeInt n
    fun opINT_m5_m1 n = Word8.fromLargeInt(0x0A - n)
    val opINTb : Word8.word = 0wx10
    val opINTh : Word8.word = 0wx11
    val opINTw : Word8.word = 0wx12
    val opINTlw : Word8.word = 0wx13
  (* `INT32` opcodes *)
    val opINT32b : Word8.word = 0wx14
    val opINT32h : Word8.word = 0wx15
    val opINT32w : Word8.word = 0wx16
  (* `INT64` opcodes *)
    val opINT64b : Word8.word = 0wx17
    val opINT64h : Word8.word = 0wx18
    val opINT64w : Word8.word = 0wx19
    val opINT64lw : Word8.word = 0wx1A
  (* record opcodes *)
    fun opRECORD_1_7 len = Word8.fromInt(0x30 + len)
    val opRECORDb: Word8.word = 0wx37
    val opRECORDh: Word8.word = 0wx38
  (* vector opcodes *)
    val opVECTORb: Word8.word = 0wx39
    val opVECTORh: Word8.word = 0wx3A
  (* return *)
    val opRETURN : Word8.word = 0wxff

  (* encode 8-bit signed value as a byte list *)
    fun toBytes8 (n, l) = Word8.fromLargeInt n :: l
  (* encode 16-bit signed value as a byte list *)
    fun toBytes16 (n, l) =
	  Word8.fromLargeInt(IntInf.~>>(n, 0w8)) ::
	  Word8.fromLargeInt n :: l
  (* encode 32-bit signed value as a byte list *)
    fun toBytes32 (n, l) =
	  Word8.fromLargeInt(IntInf.~>>(n, 0w24)) ::
	  Word8.fromLargeInt(IntInf.~>>(n, 0w16)) ::
	  toBytes16 (n, l)
  (* encode a 64-bit signed value as a byte list *)
    fun toBytes64 (n, l) =
	  Word8.fromLargeInt(IntInf.~>>(n, 0w56)) ::
	  Word8.fromLargeInt(IntInf.~>>(n, 0w48)) ::
	  Word8.fromLargeInt(IntInf.~>>(n, 0w40)) ::
	  Word8.fromLargeInt(IntInf.~>>(n, 0w32)) ::
	  toBytes32 (n, l)
    fun toBytes32' (n, l) = toBytes32 (IntInf.fromInt n, l)

(* 64BIT: assumption about default int size here *)
    fun intToBytes n = toBytes32' (n, [])
    fun strToBytes s = map Byte.charToByte (explode s)

  (* encode the literal header block *)
    fun encHeader {maxstk, maxsaved} = W8V.fromList (
	  0wx20 :: 0wx17 :: 0wx10 :: 0wx31 ::
	  toBytes32 (Int.toLarge maxstk,
	  toBytes32 (Int.toLarge Target.mlValueSz,
	  toBytes32 (Int.toLarge maxsaved, []))))

  (* encode tagged integers *)
    fun encINT (n, l) = if (0 <= n) andalso (n <= 10)
	    then opINT_0_10 n :: l
	  else if (~5 <= n) andalso (n < 0)
	    then opINT_m5_m1 n :: l
	  else if (~128 <= n) andalso (n <= 127)
	    then opINTb :: toBytes8 (n, l)
	  else if (~32768 <= n) andalso (n <= 32767)
	    then opINTh :: toBytes16 (n, l)
	  else if (~2147483648 <= n) andalso (n <= 2147483647)
	    then opINTw :: toBytes32 (n, l)
	    else opINTlw :: toBytes64 (n, l)

  (* encode 32-bit boxed integers *)
    fun encINT32 (n, l) = if (~128 <= n) andalso (n <= 127)
	    then opINT32b :: toBytes8 (n, l)
	  else if (~32768 <= n) andalso (n <= 32767)
	    then opINT32h :: toBytes16 (n, l)
	    else opINT32w :: toBytes32 (n, l)

  (* encode 64-bit boxed integers *)
    fun encINT64 (n, l) = if (~128 <= n) andalso (n <= 127)
	    then opINT64b :: toBytes8 (n, l)
	  else if (~32768 <= n) andalso (n <= 32767)
	    then opINT64h :: toBytes16 (n, l)
	  else if (~2147483648 <= n) andalso (n <= 2147483647)
	    then opINT64w :: toBytes32 (n, l)
	    else opINT64lw :: toBytes64 (n, l)

  (* encode a RECORD opcode and length *)
    fun encRECORD (len, l) = if (len <= 7)
	    then opRECORD_1_7 len :: l
	  else if (len <= 255)
	    then opRECORDb :: toBytes8 (len, l)
	  else if (len <= 65535)
	    then opRECORDh :: toBytes16 (len, l)
	    else bug "record too big"

  (* encode a VECTOR opcode and length *)
    fun encVECTOR (len, l) = if (len <= 255)
	    then opVECTORb :: toBytes8 (len, l)
	  else if (len <= 65535)
	    then opVECTORh :: toBytes16 (len, l)
	    else bug "vector too big"

    fun litToBytes (LI_TOP[]) = W8V.fromList[opRETURN]
      | litToBytes litExp = let
	(* compute the maximum stack depth required *)
	  fun depth (LI_TOP ls, d, maxDepth) = Int.max(maxDepth, d+length ls)
	    | depth (LI_RECORD(ls, _, rest), d, maxDepth) =
		depth (rest, d+1, Int.max(maxDepth, d+length ls))
	    | depth (LI_VECTOR(ls, _, rest), d, maxDepth) =
		depth (rest, d+1, Int.max(maxDepth, d+length ls))
	    | depth (LI_RAWBLOCK(ls, _, rest), d, maxDepth) =
		depth (rest, d+1, Int.max(maxDepth, d+length ls))
	    | depth (LI_F64BLOCK(ls, _, rest), d, maxDepth) =
		depth (rest, d+1, Int.max(maxDepth, d+length ls))
	  fun emitLitExp (env, exp, code) = let
		fun emitLitVals ([], _, code) = code
		  | emitLitVals (lit::r, d, code) = let
		      val instr = (case lit
			     of (LI_INT i) => emit_INT i
			      | (LI_STRING s) => emit_STR s
			      | (LI_VAR v) => let
				  fun f ([], _) = bug "unbound lvar"
				    | f (v'::r, d) = if (v = v') then d else f(r, d+1)
				  in
				    emit_LIT(f (env, d))
				  end
			    (* end case *))
		      in
			emitLitVals (r, d+1, instr::code)
		      end
		fun emitRawBlock (ls, code) = emit_RAW ls :: code
		fun emitF64Block (ls, code) = let
		      val toBits = #1 o Real64ToBits.toBits
		      in
		        emit_RAW64(map toBits ls) :: code
		      end
		in
		  case exp
		   of (LI_TOP ls) => emit_RETURN :: emitBlock(LI_RECORD, ls, code)
		    | (LI_RECORD(ls, v, rest)) =>
			emitLitExp (v::env, rest,
			  emit_RECORD(length ls, emitLitVals(ls, 0, code)))
		    | (LI_VECTOR(ls, v, rest)) =>
			emitLitExp (v::env, rest,
			  emit_VECTOR(length ls, emitLitVals(ls, 0, code)))
		    | (LI_RAWBLOCK(ls, v, rest)) =>
			emitLitExp (v::env, rest, emitRawBlock(ls, code))
		    | (LI_F64BLOCK(ls, v, rest)) =>
			emitLitExp (v::env, rest, emitF64Block(ls, code))
		  (* end case *)
		end
	  val maxDepth = depth (litExp, 0, 1)
	  val code = encHeader {maxstk=maxDepth, maxsaved=0}
		:: emit_DEPTH maxDepth
		:: List.rev(emitLitExp([], litExp, [opRETURN]))
	  in
	    W8V.concat code
	  end

  (****************************************************************************
   *                    LIFTING LITERALS ON CPS                               *
   ****************************************************************************)

    datatype literal_value
      = LV_NUM of int IntConst.t		(* boxed number of given size *)
      | LV_REAL of int RealConst.t		(* real number of given size *)
      | LV_STR of string			(* string *)
      | LV_RECORD of record_kind * literal list	(* record/vector/raw record *)

    and literal
      = LIT of {			(* heap-allocated literal value *)
	    refCnt : int ref,		(* count of uses of this literal value from other
					 * literals; when > 1, then we have shared structure. *)
	    useCnt : int ref,		(* count of uses of this literal.  When this count
					 * is > refCnt, then the literal will need to be bound
					 * to a variable in the residual program.
					 *)
	    id : word,			(* unique ID *)
	    value : literal_value	(* value *)
	  }
      | IMMED of IntInt.int		(* immediate tagged number *)

    fun useLit (LIT{useCnt, ...}) = useCnt := !useCnt + 1
      | useLit (IMMED _) = ()
    fun refLit (LIT{refCnt, ...}) = refCnt := !refCnt + 1
      | refLit (IMMED _) = ()

  (* is a literal used as value outside of being part of another literal? *)
    fun litIsUsed (LIT{refCnt, useCnt, ...}) = (!refCnt < !useCnt)

  (* an environment for tracking literals *)
    structure LitEnv : sig

	type t

      (* create a new environment *)
	val new : unit -> t
      (* add a literal_value to the environment *)
	val add : t -> literal_value -> literal
      (* return the literal that a variable is bound to *)
	val findVar : t -> C.lvar -> literal option
      (* is a value representable as a literal? *)
	val isConst : t -> value -> bool
      (* record the use of a value in a non-literal context *)
	val useValue : t -> value -> unit
      (* like useValue, but for values embedded in literal records.  This function
       * returns the literal that the value maps to.
       *)
	val useValue' : t -> value -> literal option
      (* add a literal record value to the environment *)
	val addRecord : t -> C.record_kind * literal list * C.lvar -> unit
      (* return true if there are no literals defined in the environment *)
	val isEmpty : t -> bool
      (* return a list of all of the literals defined in the environment *)
	val allLits : t -> literal list

      end = struct

	fun hashLV (LV_NUM{ty, ival}) = Word.fromLargeInt ival + ??
	  | hashLV (LV_REAL{ty, rval}) = RealLit.hash rval + ??
	  | hashLV (LV_STR s) = HashString.hashString s + ??
	  | hashLV (LV_RECORD(rk, lits)) = let
	      fun f (LIT{id, ...}, h) = 0w3 * id + ??
		| f (IMMED n, h) = Word.fromLargeInt n + ??
	      val h0 = (case rk
		     of RK_VECTOR =>
		      | RK_RECORD =>
		      | RK_RAWBLOCK =>
		      | _ => bug("unexpected record kind " ^ PPCps.rkstring rk)
		    (* end case *))
	      in
		List.foldl f h0 lits
	      end

	fun sameLV (LV_NUM{ty=ty1, ival=iv1}, LV_NUM{ty=ty2, ival=iv2}) =
	      (ty1 = ty2) andalso (iv1 = iv2)
	  | sameLV (LV_REAL{ty=ty1, rval=rv1}, LV_REAL{ty=ty2, rval=rv2}) =
	      (ty1 = ty2) andalso RealLit.same(rv1, rv2)
	  | sameLV (LV_STR s1, LV_STR s2) = (s1 = s2)
	  | sameLV (LV_RECORD(rk1, lits1), LV_RECORD(rk2, lvs2)) =
	      (rk1 = rk2) andalso ListPair.allEq sameLit (lvs1, lvs2)
	  | sameLV _ = false

	and sameLit (LIT{useCnt=u1, ...}, LIT{useCnt=u2, ...}) = (u1 = u2)
	  | sameLit (IMMED n1, IMMED n2) = (n1 = n2)
	  | sameLit _ = false

	structure LTbl = HashTableFn(
	  struct
	    type hash_key = literal_value
	    val hashVal = hashLV
	    val sameKey = sameLV
	  end)

      (* a variable that is bound to a literal is either used to build a literal
       * record, in which case the bool is false, or is used as an argument to
       * some other operation (including non-literal records).
       *)
	type var_info = bool * literal

	datatype t = LE of {
	    lits : literal LTbl.hash_table,	(* table of unique literals in the module *)
	    vMap : var_info IntTbl.hash_table	(* map from variables to the literals that they *)
						(* are bound to *)
	  }

	fun new () = LE{
		lits = LTbl.mkTable(32, Fail "LitTbl"),
		vMap = IntTbl.mkTable(32, Faile "VarTbl")
	      }

	fun add lits = let
	      val find = LTbl.find lits
	      val insert = LTbl.insert lits
	      in
		fn lv => (case find lv
		     of SOME lit => lit
		      | NONE => let
			  val lit = Lit{
				  useCnt = ref 0,
				  refCnt = ref 0,
				  id = Word.fromInt(LTbl.numItems tbl),
				  value = lv
				}
			  in
			    LTbl.insert lits (lv, lit);
			    lit
			  end
		    (* end case *)
	      end

	local
	  fun addLiteral wrap (LE{lits, ...}) = let
		val addL = add lits
		in
		  fn v => addL (wrap v)
		end
	in
	val addNum = let
	      val addNum' = addLiteral LV_NUM
	      in
		fn {ty={tag=true, ...}, ival} => IMMED ival
		 | {ty={sz, ...}, ival} => addNum' {ty=sz, ival=ival}
	      end
	val addString = addLiteral LV_STR
	val addReal = addLiteral LV_REAL
	end (* local *)

	fun findVar (LE{vMap, ...}) = IntTbl.find vMap

	fun insertVar (LE{vMap, ...}) = IntTbl.insert vMap

	fun isConst (LE{vMap, ...}) = let
	      val inDomain = inDomain tbl
	      in
		fn (C.VAR x) => inDomain x
		 | (C.LABEL _) => bug "unexpected LABEL"
		 | C.VOID => false
		 | _ => false
	      end

	fun useValue tbl = let
	      val findVar = findVar tbl
	      val addNum = addNum tbl
	      val addReal = addReal tbl
	      val addString = addString tbl
	      val insert = insertVar vMap
	      in
		fn (C.VAR x) => (case findVar x
		       of SOME(flg, lit) => (
			    useLit lit;
			    if flg then () else insert (x, (true, lit))
			| NONE => ()
		      (* end case *))
		 | (C.LABEL _) => bug "unexpected LABEL"
		 | (C.NUM n) => useLit(addNum n)
		 | (C.REAL r) => useLit(addReal r)
		 | (C.STRING s) => useLit(addString s)
		 | C.VOID => ()
	      end

	fun useValue' tbl = let
	      val findVar = findVar tbl
	      val addNum = addNum tbl
	      val addReal = addReal tbl
	      val addString = addString tbl
	      fun use lit = (useLit lit; lit)
	      in
		fn (C.VAR x) => (case findVar x
		      of SOME lit => use lit
		       | NONE => bug "expected literal"
		     (* end case *))
		 | (C.LABEL _) => bug "unexpected LABEL"
		 | (C.NUM n) => use(addNum ln)
		 | (C.REAL r) => use(addReal lr)
		 | (C.STRING s) => use(addString s)
		 | C.VOID => bug "unexpected VOID"
	      end

	fun addRecord (tbl as LE{lits, vMap}) = let
	      val add = add lits
	      val insert = IntTbl.insert vMap
	      in
		fn (rk, flds, v) => insert (v, (false, add (LV_RECORD(rk, flds))))
	      end

	fun isEmpty (LE{lits, ...}) = (LTbl.numItems lits = 0)

	fun allLits (LE{lits, ...}) = LTbl.listItems lits

      end (* LitEnv *)

  (* The first pass initializes the literal table by walking the CPS module.  After
   * this pass, we have identified any literal value that needs to be included in the
   * literal section.  Furthermore, we have identified which literal values are used
   * in non-literal contexts.
   *)
    fun identifyLiterals body = let
	  val tbl = LitEnv.new()
	  val isConst = LitEnv.isConst tbl
	  val useValue = LitEnv.useValue tbl
	  val useValues = List.app useValue
	  val useValue' = LitEnv.useValue' tbl
	  val addRecord = LitEnv.addRecord tbl
	(* process a CPS function *)
	  fun doFun (fk, f, vl, cl, e) = doExp e
	(* process a CPS expression *)
	  and doExp ce = (case ce
		 of C.RECORD(rk, fields, v, e) => let
		      fun fieldToValue (u, C,OFFp 0) = u
			| fieldToValue _ = bug "unexpected access in field"
		      val ul = List.map fieldToValue ul
		      in
			if List.all isConst ul
			  then addRecord (rk, List.map addValue' ul, v)
			  else useValues ul;
			doExp e
		      end
		  | C.SELECT(i, u, v, t, e) => (useValue u; doExp e)
		  | C.OFFSET _ => bug "unexpected OFFSET in doExp"
		  | C.APP(u, ul) => useValues ul
		  | C.FIX(fns, e) => (List.app doFun fns; doExp e)
		  | C.SWITCH(u, v, es) => (useValue u; List.app doExp es)
		  | C.BRANCH(p, ul, v, e1, e2) => (useValues ul; doExp e1; doExp e2)
		  | C.SETTER(p, ul, e) => (useValues ul; doExp e)
		  | C.LOOKER(p, ul, v, t, e) => (useValues ul; doExp e)
		  | C.ARITH(p, ul, v, t, e) => (useValues ul; doExp e)
		  | C.PURE(C.P.WRAP(P.INT sz), [u], v, t, e) => (case useValue' u
		       of SOME lit => (
			    addRecord (C.RK_RAWBLOCK, [LV_NUM{ty=sz, ival=lit}], v);
			    doExp e)
			| NONE => doExp e
		      (* end case *))
(* REAL32: FIXME *)
		  | C.PURE(C.P.WRAP(P.FLOAT 64), [u], v, t, e) => (case useValue' u
		       of SOME lit => (
			    addRecord (C.RK_RAW64BLOCK, [LV_REAL{ty=64, rval=lit}], v);
			    doExp e)
			| NONE => doExp e
		      (* end case *))
		  | C.PURE (p, ul, v, t, e) => (useValues ul; doExp e)
		  | C.RCC (k, l, p, ul, vtl, e) => (useValues ul; doExp e)
		(* end case *))
	  in
	    doExp body;
	    tbl
	  end

  (* build the representation of the literals; return the literal vector and a list of
   * variables that are
   *)
    fun buildLits ltbl = let
	(* get a list of the literals that are bound to variables in order of their
	 * definition.
	 *)
	  val lits = let
		fun gt (LIT{id=a, ...}, LIT{id=b, ...}) = (a > b)
		  | gt _ = bug "unexpected IMMED literal"
		in
		  ListMergeSort.sort gt
		    (List.filter litIsUsed (LTbl.listItems lits))
		end
	  in
???
	  end

  (* rewrite the program, removing unused variables *)
    fun liftLits (tbl, body) = let
	(* process a CPS function *)
	  fun doFun (fk, f, vl, cl, e) = (fk, f, vl, cl, doExp e)
	(* process a CPS expression *)
	  and doExp ce = (case ce
		 of C.RECORD(rk, ul, v, e) => (case findVar v
		       of NONE => let
			    fun rewriteField (u, acc) = (rewriteValue u, acc)
			    in
			      C.RECORD(rk, List.map rewriteField ul, v, doExp e)
			    end
			| SOME _ => doExp e
		      (* end case *))
		  | C.SELECT(i, u, v, t, e) => C.SELECT(i, rewriteValue u, v, t, doExp e)
		  | C.OFFSET _ => bug "unexpected OFFSET in doExp"
		  | C.APP(u, ul) => C.APP(u, rewriteValues ul)
		  | C.FIX(fns, e) => C.FIX(map doFun fns, doExp e)
		  | C.SWITCH(u, v, es) => C.SWITCH(rewriteValue u, v, List.map doExp es)
		  | C.BRANCH(p, ul, v, e1, e2) =>
		      C.BRANCH(p, rewriteValues ul, v, doExp e1, doExp e2)
		  | C.SETTER(p, ul, e) => C.SETTER(p, rewriteValues ul, doExp e)
		  | C.LOOKER(p, ul, v, t, e) => C.LOOKER(p, rewriteValues ul, v, t, doExp e)
		  | C.ARITH(p, ul, v, t, e) => C.ARITH(p, rewriteValues ul, v, t, doExp e)
		  | C.PURE(P.WRAP(P.INT sz), [u], v, t, e) => doExp e
		  | C.PURE(P.WRAP(P.FLOAT sz), [u], v, t, e) => doExp e
		  | C.PURE(p, ul, v, t, e) => C.PURE(p, rewriteValues ul, v, t, doExp e)
		  | C.RCC(k, l, p, ul, vtl, e) => C.RCC(k, l, p, rewriteValues ul, vtl, doExp e)
		(* end case *))
	  in
	  (* process the module *)
	    doExp body
	  end

 (* the main function *)
    fun split (func as (fk, f, vl as [_,x], [CNTt, t as PTRt(RPT n)], body)) = let
	(* new argument type has an additional argument for the literals *)
	  val nt = PTRt(RPT(n+1))
	  val ltbl = identifyLiterals body
	  val nbody = if if LTbl.isEmpty ltbl
		then body
		else let
		  val (litVec, litVars) = buildLiterals ltbl
		  val nbody = liftLiterals (ltbl, body)
(* wrap the body with bindings for the literals *)
		  in
		    nbody
		  end
	  in
	    if !debugFlg
	      then (
		say (concat["\n[After Literals.split ...]\n"]);
		PPCps.printcps0 nfn;
		say "==========\n";
		printLits lit;
		say "\n")
	      else ();
	    (nfunc, litVec)
	  end
      | split _ = bug "unexpected CPS header in split"

  end (* Literals *)
