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

structure Literals : LITERALS =
  struct

    structure W8V = Word8Vector
    structure LV = LambdaVar
    structure Intset = struct
	type intset = IntRedBlackSet.set ref
	fun new() = ref IntRedBlackSet.empty
	fun add set i = set := IntRedBlackSet.add(!set, i)
	fun mem set i =  IntRedBlackSet.member(!set, i)
	fun rmv set i = set := IntRedBlackSet.delete(!set, i)
      end

    open CPS

    fun bug msg = ErrorMsg.impossible ("Literals: "^msg)
    fun mkv _ = LV.mkLvar()

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

  (* an environment for tracking literals *)
    datatype env = LE of {
	usedVars : IntSet.set,
	freeVars : lvar list ref,	(* free lvars of module *)
      }

    fun newEnv () = ??

    fun addReal (LE{...}) r = ??
    fun addString (LE{...}) s = ??
    fun addVar (LE{freeVars, ...}) x = freeVars := x :: !freeVars

  (* lifting all literals from a CPS program *)
    fun liftlits (body, root, offset) = let
	(* create the literal environment *)
	  val env = newEnv()
	  val entReal = addReal env
	  val entStr = addString env
	  val addv = addVar env
	(* translation on the CPS values *)
	  fun doValue u = (case u
		 of REAL{rval, ...} => SOME(entReal rval)	(* REAL32: FIXME *)
		  | STRING s => SOME(entStr s)
		  | VAR v => (used v; NONE)
		  | _ => NONE
		(* end case *))
	  fun doValues vs = let
		fun addVal (u, (xs, hh)) = (case doValue u
		       of NONE => (u::xs, hh)
			| SOME(nu, nh) => (nu::xs, nh o hh)
		      (* end case *))
	        in
		  foldr addVal ([], Fn.id) vs
	        end
	(* process a CPS function *)
	  fun doFun (fk, f, vl, cl, e) = (fk, f, vl, cl, doExp e)
	(* process a CPS expression *)
	  and doExp ce = (case ce
		 of RECORD (rk, ul, v, e) => record (rk, ul, v) (doExp e)
		  | SELECT (i, u, v, t, e) => let
		      val (nu, hh) = doValue u
		      in
			hh (SELECT(i, nu, v, t, doExp e))
		      end
		  | OFFSET _ => bug "unexpected OFFSET in doExp"
		  | APP (u, ul) => let
		      val (nu, h1) = doValue u
		      val (nl, h2) = doValues ul
		      in
			h1 (h2 (APP(nu, nl)))
		      end
		  | FIX (fns, e) => FIX(map doFun fns, doExp e)
		  | SWITCH (u, v, es) => let
		      val (nu, hh) = doValue u
		      in
			hh(SWITCH(nu, v, map doExp es))
		      end
		  | BRANCH (p, ul, v, e1, e2) => let
		      val (nl, hh) = doValues ul
		      in
			hh(BRANCH(p, nl, v, doExp e1, doExp e2))
		      end
		  | SETTER (p, ul, e) => let
		      val (nl, hh) = doValues ul
		      in
			hh(SETTER(p, nl, doExp e))
		      end
		  | LOOKER (p, ul, v, t, e) => let
		      val (nl, hh) = doValues ul
		      in
			hh(LOOKER(p, nl, v, t, doExp e))
		      end
		  | ARITH (p, ul, v, t, e) => let
		      val (nl, hh) = doValues ul
		      in
			hh(ARITH(p, nl, v, t, doExp e))
		      end
		  | PURE (P.WRAP(P.INT sz), [u], v, t, e) =>
		      ??
		  | PURE (P.WRAP(P.FLOAT sz), [u], v, t, e) =>
		      wrapfloat (sz, u, v, t) (doExp e)
		  | PURE (p, ul, v, t, e) => let
		      val (nl, hh) = doValues ul
		      in
			hh(PURE(p, nl, v, t, doExp e))
		      end
		  | RCC (k, l, p, ul, vtl, e) => let
		      val (nl, hh) = doValues ul
		      in
			hh(RCC(k, l, p, nl, vtl, doExp e))
		      end
		(* end case *))
	(* process the module *)
	  val body' = doExp body
	(* get the literals and the prelude code *)
	  val (addPrelude) = getLiterals ()
	  in
	    (addPrelude body', lits)
	  end (* liftlits *)

  end (* Literals *)
