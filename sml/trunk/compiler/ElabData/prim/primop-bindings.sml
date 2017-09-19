(* primop-bindings.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * List of primop bindings that comprise the InlineT structure.  These are
 * represented as triples with names, types, and the actual primop.
 *)

structure PrimopBindings : sig

    type primop_bind

    val mk : string * Types.ty * Primop.primop -> primop_bind

    val nameOf : primop_bind -> string
    val typeOf : primop_bind -> Types.ty
    val defnOf : primop_bind -> Primop.primop

    val prims : primop_bind list

  end = struct

    structure T = Types
    structure BT = BasicTypes
    structure P = Primop

    type primop_bind = string * Types.ty * Primop.primop

    fun mk arg = arg

    fun nameOf (n, _, _) = n
    fun typeOf (_, ty, _) = ty
    fun defnOf (_, _, p) = p

    local
      val v1 = T.IBOUND 0
      val v2 = T.IBOUND 1
      val v3 = T.IBOUND 2

      val tu = BT.tupleTy
      fun ar(t1,t2) = BT.--> (t1, t2)

      fun ap(tc,l) = T.CONty(tc, l)
      fun cnt t = T.CONty(BT.contTycon,[t])
      fun ccnt t = T.CONty(BT.ccontTycon,[t])
      fun rf t = T.CONty(BT.refTycon,[t])
      fun ay t = T.CONty(BT.arrayTycon,[t])
      fun vct t = T.CONty(BT.vectorTycon,[t])

      val u = BT.unitTy
      val bo = BT.boolTy
      val i = BT.intTy
      val i32 = BT.int32Ty
      val i64 = BT.int64Ty
      val inf = BT.intinfTy
      val w8 = BT.word8Ty
      val w = BT.wordTy
      val w32 = BT.word32Ty
      val w64 = BT.word64Ty
      val f64 = BT.realTy
      val s  = BT.stringTy

      fun p0 t = t
      fun p1 t = T.POLYty {sign=[false], tyfun=T.TYFUN {arity=1, body=t}}
      fun ep1 t = T.POLYty {sign=[true], tyfun=T.TYFUN {arity=1, body=t}}
      fun p2 t = T.POLYty {sign=[false,false], tyfun=T.TYFUN {arity=2, body=t}}
      fun p3 t = T.POLYty {sign=[false,false,false], tyfun=T.TYFUN {arity=3, body=t}}
      val numSubTy = p2(ar(tu[v1,i],v2))
      val numUpdTy = p2(ar(tu[v1,i,v2],u))

      fun unf t = p0(ar(t,t))
      fun binf t = p0(ar(tu[t,t],t))
      fun binp t = p0(ar(tu[t,t],bo))
      fun shifter t = p0(ar(tu[t,w],t))

      val w32_i32 = p0(ar(w32,i32))
      val w32_f64 = p0(ar(w32,f64))
      val w32w32_u = p0(ar(tu[w32,w32],u))
      val w32i32_u = p0(ar(tu[w32,i32],u))
      val w32f64_u = p0(ar(tu[w32,f64],u))

      val i_x      = p1(ar(i,v1))
      val xw32_w32 = p1(ar(tu[v1,w32],w32))
      val xw32_i32 = p1(ar(tu[v1,w32],i32))
      val xw32_f64 = p1(ar(tu[v1,w32],f64))
      val xw32w32_u = p1(ar(tu[v1,w32,w32],u))
      val xw32i32_u = p1(ar(tu[v1,w32,i32],u))
      val xw32f64_u = p1(ar(tu[v1,w32,f64],u))

      val b_b = unf bo

      val f64_i = p0(ar(f64,i))
      val i_f64 = p0(ar(i,f64))
      val i32_f64 = p0(ar(i32,f64))

      val w32_i = p0(ar(w32,i))
      val i32_i = p0(ar(i32,i))

      val i_i32 = p0(ar(i,i32))
      val i_w32 = p0(ar(i,w32))

      val w32_w = p0(ar(w32,w))
      val i32_w = p0(ar(i32,w))

      val w_w32 = p0(ar(w,w32))
      val w_i32 = p0(ar(w,i32))

      val w_i = p0(ar(w,i))
      val i_w = p0(ar(i,w))

      val w32_i32 = p0(ar(w32,i32))
      val i32_w32 = p0(ar(i32,w32))

      val i_i = unf i
      val ii_i = binf i
      val ii_b = binp i
      val iw_i = shifter i

      val w_w = unf w
      val ww_w = binf w
      val ww_b = binp w

      val i32_i32 = unf i32
      val i32i32_i32 = binf i32
      val i32i32_b = binp i32

      val w32_w32 = unf w32
      val w32w32_w32 = binf w32
      val w32w32_b = binp w32
      val w32w_w32 = shifter w32

      val w8_w8 = unf w8
      val w8w8_w8 = binf w8
      val w8w8_b = binp w8
      val w8w_w8 = shifter w8

      val f64_b = p0(ar(f64,bo))
      val f64_f64 = unf f64
      val f64f64_f64 = binf f64
      val f64f64_b = binp f64

      val w8_i = p0(ar(w8,i))
      val w8_i32 = p0(ar(w8,i32))
      val w8_w32 = p0(ar(w8,w32))
      val i_w8 = p0(ar(i,w8))
      val i32_w8 = p0(ar(i32,w8))
      val w32_w8 = p0(ar(w32,w8))

      val inf_i   = p0(ar(inf,i))
      val inf_i32 = p0(ar(inf,i32))
      val inf_i64 = p0(ar(inf,i64))
      val inf_w8  = p0(ar(inf,w8))
      val inf_w   = p0(ar(inf,w))
      val inf_w32 = p0(ar(inf,w32))
      val inf_w64 = p0(ar(inf,w64))
      val i_inf   = p0(ar(i,inf))
      val i32_inf = p0(ar(i32,inf))
      val i64_inf = p0(ar(i64,inf))
      val w8_inf  = p0(ar(w8,inf))
      val w_inf   = p0(ar(w,inf))
      val w32_inf = p0(ar(w32,inf))
      val w64_inf = p0(ar(w64,inf))

      val w64_pw32 = p0(ar(w64,tu[w32,w32]))
      val pw32_w64 = p0(ar(tu[w32,w32],w64))
      val i64_pw32 = p0(ar(i64,tu[w32,w32]))
      val pw32_i64 = p0(ar(tu[w32,w32],i64))

      val cc_b = binp BT.charTy

    (* The type of the RAW_CCALL primop (as far as the type checker is concerned)
     * is:
     *    word32 * 'a * 'b -> 'd
     * However, the primop cannot be used without having 'a, 'b, and 'c
     * monomorphically instantiated.  In particular, 'a will be the type of the
     * ML argument list, 'c will be the type of the result, and 'b
     * will be a type of a fake arguments.  The idea is that 'b will be
     * instantiated with some ML type that encodes the type of the actual
     * C function in order to be able to generate code according to the C
     * calling convention.
     * (In other words, 'b will be a completely ad-hoc encoding of a CTypes.c_proto
     * value in ML types.  The encoding also contains information about
     * calling conventions and reentrancy.)
     *)
      val rccType = p3(ar(tu[w32,v1,v2],v3))

    (* helper functions for primop definitions *)
      fun bits size oper = P.ARITH{oper=oper, overflow=false, kind=P.INT size}
      val bits31 = bits 31		
      val bits32 = bits 32		

      fun int size oper = P.ARITH{oper=oper, overflow=true, kind=P.INT size}
      val int31 = int 31
      val int32 = int 32

      fun word size oper = P.ARITH{oper=oper, overflow=false, kind=P.UINT size}
      val word32 = word 32
      val word31 = word 31
      val word8  = word 8

      fun purefloat size oper = P.ARITH{oper=oper,overflow=false,kind=P.FLOAT size}
      val purefloat64 = purefloat 64	

      fun cmp kind oper = P.CMP{oper=oper, kind=kind}
      val int31cmp = cmp (P.INT 31)
      val int32cmp = cmp (P.INT 32)

      val word32cmp = cmp (P.UINT 32)
      val word31cmp = cmp (P.UINT 31)
      val word8cmp  = cmp (P.UINT 8)

      val float64cmp = cmp (P.FLOAT 64)

      fun sub kind = P.NUMSUBSCRIPT{kind=kind, checked=false, immutable=false}
      fun chkSub kind = P.NUMSUBSCRIPT{kind=kind, checked=true, immutable=false}

      fun subv kind = P.NUMSUBSCRIPT{kind=kind, checked=false, immutable=true}
      fun chkSubv kind = P.NUMSUBSCRIPT{kind=kind, checked=true, immutable=true}

      fun update kind = P.NUMUPDATE {kind=kind, checked=false}
      fun chkUpdate kind = P.NUMUPDATE {kind=kind, checked=true}

    in
    val prims = [
	  (* continuation operators *)
	    ("callcc", p1(ar(ar(cnt(v1),v1),v1)), P.CALLCC),
	    ("throw", p2(ar(cnt(v1),ar(v1,v2))), P.THROW),
	    ("capture", p1(ar(ar(ccnt(v1),v1),v1)), P.CAPTURE),
	    ("isolate", p1(ar(ar(v1,u),cnt(v1))), P.ISOLATE),
	    ("cthrow", p2(ar(ccnt(v1),ar(v1,v2))), P.THROW),
	  (* reference operations *)
	    ("!", p1(ar(rf(v1),v1)), P.DEREF),
	    (":=", p1(ar(tu[rf(v1),v1],u)), P.ASSIGN),
	    ("makeref", p1(ar(v1,rf(v1))), P.MAKEREF),
	  (* boxity tests *)
	    ("boxed", p1(ar(v1,bo)), P.BOXED),
	    ("unboxed", p1(ar(v1,bo)), P.UNBOXED),
	  (* type casts *)
	    ("cast", p2(ar(v1,v2)), P.CAST),
	  (* polymorphic equality tests *)
	    ("=", ep1(ar(tu[v1,v1],bo)), P.POLYEQL),
	    ("<>", ep1(ar(tu[v1,v1],bo)), P.POLYNEQ),
	    ("ptreql", p1(ar(tu[v1,v1],bo)), P.PTREQL),
	    ("ptrneq", p1(ar(tu[v1,v1],bo)), P.PTRNEQ),
          (* runtime hooks *)
	    ("getvar", p1(ar(u,v1)), P.GETVAR),
	    ("setvar", p1(ar(v1,u)), P.SETVAR),
	    ("setpseudo", p1(ar(tu[v1,i],u)), P.SETPSEUDO),
	    ("getpseudo", p1(ar(i,v1)), P.GETPSEUDO),
	    ("mkspecial", p2(ar(tu[i,v1],v2)), P.MKSPECIAL),
	    ("getspecial", p1(ar(v1,i)), P.GETSPECIAL),
	    ("setspecial", p1(ar(tu[v1,i],u)), P.SETSPECIAL),
	    ("gethdlr", p1(ar(u,cnt(v1))), P.GETHDLR),
	    ("sethdlr", p1(ar(cnt(v1),u)), P.SETHDLR),
	    ("gettag", p1(ar(v1,i)), P.GETTAG),
	    ("setmark", p1(ar(v1,u)), P.SETMARK),
	    ("dispose", p1(ar(v1,u)), P.DISPOSE),
	  (* *)
	    ("compose", p3(ar(tu[ar(v2,v3),ar(v1,v2)],ar(v1,v3))), P.INLCOMPOSE),
	    ("before", p2(ar(tu[v1,v2],v1)), P.INLBEFORE),
	    ("ignore", p1(ar(v1,u)), P.INLIGNORE),
	    ("identity", p1(ar(v1,v1)), P.INLIDENTITY),
	    ("length", p1(ar(v1,i)), P.LENGTH),
	    ("objlength", p1(ar(v1,i)), P.OBJLENGTH),
	    ("unboxedupdate", p1(ar(tu[ay(v1),i,v1],u)), P.UNBOXEDUPDATE),
          (* boolean operations *)
	    ("inlnot", b_b, P.INLNOT),
	  (* int to/from real conversions *)
	    ("floor", f64_i, P.ROUND{floor=true, fromkind=P.FLOAT 64, tokind=P.INT 31}),
	    ("round", f64_i, P.ROUND{floor=false, fromkind=P.FLOAT 64, tokind=P.INT 31}),
	    ("real", i_f64, P.REAL{fromkind=P.INT 31, tokind=P.FLOAT 64}),
	    ("real32", i32_f64, P.REAL{fromkind=P.INT 32, tokind=P.FLOAT 64}),
	  (* bytearray and bytevector operations *)
	    ("ordof", numSubTy, P.NUMSUBSCRIPT{kind=P.INT 8, checked=false, immutable=true}),
	    ("store", numUpdTy, P.NUMUPDATE{kind=P.INT 8, checked=false}),
	    ("inlbyteof", numSubTy, P.NUMSUBSCRIPT{kind=P.INT 8, checked=true, immutable=false}),
	    ("inlstore", numUpdTy, P.NUMUPDATE{kind=P.INT 8, checked=true}),
	    ("inlordof", numSubTy, P.NUMSUBSCRIPT{kind=P.INT 8, checked=true, immutable=true}),
	  (* polymorphic array and vector *)
	    ("mkarray", p1(ar(tu[i,v1],ay(v1))), P.INLMKARRAY),
	    ("arrSub", p1(ar(tu[ay(v1),i],v1)), P.SUBSCRIPT),
	    ("arrChkSub", p1(ar(tu[ay(v1),i],v1)), P.INLSUBSCRIPT),
	    ("vecSub", p1(ar(tu[vct(v1),i],v1)), P.SUBSCRIPTV),
	    ("vecChkSub", p1(ar(tu[vct(v1),i],v1)), P.INLSUBSCRIPTV),
	    ("arrUpdate", p1(ar(tu[ay(v1),i,v1],u)), P.UPDATE),
	    ("arrChkUpdate", p1(ar(tu[ay(v1),i,v1],u)), P.INLUPDATE),
	  (* new array representations *)
	    ("newArray0", p1(ar(u,v1)), P.NEW_ARRAY0),
	    ("getSeqData", p2(ar(v1, v2)), P.GET_SEQ_DATA),
	    ("recordSub", p2(ar(tu[v1,i],v2)), P.SUBSCRIPT_REC),
	    ("raw64Sub", p1(ar(tu[v1,i],f64)), P.SUBSCRIPT_RAW64),
	  (*** conversion primops ***
	   *   There are certain duplicates for the same primop (but with
	   *   different types).  In such a case, the "canonical" name
	   *   of the primop has been extended using a simple suffix
	   *   scheme.
           *)
	    ("test_32_31_w", w32_i, P.TEST(32,31)),
	    ("test_32_31_i", i32_i, P.TEST(32,31)),
	    ("testu_31_31", w_i, P.TESTU(31,31)),
	    ("testu_32_31", w32_i, P.TESTU(32,31)),
	    ("testu_32_32", w32_i32, P.TESTU(32,32)),
	    ("copy_32_32_ii", i32_i32, P.COPY(32,32)),
	    ("copy_32_32_wi", w32_i32, P.COPY(32,32)),
	    ("copy_32_32_iw", i32_w32, P.COPY(32,32)),
	    ("copy_32_32_ww", w32_w32, P.COPY(32,32)),
	    ("copy_31_31_ii", i_i, P.COPY(31,31)),
	    ("copy_31_31_wi", w_i, P.COPY(31,31)),
	    ("copy_31_31_iw", i_w, P.COPY(31,31)),
	    ("copy_31_32_i", w_i32, P.COPY(31,32)),
	    ("copy_31_32_w", w_w32, P.COPY(31,32)),
	    ("copy_8_32_i", w8_i32, P.COPY(8,32)),
	    ("copy_8_32_w", w8_w32, P.COPY(8,32)),
	    ("copy_8_31", w8_i, P.COPY(8,31)),
	    ("extend_31_32_ii", i_i32, P.EXTEND(31,32)),
	    ("extend_31_32_iw", i_w32, P.EXTEND(31,32)),
	    ("extend_31_32_wi", w_i32, P.EXTEND(31,32)),
	    ("extend_31_32_ww", w_w32, P.EXTEND(31,32)),
	    ("extend_8_31", w8_i, P.EXTEND(8,31)),
	    ("extend_8_32_i", w8_i32, P.EXTEND(8,32)),
	    ("extend_8_32_w", w8_w32, P.EXTEND(8,32)),
	    ("trunc_32_31_i", i32_w, P.TRUNC(32,31)),
	    ("trunc_32_31_w", w32_w, P.TRUNC(32,31)),
	    ("trunc_31_8", i_w8, P.TRUNC(31,8)),
	    ("trunc_32_8_i", i32_w8, P.TRUNC(32,8)),
	    ("trunc_32_8_w", w32_w8, P.TRUNC(32,8)),
	  (* conversion primops involving intinf *)
	    ("test_inf_31", inf_i, P.TEST_INF 31),
	    ("test_inf_32", inf_i32, P.TEST_INF 32),
	    ("test_inf_64", inf_i64, P.TEST_INF 64),
	    ("copy_8_inf", w8_inf, P.COPY_INF 8),
	    ("copy_8_inf_w", w8_inf, P.COPY_INF 8),
	    ("copy_31_inf_w", w_inf, P.COPY_INF 31),
	    ("copy_32_inf_w", w32_inf, P.COPY_INF 32),
	    ("copy_64_inf_w", w64_inf, P.COPY_INF 64),
	    ("copy_31_inf_i", i_inf, P.COPY_INF 31),
	    ("copy_32_inf_i", i32_inf, P.COPY_INF 32),
	    ("copy_64_inf_i", i64_inf, P.COPY_INF 64),
	    ("extend_8_inf", w8_inf, P.EXTEND_INF 8),
	    ("extend_8_inf_w", w8_inf, P.EXTEND_INF 8),
	    ("extend_31_inf_w", w_inf, P.EXTEND_INF 31),
	    ("extend_32_inf_w", w32_inf, P.EXTEND_INF 32),
	    ("extend_64_inf_w", w64_inf, P.EXTEND_INF 64),
	    ("extend_31_inf_i", i_inf, P.EXTEND_INF 31),
	    ("extend_32_inf_i", i32_inf, P.EXTEND_INF 32),
	    ("extend_64_inf_i", i64_inf, P.EXTEND_INF 64),
	    ("trunc_inf_8", inf_w8, P.TRUNC_INF 8),
	    ("trunc_inf_31", inf_w, P.TRUNC_INF 31),
	    ("trunc_inf_32", inf_w32, P.TRUNC_INF 32),
	    ("trunc_inf_64", inf_w64, P.TRUNC_INF 64),
	  (* primops to go between abstract and concrete representation of
	   * 64-bit ints and words
	   *)
	    ("w64p", w64_pw32, P.CVT64),
	    ("p64w", pw32_w64, P.CVT64),
	    ("i64p", i64_pw32, P.CVT64),
	    ("p64i", pw32_i64, P.CVT64),
	  (*** integer 31 primops ***
	   *   Many of the i31 primops are being abused for different types
	   *   (mostly Word8.word and also for char).  In these cases
	   *   there are suffixed alternative versions of the primop
	   *   (i.e., same primop, different type).
	   *)
	    ("i31add", ii_i, int31 P.ADD),
	    ("i31add_8", w8w8_w8, int31 P.ADD),
	    ("i31sub", ii_i, int31 P.SUB),
	    ("i31sub_8", w8w8_w8, int31 P.SUB),
	    ("i31mul", ii_i, int31 P.MUL ),
	    ("i31mul_8", w8w8_w8, int31 P.MUL ),
	    ("i31div", ii_i, int31 P.DIV),
	    ("i31div_8", w8w8_w8, int31 P.DIV),
	    ("i31mod", ii_i, int31 P.MOD),
	    ("i31mod_8", w8w8_w8, int31 P.MOD),
	    ("i31quot", ii_i, int31 P.DIV),
	    ("i31rem", ii_i, int31 P.REM),
	    ("i31orb", ii_i, bits31 P.ORB),
	    ("i31orb_8", w8w8_w8, bits31 P.ORB),
	    ("i31andb", ii_i, bits31 P.ANDB),
	    ("i31andb_8", w8w8_w8, bits31 P.ANDB),
	    ("i31xorb", ii_i, bits31 P.XORB),
	    ("i31xorb_8", w8w8_w8, bits31 P.XORB),
	    ("i31notb", i_i, bits31 P.NOTB),
	    ("i31notb_8", w8_w8, bits31 P.NOTB),
	    ("i31neg", i_i, int31 P.NEG),
	    ("i31neg_8", w8_w8, int31 P.NEG),
	    ("i31lshift", ii_i, bits31 P.LSHIFT),
	    ("i31lshift_8", w8w_w8, bits31 P.LSHIFT),
	    ("i31rshift", ii_i, bits31 P.RSHIFT),
	    ("i31rshift_8", w8w_w8, bits31 P.RSHIFT),
	    ("i31lt", ii_b, int31cmp P.LT),
	    ("i31lt_8", w8w8_b, int31cmp P.LT),
	    ("i31lt_c", cc_b, int31cmp P.LT),
	    ("i31le", ii_b, int31cmp P.LTE),
	    ("i31le_8", w8w8_b, int31cmp P.LTE),
	    ("i31le_c", cc_b, int31cmp P.LTE),
	    ("i31gt", ii_b, int31cmp P.GT),
	    ("i31gt_8", w8w8_b, int31cmp P.GT),
	    ("i31gt_c", cc_b, int31cmp P.GT),
	    ("i31ge", ii_b, int31cmp P.GTE),
	    ("i31ge_8", w8w8_b, int31cmp P.GTE),
	    ("i31ge_c", cc_b, int31cmp P.GTE),
	    ("i31ltu", ii_b, word31cmp P.LTU),
	    ("i31geu", ii_b, word31cmp P.GEU),
	    ("i31eq", ii_b, int31cmp P.EQL),
	    ("i31ne", ii_b, int31cmp P.NEQ),
	    ("i31min", ii_i, P.INLMIN (P.INT 31)),
	    ("i31min_8", w8w8_w8, P.INLMIN (P.INT 31)),
	    ("i31max", ii_i, P.INLMAX (P.INT 31)),
	    ("i31max_8", w8w8_w8, P.INLMAX (P.INT 31)),
	    ("i31abs", i_i, P.INLABS (P.INT 31)),
	  (* integer 32 primops *)
	    ("i32mul", i32i32_i32, int32 P.MUL ),
	    ("i32div", i32i32_i32, int32 P.DIV),
	    ("i32mod", i32i32_i32, int32 P.MOD),
	    ("i32quot", i32i32_i32, int32 P.DIV),
	    ("i32rem", i32i32_i32, int32 P.REM),
	    ("i32add", i32i32_i32, int32 P.ADD),
	    ("i32sub", i32i32_i32, int32 P.SUB),
	    ("i32orb", i32i32_i32, bits32 P.ORB),
	    ("i32andb", i32i32_i32, bits32 P.ANDB),
	    ("i32xorb", i32i32_i32, bits32 P.XORB),
	    ("i32lshift", i32i32_i32, bits32 P.LSHIFT),
	    ("i32rshift", i32i32_i32, bits32 P.RSHIFT),
	    ("i32neg", i32_i32, int32 P.NEG),
	    ("i32lt", i32i32_b, int32cmp P.LT),
	    ("i32le", i32i32_b, int32cmp P.LTE),
	    ("i32gt", i32i32_b, int32cmp P.GT),
	    ("i32ge", i32i32_b, int32cmp P.GTE),
	    ("i32eq", i32i32_b, int32cmp P.EQL),
	    ("i32ne", i32i32_b, int32cmp P.NEQ),
	    ("i32min", i32i32_i32, P.INLMIN (P.INT 32)),
	    ("i32max", i32i32_i32, P.INLMAX (P.INT 32)),
	    ("i32abs", i32_i32, P.INLABS (P.INT 32)),
	  (* float 64 primops *)
	    ("f64add", f64f64_f64, purefloat64 (P.ADD)),
	    ("f64sub", f64f64_f64, purefloat64 (P.SUB)),
	    ("f64div", f64f64_f64, purefloat64 (P.DIV)),
	    ("f64mul", f64f64_f64, purefloat64 (P.MUL )),
	    ("f64neg", f64_f64, purefloat64 P.NEG),
	    ("f64ge", f64f64_b, float64cmp (P.GTE)),
	    ("f64gt", f64f64_b, float64cmp (P.GT)),
	    ("f64le", f64f64_b, float64cmp (P.LTE)),
	    ("f64lt", f64f64_b, float64cmp (P.LT)),
	    ("f64eq", f64f64_b, float64cmp P.EQL),
	    ("f64ne", f64f64_b, float64cmp P.NEQ),
	    ("f64sgn", f64_b, float64cmp P.FSGN),
	    ("f64abs", f64_f64, purefloat64 P.ABS),
	    ("f64sin", f64_f64, purefloat64 P.FSIN),
	    ("f64cos", f64_f64, purefloat64 P.FCOS),
	    ("f64tan", f64_f64, purefloat64 P.FTAN),
	    ("f64sqrt", f64_f64, purefloat64 P.FSQRT),
	    ("f64min", f64f64_f64, P.INLMIN (P.FLOAT 64)),
	    ("f64max", f64f64_f64, P.INLMAX (P.FLOAT 64)),
	  (* float64 array *)	
	    ("f64Sub", numSubTy, sub (P.FLOAT 64)),
	    ("f64chkSub", numSubTy, chkSub (P.FLOAT 64)),
	    ("f64Update", numUpdTy, update (P.FLOAT 64)),
	    ("f64chkUpdate", numUpdTy, chkUpdate (P.FLOAT 64)),
	  (* word8 primops *)
	    ("w8orb", w8w8_w8, word31 P.ORB),
	    ("w8xorb", w8w8_w8, word31 P.XORB),
	    ("w8andb", w8w8_w8, word31 P.ANDB),
	    ("w8gt", w8w8_b, word8cmp P.GT),
	    ("w8ge", w8w8_b, word8cmp P.GTE),
	    ("w8lt", w8w8_b, word8cmp P.LT),
	    ("w8le", w8w8_b, word8cmp P.LTE),
	    ("w8eq", w8w8_b, word8cmp P.EQL),
	    ("w8ne", w8w8_b, word8cmp P.NEQ),
	  (* word8 array and vector *)
	    ("w8Sub", numSubTy, sub (P.UINT 8)),
	    ("w8chkSub", numSubTy, chkSub (P.UINT 8)),
	    ("w8subv", numSubTy, subv (P.UINT 8)),
	    ("w8chkSubv", numSubTy, chkSubv (P.UINT 8)),
	    ("w8update", numUpdTy, update (P.UINT 8)),
	    ("w8chkUpdate", numUpdTy, chkUpdate (P.UINT 8)),
	  (* word31 primops *)
	    ("w31mul", ww_w, word31 (P.MUL )),
	    ("w31div", ww_w, word31 (P.DIV)),
	    ("w31mod", ww_w, word31 (P.REM)),
	    ("w31add", ww_w, word31 (P.ADD)),
	    ("w31sub", ww_w, word31 (P.SUB)),
	    ("w31orb", ww_w, word31 P.ORB),
	    ("w31xorb", ww_w, word31 P.XORB),
	    ("w31andb", ww_w, word31 P.ANDB),
	    ("w31notb", w_w, word31 P.NOTB),
	    ("w31neg", w_w, word31 P.NEG),
	    ("w31rshift", ww_w, word31 P.RSHIFT),
	    ("w31rshiftl", ww_w, word31 P.RSHIFTL),
	    ("w31lshift", ww_w, word31 P.LSHIFT),
	    ("w31gt", ww_b, word31cmp (P.GT)),
	    ("w31ge", ww_b, word31cmp (P.GTE)),
	    ("w31lt", ww_b, word31cmp (P.LT)),
	    ("w31le", ww_b, word31cmp (P.LTE)),
	    ("w31eq", ww_b, word31cmp P.EQL),
	    ("w31ne", ww_b, word31cmp P.NEQ),
	    ("w31ChkRshift", ww_w, P.INLRSHIFT(P.UINT 31)),
	    ("w31ChkRshiftl", ww_w, P.INLRSHIFTL(P.UINT 31)),
	    ("w31ChkLshift", ww_w, P.INLLSHIFT(P.UINT 31)),
	    ("w31min", ww_w, P.INLMIN (P.UINT 31)),
	    ("w31max", ww_w, P.INLMAX (P.UINT 31)),
	  (* (pseudo-)word8 primops *)
	    ("w31mul_8", w8w8_w8, word31 (P.MUL )),
	    ("w31div_8", w8w8_w8, word31 (P.DIV)),
	    ("w31mod_8", w8w8_w8, word31 (P.REM)),
	    ("w31add_8", w8w8_w8, word31 (P.ADD)),
	    ("w31sub_8", w8w8_w8, word31 (P.SUB)),
	    ("w31orb_8", w8w8_w8, word31 P.ORB),
	    ("w31xorb_8", w8w8_w8, word31 P.XORB),
	    ("w31andb_8", w8w8_w8, word31 P.ANDB),
	    ("w31notb_8", w8_w8, word31 P.NOTB),
	    ("w31neg_8", w8_w8, word31 P.NEG),
	    ("w31rshift_8", w8w_w8, word31 P.RSHIFT),
	    ("w31rshiftl_8", w8w_w8, word31 P.RSHIFTL),
	    ("w31lshift_8", w8w_w8, word31 P.LSHIFT),
	    ("w31gt_8", w8w8_b, word31cmp (P.GT)),
	    ("w31ge_8", w8w8_b, word31cmp (P.GTE)),
	    ("w31lt_8", w8w8_b, word31cmp (P.LT)),
	    ("w31le_8", w8w8_b, word31cmp (P.LTE)),
	    ("w31eq_8", w8w8_b, word31cmp P.EQL),
	    ("w31ne_8", w8w8_b, word31cmp P.NEQ),
	    ("w31ChkRshift_8", w8w_w8, P.INLRSHIFT(P.UINT 31)),
	    ("w31ChkRshiftl_8", w8w_w8, P.INLRSHIFTL(P.UINT 31)),
	    ("w31ChkLshift_8", w8w_w8, P.INLLSHIFT(P.UINT 31)),
	    ("w31min_8", w8w8_w8, P.INLMIN (P.UINT 31)),
	    ("w31max_8", w8w8_w8, P.INLMAX (P.UINT 31)),
	  (* word32 primops *)
	    ("w32mul", w32w32_w32, word32 (P.MUL )),
	    ("w32div", w32w32_w32, word32 (P.DIV)),
	    ("w32mod", w32w32_w32, word32 (P.REM)),
	    ("w32add", w32w32_w32, word32 (P.ADD)),
	    ("w32sub", w32w32_w32, word32 (P.SUB)),
	    ("w32orb", w32w32_w32, word32 P.ORB),
	    ("w32xorb", w32w32_w32, word32 P.XORB),
	    ("w32andb", w32w32_w32, word32 P.ANDB),
	    ("w32notb", w32_w32, word32 P.NOTB),
	    ("w32neg", w32_w32, word32 P.NEG),
	    ("w32rshift", w32w_w32, word32 P.RSHIFT),
	    ("w32rshiftl", w32w_w32, word32 P.RSHIFTL),
	    ("w32lshift", w32w_w32, word32 P.LSHIFT),
	    ("w32gt", w32w32_b, word32cmp (P.GT)),
	    ("w32ge", w32w32_b, word32cmp (P.GTE)),
	    ("w32lt", w32w32_b, word32cmp (P.LT)),
	    ("w32le", w32w32_b, word32cmp (P.LTE)),
	    ("w32eq", w32w32_b, word32cmp P.EQL),
	    ("w32ne", w32w32_b, word32cmp P.NEQ),
	    ("w32ChkRshift", w32w_w32, P.INLRSHIFT(P.UINT 32)),
	    ("w32ChkRshiftl", w32w_w32, P.INLRSHIFTL(P.UINT 32)),
	    ("w32ChkLshift", w32w_w32, P.INLLSHIFT(P.UINT 32)),
	    ("w32min", w32w32_w32, P.INLMIN (P.UINT 32)),
	    ("w32max", w32w32_w32, P.INLMAX (P.UINT 32)),
	  (* C FFI primops *)
	    ("raww8l", w32_w32, P.RAW_LOAD (P.UINT 8)),
	    ("rawi8l", w32_i32, P.RAW_LOAD (P.INT 8)),
	    ("raww16l", w32_w32, P.RAW_LOAD (P.UINT 16)),
	    ("rawi16l", w32_i32, P.RAW_LOAD (P.INT 16)),
	    ("raww32l", w32_w32, P.RAW_LOAD (P.UINT 32)),
	    ("rawi32l", w32_i32, P.RAW_LOAD (P.INT 32)),
	    ("rawf32l", w32_f64, P.RAW_LOAD (P.FLOAT 32)),
	    ("rawf64l", w32_f64, P.RAW_LOAD (P.FLOAT 64)),
	    ("raww8s", w32w32_u, P.RAW_STORE (P.UINT 8)),
	    ("rawi8s", w32i32_u, P.RAW_STORE (P.INT 8)),
	    ("raww16s", w32w32_u, P.RAW_STORE (P.UINT 16)),
	    ("rawi16s", w32i32_u, P.RAW_STORE (P.INT 16)),
	    ("raww32s", w32w32_u, P.RAW_STORE (P.UINT 32)),
	    ("rawi32s", w32i32_u, P.RAW_STORE (P.INT 32)),
	    ("rawf32s", w32f64_u, P.RAW_STORE (P.FLOAT 32)),
	    ("rawf64s", w32f64_u, P.RAW_STORE (P.FLOAT 64)),
	    ("rawccall", rccType, P.RAW_CCALL NONE),
          (* Support for direct construction of C objects on ML heap.
           * rawrecord builds a record holding C objects on the heap.
           * rawselectxxx index on this record.  They are of type:
           *    'a * Word32.word -> Word32.word
           * The 'a is to guarantee that the compiler will treat
           * the record as a ML object, in case it passes thru a gc boundary.
           * rawupdatexxx writes to the record.
           *) 
	    ("rawrecord", i_x, P.RAW_RECORD { fblock = false }),
	    ("rawrecord64", i_x, P.RAW_RECORD { fblock = true }),
	    ("rawselectw8", xw32_w32, P.RAW_LOAD (P.UINT 8)),
	    ("rawselecti8", xw32_i32, P.RAW_LOAD (P.INT 8)),
	    ("rawselectw16", xw32_w32, P.RAW_LOAD (P.UINT 16)),
	    ("rawselecti16", xw32_i32, P.RAW_LOAD (P.INT 16)),
	    ("rawselectw32", xw32_w32, P.RAW_LOAD (P.UINT 32)),
	    ("rawselecti32", xw32_i32, P.RAW_LOAD (P.INT 32)),
	    ("rawselectf32", xw32_f64, P.RAW_LOAD (P.FLOAT 32)),
	    ("rawselectf64", xw32_f64, P.RAW_LOAD (P.FLOAT 64)),
	    ("rawupdatew8", xw32w32_u, P.RAW_STORE (P.UINT 8)),
	    ("rawupdatei8", xw32i32_u, P.RAW_STORE (P.INT 8)),
	    ("rawupdatew16", xw32w32_u, P.RAW_STORE (P.UINT 16)),
	    ("rawupdatei16", xw32i32_u, P.RAW_STORE (P.INT 16)),
	    ("rawupdatew32", xw32w32_u, P.RAW_STORE (P.UINT 32)),
	    ("rawupdatei32", xw32i32_u, P.RAW_STORE (P.INT 32)),
	    ("rawupdatef32", xw32f64_u, P.RAW_STORE (P.FLOAT 32)),
	    ("rawupdatef64", xw32f64_u, P.RAW_STORE (P.FLOAT 64))
	  ]
    end (* local *)

  end


