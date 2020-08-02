(* ex4.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Hand-crafted CFG for the function
 *
 *      fun fact (n : Int64.int) = if (n <= 1) then 1 else n * fact(n-1);
 *
 * The generated first-order CPS is:
 *
 * ***********************************************
 *      std v96(v97[PV],v40[PV],v76[C],v77[PV],v78[PV],v79[PV],v53[PR2]) =
 *         {RK_ESCAPE 1,(L)v80} -> v121
 *         {v121} -> v122
 *         v76(v76,v77,v78,v79,v122)
 *
 *      std v80(v104[PV],v103[PV],v102[C],v101[PV],v100[PV],v99[PV],v98[I64]) =
 *         (L)v87(v102,v101,v100,v99,v98)
 *
 *      known_chk v87(v109[C],v108[PV],v107[PV],v106[PV],v105[I64]) =
 *         if i64>(v105,(I64)1) [v68] then
 *            isub64(v105,(I64)1) -> v44[I64]
 *            {RK_RAWBLOCK 1,v105} -> v119
 *            {RK_CONT 3,v109,v108,v119} -> v120
 *            (L)v87((L)v92,v120,v107,v106,v44)
 *         else
 *            v109(v109,v108,v107,v106,(I64)1)
 *
 *      std_cont v92(v114[PV],v113[PV],v112[PV],v111[PV],v110[I64]) =
 *         v113.2 -> v115[PV]
 *         v115.0 -> v116[I64]
 *         imul64(v116,v110) -> v42[I64]
 *         v113.1 -> v117[PV]
 *         v113.0 -> v118[C]
 *         v118(v118,v117,v112,v111,v42)
 * ***********************************************
 *
 * The equivalent CFG IR (with types omitted) is
 *
 * ***********************************************
 *      FUN v96 (v97, v40, v76, v77, v78, v79, v53) =
 *         ALLOC(RECORD 0x80, [LABEL v80]) -> v121
 *         ALLOC(RECORD 0x80, [VAR v121]) -> v122
 *           THROW (VAR v76) (VAR v76, VAR v77, VAR v78, VAR v79, VAR v122)
 *
 *      FUN v80 (v104, v103, v102, v101, v100, v99, v98]) =
 *         GOTO v87 (VAR v102, VAR v101. VAR v100, VAR v99, VAR v98)
 *
 *      FRAG v87 (v109, v108, v107, v106, v105) =
 *	   CHK_GC(NONE,
 *            IF IGT(VAR v105, NUM(1:i64)) THEN
 *               ISUB(v105, NUM(1:i64)) -> v44
 *               ALLOC(RAW_RECORD(, i64), [v105]) -> v119
 *               ALLOC(RECORD , [v109, v108, v119]) -> v120
 *                 GOTO v87 (LABEL 92, VAR v120, VAR v107, VAR v106, VAR v44)
 *            ELSE
 *               THROW (VAR v109) (VAR v109, VAR v108, VAR v107, VAR v106, NUM(1:i64))
 *
 *      CONT v92 (v114, v113, v112, v111, v110) =
 *	   IMUL(RAW_SELECT(0, SELECT(2, v113)), VAR v110) -> v42
 *         LET SELECT(0, VAR v113) -> v118
 *           THROW (VAR v118) [VAR v118, SELECT(1, VAR v113), VAR v112, VAR v111, VAR v42]
 * ***********************************************
 *)

structure Ex4 =
  struct

    local
      structure P = CFG_Prim
      structure C = CFG
      structure II = IntInf
      fun v id = LambdaVar.fromId id
      fun V id = C.VAR(v id)
      fun LAB id = C.LABEL(v id)

      fun num n = C.NUM{iv=n, signed=true, sz=64}
      fun record (flds, x, k) = let
	    val desc = ObjDesc.record(length flds)
	    in
	      C.ALLOC(P.RECORD{desc = desc, mut = false}, flds, x, k)
	    end
      fun rawRecord (flds, x, k) = let
	    val desc = ObjDesc.rawRecord(length flds)
	    in
	      C.ALLOC(P.RAW_RECORD{desc = desc, kind = P.INT, sz = 64}, flds, x, k)
	    end
      fun pureOp (oper, args) = C.PURE(P.PURE_ARITH{oper=oper, sz=64}, args)
      fun arith (oper, args, res, k) = C.ARITH(P.ARITH{oper=oper, sz=64}, args, (res, C.NUMt 64), k)
      fun rawSelect (i, v) = C.PURE(P.PURE_RAW_SUBSCRIPT{kind=P.INT, sz=64}, [v, num i])
      fun num n = C.NUM{iv=n, signed=true, sz=64}
      fun fAttrs bp = { (* function attrs *)
	      isCont = false, alignHP = 8, needsBasePtr = bp, hasTrapArith = false, hasRCC = false
	    }
      fun cAttrs bp = { (* contiuation attrs *)
	      isCont = true, alignHP = 8, needsBasePtr = bp, hasTrapArith = false, hasRCC = false
	    }
      val unkProb = 0

      val fn96 = C.Cluster{
	      attrs = fAttrs true,
	      entry = C.Frag{
		  lab = v 96,
		  params = [
		      (v 97, C.PTRt), (v 40, C.PTRt), (v 76, C.CNTt), (v 77, C.PTRt),
		      (v 78, C.PTRt), (v 79, C.PTRt), (v 53, C.PTRt)
		    ],
		  allocChk = SOME 0w0,
		  body = record ([LAB 80], v 121,
		    record ([V 121], v 122,
		      C.THROW (V 76,
			[V 76, V 77, V 78, V 79, V 122],
			[C.CNTt, C.PTRt, C.PTRt, C.PTRt, C.PTRt])))
		},
	      frags = []
	    }
      val fn87 = C.Frag{
	      lab = v 87,
	      params = [
		  (v 109, C.CNTt), (v 108, C.PTRt), (v 107, C.PTRt),
		  (v 106, C.PTRt), (v 105, C.NUMt 64)
		],
	      allocChk = SOME 0w0,
	      body = C.BRANCH(
		P.CMP{oper=P.GT, signed=true, sz=64}, [V 105, num 1],
		unkProb,
		(* then *)
		  arith(P.ISUB, [V 105, num 1], v 44,
		  rawRecord([V 105], v 119,
		  record([V 109, V 108, V 119], v 120,
		    C.GOTO(v 87,
		      [LAB 92, V 120, V 107, V 106, V 44])))),
		(* else *)
		  C.THROW(V 109,
		    [V 109, V 108, V 107, V 106, num 1],
		    [C.CNTt, C.PTRt, C.PTRt, C.PTRt, C.NUMt 64]))
	    }
      val fn80 = C.Cluster{
	      attrs = { (* function attrs *)
	          isCont = false, alignHP = 8, needsBasePtr = true,
		  hasTrapArith = true, hasRCC = false
	        },
	      entry = C.Frag{
		  lab = v 80,
		  params = [
		      (v 104, C.PTRt), (v 103, C.PTRt), (v 102, C.CNTt),
		      (v 101, C.PTRt), (v 100, C.PTRt), (v 99, C.PTRt),
		      (v 98, C.NUMt 64)
		    ],
		  allocChk = NONE,
		  body = C.GOTO (v 87, [V 102, V 101, V 100, V 99, V 98])
		},
	      frags = [fn87]
	    }
      val fn92 = C.Cluster{
	      attrs = cAttrs false,
	      entry = C.Frag{
		  lab = v 92,
		  params = [
		      (v 114, C.PTRt), (v 113, C.PTRt), (v 112, C.PTRt),
		      (v 111, C.PTRt), (v 110, C.NUMt 64)
		    ],
		  allocChk = SOME 0w0,
		  body =
		    arith(P.IMUL, [rawSelect(0, C.SELECT(2, V 113)), V 110], v 42,
		    C.LET(C.SELECT(0, V 113), (v 118, C.CNTt),
		      C.THROW (V 118,
			[V 118, C.SELECT(1, V 113), V 112, V 111, V 42],
			[C.CNTt, C.PTRt, C.PTRt, C.PTRt, C.NUMt 64])))
		},
	      frags = []
	    }
    in
    val cu = {srcFile = "rev.sml", entry = fn96, fns = [fn80, fn92]}
    end (* local *)

  end
