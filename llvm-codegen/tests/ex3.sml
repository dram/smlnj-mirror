(* ex3.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Hand-crafted CFG for the function
 *
 *      fun madd (a : real, b, c) = a * b + c;
 *
 * The generated first-order CPS is:
 *
 * ***********************************************
 *      std v80 (v81[PV], v44[PV], v69[C], v70[PV], v71[PV], v72[PV], v57[PR2]) =
 *         {RK_ESCAPE 1, (L)v73} -> v91
 *         {v91} -> v92
 *         v69(v69, v70, v71, v72, v92)
 *
 *      std v73 (v90[PV], v89[PV], v88[C], v87[PV], v86[PV], v85[PV], v84[R64], v83[R64], v82[R64]) =
 *         mulf64(v84, v83) -> v46[R64]
 *         addf64(v46, v82) -> v45[R64]
 *         v88(v88, v87, v86, v85, v45)
 * ***********************************************
 *
 * The equivalent CFG IR (with types omitted) is
 *
 * ***********************************************
 *      FUN v80 (v81, v44, v69, v70, v71, v72, v57) =
 *         ALLOC(RECORD 0x80, [LABEL v73]) -> v91
 *         ALLOC(RECORD 0x80, [VAR 91]) -> v92
 *           THROW (VAR v69) (VAR v69, VAR v70, VAR v71, VAR v72, VAR v92)
 *
 *      FUN v73 (v90, v89, v88, v87, v86, v85, v84, v83, v82) =
 *         THROW (VAR v88) (VAR v88, VAR v87, VAR v86, VAR v85, FADD(FMUL(VAR v84, VAR v83), VAR v82))
 * ***********************************************
 *)

structure Ex3 =
  struct

    local
      structure P = CFG_Prim
      structure C = CFG
      structure II = IntInf
      fun v id = LambdaVar.fromId id
      fun V id = C.VAR(v id)
      fun LAB id = C.LABEL(v id)

      fun num n = C.NUM{iv=n, sz=64}
      fun record (flds, x, k) = let
	    val desc = ObjDesc.record(length flds)
	    in
	      C.ALLOC(P.RECORD{desc = desc, mut = false}, flds, x, k)
	    end
      fun pureOp (oper, args) = C.PURE(P.PURE_ARITH{oper=oper, sz=64}, args)
      fun fAttrs bp = { (* function attrs *)
	      isCont = false, alignHP = 8, needsBasePtr = bp, hasTrapArith = false, hasRCC = false
	    }
      fun cAttrs bp = { (* contiuation attrs *)
	      isCont = true, alignHP = 8, needsBasePtr = bp, hasTrapArith = false, hasRCC = false
	    }
      val unkProb = 0

      val fn80 = C.Cluster{
	      attrs = fAttrs true,
	      entry = C.Frag{
		  lab = v 80,
		  params = [
		      (v 81, C.PTRt), (v 44, C.PTRt), (v 69, C.CNTt), (v 70, C.PTRt),
		      (v 71, C.PTRt), (v 72, C.PTRt), (v 57, C.PTRt)
		    ],
		  allocChk = SOME 0w0,
		  body = record ([LAB 73], v 91,
		    record ([V 91], v 92,
		      C.THROW (V 69,
			[V 69, V 70, V 71, V 72, V 92],
			[C.CNTt, C.PTRt, C.PTRt, C.PTRt, C.PTRt])))
		},
	      frags = []
	    }
      val fn73 = C.Cluster{
	      attrs = fAttrs false,
	      entry = C.Frag{
		  lab = v 73,
		  params = [
		      (v 90, C.PTRt), (v 89, C.PTRt), (v 88, C.CNTt),
		      (v 87, C.PTRt), (v 86, C.PTRt), (v 85, C.PTRt),
		      (v 84, C.FLTt 64), (v 83, C.FLTt 64), (v 82, C.FLTt 64)
		    ],
		  allocChk = SOME 0w0,
		  body = C.THROW (V 88, [
		        V 88, V 87, V 86, V 85,
			pureOp(P.FADD, [pureOp(P.FMUL, [V 84, V 83]), V 82])
		      ],
		    [C.PTRt, C.PTRt, C.PTRt, C.PTRt, C.FLTt 64])
		},
	      frags = []
	    }
    in
    val cu = {srcFile = "fmadd.sml", entry = fn80, fns = [fn73]}
    end (* local *)

  end
