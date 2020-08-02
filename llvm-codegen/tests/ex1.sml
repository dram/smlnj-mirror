(* ex1.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Hand-crafted CFG for the function
 *
 *	fun f x = x;
 *
 * The generated first-order CPS is:
 *
 * ***********************************************
 *	std v78(v79[PV],v38[PV],v60[C],v61[PV],v62[PV],v63[PV],v45[PR2]) =
 *	   {RK_ESCAPE 1,(L)v64} -> v95
 *	   {v95} -> v96
 *	   v60(v60,v61,v62,v63,v96)
 *
 *	std v64(v86[PV],v85[PV],v84[C],v83[PV],v82[PV],v81[PV],v80[PR1]) =
 *	   {RK_ESCAPE 1,(L)v71} -> v94
 *	   v84(v84,v83,v82,v81,v94)
 *
 *	std v71(v93[PV],v92[PV],v91[C],v90[PV],v89[PV],v88[PV],v87[PV]) =
 *	   v91(v91,v90,v89,v88,v87)
 * ***********************************************
 *)

structure Ex1 =
  struct

    local
      structure P = CFG_Prim
      structure C = CFG
      structure II = IntInf
      fun v id = LambdaVar.fromId id
      fun V id = C.VAR(v id)

      fun record (flds, x, k) = let
	    val desc = ObjDesc.record(length flds)
	    in
	      C.ALLOC(P.RECORD{desc = desc, mut = false}, flds, x, k)
	    end
      fun pureOp (oper, args) = C.PURE(P.PURE_ARITH{oper=oper, sz=64}, args)
      fun num n = C.NUM{iv=n, signed=true, sz=64}
      fun fAttrs bp = { (* function attrs *)
	      isCont = false, alignHP = 8, needsBasePtr = bp, hasTrapArith = false, hasRCC = false
	    }
      fun cAttrs bp = { (* contiuation attrs *)
	      isCont = true, alignHP = 8, needsBasePtr = bp, hasTrapArith = false, hasRCC = false
	    }
      val unkProb = 0

      val fn78 = C.Cluster{
	      attrs = fAttrs true,
	      entry = C.Frag{
		  lab = v 78,
		  params = [
		      (v 79, C.PTRt), (v 38, C.PTRt), (v 60, C.CNTt), (v 61, C.PTRt),
		      (v 62, C.PTRt), (v 63, C.PTRt), (v 45, C.PTRt)
		    ],
		  allocChk = SOME 0w0,
		  body = record ([C.LABEL(v 64)], v 95,
		    record ([V 95], v 96,
		      C.THROW (V 60,
			[V 60, V 61, V 62, V 63, V 96],
			[C.CNTt, C.PTRt, C.PTRt, C.PTRt, C.PTRt])))
		},
	      frags = []
	    }
      val fn64 = C.Cluster{
	      attrs = fAttrs true,
	      entry = C.Frag{
		  lab = v 64,
		  params = [
		      (v 86, C.PTRt), (v 85, C.PTRt), (v 84, C.CNTt),
		      (v 83, C.PTRt), (v 82, C.PTRt), (v 81, C.PTRt), (v 80, C.PTRt)
		    ],
		  allocChk = SOME 0w0,
		  body = record ([C.LABEL(v 71)], v 94,
		    C.THROW (V 84,
		      [V 84, V 83, V 82, V 81, V 94],
		      [C.PTRt, C.PTRt, C.PTRt, C.PTRt, C.PTRt]))
		},
	      frags = []
	    }
      val fn71 = C.Cluster{
	      attrs = fAttrs false,
	      entry = C.Frag{
		  lab = v 71,
		  params = [
		      (v 93, C.PTRt), (v 92, C.PTRt), (v 91, C.CNTt), (v 90, C.PTRt),
		      (v 89, C.PTRt), (v 88, C.PTRt), (v 87, C.PTRt)
		    ],
		  allocChk = SOME 0w0,
		  body = C.APPLY(V 91,
		    [V 91, V 90, V 89, V 88, V 87],
		    [C.PTRt, C.PTRt, C.PTRt, C.PTRt, C.PTRt])
		},
	      frags = []
	    }
    in
    val cu = {srcFile = "id.sml", entry = fn78, fns = [fn64, fn71]}
    end (* local *)

  end
