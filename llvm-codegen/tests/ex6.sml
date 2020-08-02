(* ex6.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Hand-crafted CFG for the function
 *
 *      fun try (f, h, x) = (f x) handle ex => h ex;
 *
 * The generated first-order CPS is:
 *
 * ***********************************************
 *      std v181(v182[PV],v137[PV],v170[C],v171[PV],v172[PV],v173[PV],v148[PR2]) =
 *         {RK_ESCAPE 1,(L)v174} -> v190
 *         {v190} -> v191
 *         v170(v170,v171,v172,v173,v191)
 *
 *      std v174(v189[PV],v188[PV],v187[C],v186[PV],v185[PV],v184[PV],v183[PV]) =
 *         case v183  [165] of
 *          0 => v187(v187,v186,v185,v184,(I63t)0)
 *          1 => v187(v187,v186,v185,v184,(I63t)1)
 *          2 => v187(v187,v186,v185,v184,(I63t)2)
 *          3 => v187(v187,v186,v185,v184,(I63t)3)
 *          4 => v187(v187,v186,v185,v184,(I63t)4)
 *          5 => v187(v187,v186,v185,v184,(I63t)5)
 * ***********************************************
 *
 * The equivalent CFG IR (with types omitted) is
 *
 * ***********************************************
 *      FUN v181 (v182, v137, v170, v171, v172, v173, v148) =
 *         ALLOC(RECORD 0x80, [LABEL v174]) -> v191
 *         ALLOC(RECORD 0x80, [VAR v190]) -> v378
 *           THROW (VAR v170) (VAR v170, VAR v171, VAR v172, VAR v173, VAR v191)
 *
 *      FUN v174 (v189, v188, v187, v186, v185, v184, v183) =
 *	   SWITCH (VAR v183)
 *	     case 0: THROW (VAR v187) (VAR v187, VAR v186, VAR v185, VAR v184, NUM 0)
 *	     case 1: THROW (VAR v187) (VAR v187, VAR v186, VAR v185, VAR v184, NUM 1)
 *	     case 2: THROW (VAR v187) (VAR v187, VAR v186, VAR v185, VAR v184, NUM 2)
 *	     case 3: THROW (VAR v187) (VAR v187, VAR v186, VAR v185, VAR v184, NUM 3)
 *	     case 4: THROW (VAR v187) (VAR v187, VAR v186, VAR v185, VAR v184, NUM 4)
 *	     case 5: THROW (VAR v187) (VAR v187, VAR v186, VAR v185, VAR v184, NUM 5)
 * ***********************************************
 *)

structure Ex6 =
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
      fun num n = C.NUM{iv=n, signed=true, sz=64}
      fun fAttrs bp = { (* function attrs *)
	      isCont = false, alignHP = 8, needsBasePtr = bp, hasTrapArith = false, hasRCC = false
	    }
      fun cAttrs bp = { (* contiuation attrs *)
	      isCont = true, alignHP = 8, needsBasePtr = bp, hasTrapArith = false, hasRCC = false
	    }
      val unkProb = 0

      val fn181 = C.Cluster{
	      attrs = fAttrs true,
	      entry = C.Frag{
		  lab = v 181,
		  params = [
		      (v 182, C.PTRt), (v 137, C.PTRt), (v 170, C.CNTt), (v 171, C.PTRt),
		      (v 172, C.PTRt), (v 173, C.PTRt), (v 148, C.PTRt)
		    ],
		  allocChk = SOME 0w0,
		  body = record ([LAB 174], v 190,
		    record ([V 190], v 191,
		      C.THROW (V 170,
			[V 170, V 171, V 172, V 173, V 191],
			[C.CNTt, C.PTRt, C.PTRt, C.PTRt, C.PTRt])))
		},
	      frags = []
	    }
      fun return res = C.THROW (V 187,
	    [V 187, V 186, V 185, V 184, num 0],
	    [C.CNTt, C.PTRt, C.PTRt, C.PTRt, C.NUMt 64])
      val fn174 = C.Cluster{
	      attrs = fAttrs true,
	      entry = C.Frag{
		  lab = v 174,
		  params = [
		      (v 189, C.PTRt), (v 188, C.PTRt), (v 187, C.CNTt), (v 186, C.PTRt),
		      (v 185, C.PTRt), (v 184, C.PTRt), (v 183, C.PTRt)
		    ],
		  allocChk = SOME 0w0,
		  body = C.SWITCH(V 183, [
		      return 0,
		      return 1,
		      return 2,
		      return 3,
		      return 4,
		      return 5
		    ])
		},
	      frags = []
	    }
    in
    val cu = {srcFile = "switch.sml", entry = fn181, fns = [fn174]}
    end (* local *)

  end
