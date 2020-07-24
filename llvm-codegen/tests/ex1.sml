(* ex1.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Hand-crafted CFG for the function
 *
 *	fun rev xs = let
 *	      fun rev' ([], xs') = xs'
 *		| rev' (x::xs, xs') = rev' (xs, x::xs')
 *	      in
 *		rev' (xs, [])
 *	      end;
 *
 * The generated first-order CPS is:
 *
 * ***********************************************
 *	std v130 (v131[PV], v31[PV], v111[C], v112[PV], v113[PV], v114[PV], v53[PR2]) =
 *	   {RK_ESCAPE 1, (L)v115} -> v156
 *	   {v156} -> v157
 *	   v111 (v111, v112, v113, v114, v157)
 *
 *	std v115 (v138[PV], v137[PV], v136[C], v135[PV], v134[PV], v133[PV], v132[PR1]) =
 *	   {RK_ESCAPE 1, (L)v122} -> v155
 *	   v136 (v136, v135, v134, v133, v155)
 *
 *	std v122 (v145[PV], v144[PV], v143[C], v142[PV], v141[PV], v140[PV], v139[PV]) =
 *	   (L)v129 (v139, (I63t)0, v143, v142, v141, v140)
 *
 *	known_chk v129 (v151[PV], v150[PV], v149[C], v148[PV], v147[PV], v146[PV]) =
 *	   if boxed(v151) [v105] then
 *	      v151.0 -> v152[PV]
 *	      v151.1 -> v153[PV]
 *	      {v152, v150} -> v154
 *	      (L)v129 (v153, v154, v149, v148, v147, v146)
 *	   else
 *	      v149 (v149, v148, v147, v146, v150)
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
	    val desc = II.<<(II.fromInt(length flds), 0w7)
	    in
	      C.ALLOC(P.RECORD{desc = desc, mut = false}, flds, (x, C.PTRt), k)
	    end
      fun pureOp (oper, args) = C.PURE(P.PURE_ARITH{oper=oper, sz=64}, args)
      fun goto (lab, args) = C.GOTO(C.KNOWN_CHK, lab, args)
      fun num n = C.NUM{iv=n, signed=true, sz=64}
      val attrs = {alignHP = 8, needsBasePtr = true, hasTrapArith = false, hasRCC = false},
      val unkProb = 0

      val fn130 = C.Cluster{
	      attrs = attrs,
	      entry = C.Entry{
		  cc = C.STD_FUN, lab = v 130,
		  params = [
		      (v 131, C.PTRt), (v 31, C.PTRt), (v 111, C.CNTt), (v 112, C.PTRt),
		      (v 113, C.PTRt), (v 114, C.PTRt), (v 53, C.PTRt)
		    ],
		  body = record ([C.LABEL(v 115)], v 156,
		    record ([V 156], v 157,
		      C.APPLY ([V 111, V 111, V 112, V 113, V 114, V 157])))
		},
	      frags = []
	    }
      val fn115 = C.Cluster{
	      attrs = attrs,
	      entry = C.Entry{
		  cc = C.STD_FUN, lab = v 115,
		  params = [
		      (v 138, C.PTRt), (v 137, C.PTRt), (v 136, C.CNTt),
		      (v 135, C.PTRt), (v 134, C.PTRt), (v 133, C.PTRt), (v 132, C.PTRt)
		    ],
		  body = record ([C.LABEL(v 122)], v 155,
		    C.APPLY ([V 136, V 136, V 135, V 134, V 133, V 155]))
		},
	      frags = []
	    }
      val fn129 = C.Frag{
	      gcCheck = true, lab = v 129,
	      params = [
		  (v 151, C.PTRt), (v 150, C.PTRt), (v 149, C.CNTt),
		  (v 148, C.PTRt), (v 147, C.PTRt), (v 146, C.PTRt)
		],
	      body = C.BRANCH(
		  P.CMP{oper=P.EQL, signed=false, sz=64},
		  [pureOp(P.ANDB, [V 151, num 1]), num 0],
		  unkProb,
		  (* then *)
		    select(0, V 151, (v 152, C.PTRt),
		    select(1, V 151, (v 153, C.PTRt),
		    record([V 152, V 150], v 154,
		      C.GOTO(C.KNONW_CHK, v 129, [
			  V 153, V 154, V 149, V 148, V 147, V 146
			],
			[
			  C.PTRt, C.PTRt, C.CNTt, C.PTRt, C.PTRt, C.PTRt
			])))),
		  (* else *)
		    C.THROW[V 149, V 149, V 148, V 147, V 146, V 150])
	    }
      val fn122 = C.Cluster{
	      attrs = attrs,
	      entry = C.Entry{
		  cc = C.STD_FUN, lab = v 122,
		  params = [
		      (v 145, C.PTRt), (v 144, C.PTRt), (v 143, C.CNTt), (v 142, C.PTRt),
		      (v 141, C.PTRt), (v 140, C.PTRt), (v 139, C.PTRt)
		    ],
		  body = C.GOTO (C.KNOWN_CHK, v 129, [
			V 139, num 1, V 143, V 142, V 141, V 140
		      ], [
			C.PTRt, C.PTRt, C.CNTt, C.PTRt, C.PTRt, C.PTRt
		      ])
		},
	      frags = [fn129]
	    }
    in
    val cu = {srcFile = "rev.sml", entry = fn130, fns = [fn115, fn122]}
    end (* local *)

  end
