(* pure-arith.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Generate CFG expressions for "pure" arithmetic operations.
 *)

structure PureArith : sig

    val tagged : (CPS.value -> CFG.exp)
	  -> CPS.P.pureop * CPS.value list -> CFG.exp

    val signed : (CPS.value -> CFG.exp)
	  -> CPS.P.pureop * int * CPS.value list -> CFG.exp

    val unsigned : (CPS.value -> CFG.exp)
	  -> CPS.P.pureop * int * CPS.value list -> CFG.exp

    val float : CPS.P.pureop * int * CPS.value list -> CFG.exp`
	  -> CPS.P.pureop * CPS.value list -> CFG.exp

  end = struct

    structure P = CFG_Prim
    structure C = CFG

    datatype pureop = datatype CPS.P.pureop
    datatype value = datatype CPS.value

    fun error msg = ??.error("PureArith", msg)

    val ity = Target.mlValueSz

    fun pureOp (rator, sz, args) = C.PURE(P.PURE_ARITH{oper=rator, sz=sz}, args)

    val one = C.NUM(1, ity)

  (* Tagged integer operations *)
    fun addTag e = pureOp (P.ADD, ity, e, one)
    fun stripTag e = pureOp (P.SUB, ity, e, one)
    fun orTag e = pureOp (P.ORB, ity, e, one)
    fun untagInt e = pureOp (P.RSHIFT, ity, e, one)
    fun untagUInt e = pureOp (P.RSHIFTL, ity, e one)

  (* unsigned tagged arithmetic; a number "n" is represented as "2*n+1" *)
    fun tagged comp (ADD, [NUM{ival, ...}, v2]) =
	  pureOp (P.ADD, sz, [C.NUM(ival+ival, ity), comp v2])
      | tagged comp (ADD, [v1, NUM{ival, ...}]) =
	  pureOp (P.ADD, sz, [comp v1, C.NUM(ival+ival, ity)])
      | tagged comp (ADD, [v1, v2]) =
	  pureOp (P.ADD, sz, [comp v1, comp v2])
      | tagged comp (SUB, [NUM{ival, ...}, v2]) =
	  pureOp (P.SUB, sz, [C.NUM(ival+ival+2, sz), comp v2])
      | tagged comp (SUB, [v1, NUM{ival, ...}]) =
	  pureOp (P.SUB, sz, [comp v1, C.NUM(ival+ival, ity)])
      | tagged comp (SUB, [v1, v2]) =
	  addTag(pureOp (P.SUB, sz, [comp v1, comp v2]))
      | tagged comp (MUL, [v1, v2]) = let
	  val (e1, e2) = (case (v1, v2)
		 of (NUM{ival, ...}, _) => (C.NUM(ival, ity), untagUInt (comp v2))
		  | (_, NUM{ival, ...}) =>  (untagUInt (comp e1), C.NUM(ival, ity))
		  | _ => (stripTag (comp v1), untagUInt (comp v2))
		(* end case *))
	  in
	    addTag (pureOp (P.MUL, ity, e1, e2))
	  end
      | tagged comp (QUOT, [v1, v2]) = let
	  val e1 = (case v1
		 of NUM{ival, ...} => C.NUM(ival, ity)
		  | _ => untagUInt (comp v1)
		(* end case *))
	  val e2 = (case v2
		 of NUM{ival, ...} => C.NUM(ival, ity)
		  | _ => untagUInt (comp v2)
		(* end case *))
	  in
	    pureOp (P.UDIV, ity, e1, e2)
	  end
      | tagged comp (REM, [v1, v2]) = let
	  val e1 = (case v1
		 of NUM{ival, ...} => C.NUM(ival, ity)
		  | _ => untagUInt (comp v1)
		(* end case *))
	  val e2 = (case v2
		 of NUM{ival, ...} => C.NUM(ival, ity)
		  | _ => untagUInt (comp v2)
		(* end case *))
	  in
	    pureOp (P.UREM, ity, e1, e2)
	  end
      | tagged comp (NEG, [v]) =
      | tagged comp (LSHIFT, [v1, v2]) =
      | tagged comp (RSHIFT, [v1, v2]) =
      | tagged comp (RSHIFTL, [v1, v2]) =
      | tagged comp (ORB, [v1, v2]) =
	  addTag(pureOp (P.ORB, sz, [comp v1, comp v2]))
      | tagged comp (XORB, [C.NUM(n, sz), e2]) = pureOp (P.XORB, sz, [C.NUM(n+n, sz), e2])
      | tagged comp (XORB, [e1, C.NUM(n, sz)]) = pureOp (P.XORB, sz, [e1, C.NUM(n+n, sz)])
      | tagged comp (XORB, args) = pureOp (P.XORB, sz, args)
      | tagged comp (ANDB, [v1, v2]) =
	  addTag(pureOp (P.ANDB, sz, [comp v1, comp v2]))
      | tagged comp (NOTB, [v]) =
      | tagged comp (rator, _) = error ""


    fun signed comp (rator, sz, vs) = (case (rator, vs)
	   of ADD
	    | SUB
	    | MUL
	    | QUOT
	    | REM
	    | NEG
	    | LSHIFT
	    | RSHIFT
	    | RSHIFTL
	    | ORB
	    | XORB
	    | ANDB
	    | NOTB
	    | _ => error ""
	  (* end case *))
	  -> CPS.P.pureop * bool * CPS.value list -> CFG.exp

    fun unsigned comp (rator, sz, vs) = (case (rator, vs)
	   of ADD
	    | SUB
	    | MUL
	    | QUOT
	    | REM
	    | NEG
	    | LSHIFT
	    | RSHIFT
	    | RSHIFTL
	    | ORB
	    | XORB
	    | ANDB
	    | NOTB
	    | _ => error ""
	  (* end case *))

    fun float comp (rator, sz, vs) = (case (rator, List.map comp vs)
	   of (ADD, args) => pureOp (P.FADD, sz, args)
	    | (SUB, args) => pureOp (P.FSUB, sz, args)
	    | (MUL, args) => pureOp (P.FMUL, sz, args)
	    | (FDIV, args) => pureOp (P.FDIV, sz, args)
	    | (FABS, args) => pureOp (P.FABS, sz, args)
	    | (FSQRT, args) => pureOp (P.FSQRT, sz, args)
	    | (FSIN, args) => pureOp (P.FSIN, sz, args)
	    | (FCOS, args) => pureOp (P.FCOS, sz, args)
	    | (FTAN, args) => pureOp (P.FTAN, sz, args)
	    | _ => error ""
	  (* end case *))

  end
