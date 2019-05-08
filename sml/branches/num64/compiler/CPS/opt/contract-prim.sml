(* contract-prim.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Contraction for CPS primitive operations.
 *)

structure ContractPrim : sig

  (* information about a variable *)
    datatype info
      = FNinfo of {
	    args: CPS.lvar list,
	    body : CPS.cexp option ref,
	    specialuse: int ref option ref,
	    liveargs : bool list option ref
	  }
      | RECinfo of CPS.record_kind * (CPS.value * CPS.accesspath) list
      | SELinfo of int * CPS.value * CPS.cty
      | OFFinfo of int * CPS.value
      | WRPinfo of CPS.P.numkind * CPS.value			(* CPS.P.wrap of a value *)
      | IFIDIOMinfo of {body : (CPS.lvar * CPS.cexp * CPS.cexp) option ref}
      | MISCinfo of CPS.cty

    type get_info = CPS.lvar -> {info: info, used : int ref, called : int ref}

    val infoToString : info -> string

    val arith : CPS.P.arith * CPS.value list -> CPS.value option

    val pure : get_info -> CPS.P.pure * CPS.value list -> CPS.value option

    val branch : get_info -> CPS.P.branch * CPS.value list -> bool option

  end = struct

    structure P = CPS.P
    structure CA = ConstArith

    fun bug s = ErrorMsg.impossible ("ContractPrim: " ^ s)

    datatype value = datatype CPS.value

  (* information about a variable *)
    datatype info
      = FNinfo of {
	    args: CPS.lvar list,
	    body : CPS.cexp option ref,
	    specialuse: int ref option ref,
	    liveargs : bool list option ref
	  }
      | RECinfo of CPS.record_kind * (value * CPS.accesspath) list
      | SELinfo of int * value * CPS.cty
      | OFFinfo of int * value
      | WRPinfo of CPS.P.numkind * value			(* CPS.P.wrap of a value *)
      | IFIDIOMinfo of {body : (CPS.lvar * CPS.cexp * CPS.cexp) option ref}
      | MISCinfo of CPS.cty

    type get_info = CPS.lvar -> {info: info, used : int ref, called : int ref}

    fun infoToString info = (case info
	   of FNinfo _ => "FNinfo{...}"
	    | RECinfo(_, args) => concat[
		  "RECinfo(_, [", String.concatWithMap "," PPCps.vpathToString args, "])"
		]
	    | SELinfo(i, v, cty) => concat[
		  "SELinfo(", Int.toString i, ", ", PPCps.value2str v, ", ",
		  CPSUtil.ctyToString cty, ")"
		]
	    | OFFinfo(i, v) => concat[
		  "OFFinfo(", Int.toString i, ", ", PPCps.value2str v, ")"
		]
	    | WRPinfo(nk, v) => concat[
		  "WRPinfo(", PPCps.numkindToString nk, ", ", PPCps.value2str v, ")"
		]
	    | IFIDIOMinfo _ => "IFIDIOMinfo{...}"
	    | MISCinfo cty => concat[
		  "MISCinfo(", CPSUtil.ctyToString cty, ")"
		]
	  (* end case *))

    (* integer types/values *)
    local
      val tt = {sz = Target.defaultIntSz, tag = true}
    in
    fun tagInt n = NUM{ival = IntInf.fromInt n, ty = tt}
    end

  (* get the size of an integer operation *)
    fun sizeOfKind (P.INT sz) = sz
      | sizeOfKind (P.UINT sz) = sz
      | sizeOfKind (P.FLOAT _) = bug "sizeOfKind(FLOAT _)"

  (* contraction for impure arithmetic operations; note that 64-bit IMUL, IDIV,
   * IMOD, IQUOT, and IREM have three arguments on 32-bit targets, so we need
   * to allow for the extra argument in the patterns.
   *)
    fun arith (rator, args) = ((case (rator, args)
	   of (P.IARITH{oper=P.IMUL, ...}, NUM{ival=1, ...} :: v :: _) => SOME v
	    | (P.IARITH{oper=P.IMUL, ...}, v :: NUM{ival=1, ...} :: _) => SOME v
	    | (P.IARITH{oper=P.IMUL, ...}, (v as NUM{ival=0, ...}) :: _) => SOME v
	    | (P.IARITH{oper=P.IMUL, ...}, _ :: (v as NUM{ival=0, ...}) :: _) => SOME v
	    | (P.IARITH{oper=P.IMUL, sz=sz}, NUM i :: NUM j :: _) =>
		SOME(NUM{ival = CA.sMul(sz, #ival i, #ival j), ty = #ty i})
	    | (P.IARITH{oper=P.IQUOT, ...}, v :: NUM{ival=1, ...} :: _) => SOME v
	    | (P.IARITH{oper=P.IQUOT, ...}, _ :: NUM{ival=0, ...} :: _) => NONE
	    | (P.IARITH{oper=P.IQUOT, sz=sz}, NUM i :: NUM j :: _) =>
		SOME(NUM{ival = CA.sQuot(sz, #ival i, #ival j), ty = #ty i})
	    | (P.IARITH{oper=P.IDIV, ...}, v :: NUM{ival=1, ...} :: _) => SOME v
	    | (P.IARITH{oper=P.IDIV, ...}, _ :: NUM{ival=0, ...} :: _) => NONE
	    | (P.IARITH{oper=P.IDIV, sz=sz}, NUM i :: NUM j :: _) =>
		SOME(NUM{ival = CA.sDiv(sz, #ival i, #ival j), ty = #ty i})
	    (* FIXME: should we do anything for mod or rem here? *)
	    | (P.IARITH{oper=P.IADD, ...}, [NUM{ival=0, ...}, v]) => SOME v
	    | (P.IARITH{oper=P.IADD, ...}, [v, NUM{ival=0, ...}]) => SOME v
	    | (P.IARITH{oper=P.IADD, sz=sz}, [NUM i, NUM j]) =>
		SOME(NUM{ival = CA.sAdd(sz, #ival i, #ival j), ty = #ty i})
	    | (P.IARITH{oper=P.ISUB, ...}, [v, NUM{ival=0, ...}]) => SOME v
	    | (P.IARITH{oper=P.ISUB, sz=sz}, [NUM i, NUM j]) =>
		SOME(NUM{ival = CA.sSub(sz, #ival i, #ival j), ty = #ty i})
	    | (P.IARITH{oper=P.INEG, sz=sz}, [NUM i]) =>
		SOME(NUM{ival = CA.sNeg(sz, #ival i), ty = #ty i})
	    | _ => NONE
	  (* end case *))
	    handle _ => NONE)

  (* contraction for pure operations; note that 64-bit MUL, QUOT, and REM
   * have three arguments on 32-bit targets, so we need to allow for the
   * extra argument in the patterns.
   *)
    fun pure (get : get_info) arg = (case arg
	   of (P.PURE_ARITH{oper=P.MUL, ...}, NUM{ival=1, ...} :: v :: _) => SOME v
	    | (P.PURE_ARITH{oper=P.MUL, ...}, v :: NUM{ival=1, ...} :: _) => SOME v
	    | (P.PURE_ARITH{oper=P.MUL, ...}, (v as NUM{ival=0, ...}) :: _) => SOME v
	    | (P.PURE_ARITH{oper=P.MUL, ...}, _ :: (v as NUM{ival=0, ...}) :: _) => SOME v
	    | (P.PURE_ARITH{oper=P.MUL, kind=P.UINT sz}, NUM i :: NUM j :: _) =>
		SOME(NUM{ival = CA.uMul(sz, #ival i, #ival j), ty = #ty i})
	    | (P.PURE_ARITH{oper=P.QUOT, ...}, v :: NUM{ival=1, ...} :: _) => SOME v
	    | (P.PURE_ARITH{oper=P.QUOT, ...}, _ :: NUM{ival=0, ...} :: _) => NONE
	    | (P.PURE_ARITH{oper=P.QUOT, kind=P.UINT sz}, NUM i :: NUM j :: _) =>
		SOME(NUM{ival = CA.uDiv(sz, #ival i, #ival j), ty = #ty i})
	    | (P.PURE_ARITH{oper=P.ADD, ...}, [NUM{ival=0, ...}, v]) => SOME v
	    | (P.PURE_ARITH{oper=P.ADD, ...}, [v, NUM{ival=0, ...}]) => SOME v
	    | (P.PURE_ARITH{oper=P.ADD, kind=P.UINT sz}, [NUM i, NUM j]) =>
		SOME(NUM{ival = CA.uAdd(sz, #ival i, #ival j), ty = #ty i})
	    | (P.PURE_ARITH{oper=P.SUB, ...}, [v, NUM{ival=0, ...}]) => SOME v
	    | (P.PURE_ARITH{oper=P.SUB, kind=P.UINT sz}, [NUM i, NUM j]) =>
		SOME(NUM{ival = CA.uSub(sz, #ival i, #ival j), ty = #ty i})
	    | (P.PURE_ARITH{oper=P.RSHIFT, ...}, [i as NUM{ival=0, ...}, _]) => SOME i
	    | (P.PURE_ARITH{oper=P.RSHIFT, ...}, [v, NUM{ival=0, ...}]) => SOME v
	    | (P.PURE_ARITH{oper=P.RSHIFT, kind}, [NUM i, NUM j]) =>
		SOME(NUM{ival = CA.sShR(sizeOfKind kind, #ival i, #ival j), ty = #ty i})
	    | (P.PURE_ARITH{oper=P.RSHIFTL, ...}, [i as NUM{ival=0, ...}, _]) => SOME i
	    | (P.PURE_ARITH{oper=P.RSHIFTL, ...}, [v, NUM{ival=0, ...}]) => SOME v
	    | (P.PURE_ARITH{oper=P.RSHIFTL, kind=P.UINT sz}, [NUM i, NUM j]) =>
		SOME(NUM{ival = CA.uShR(sz, #ival i, #ival j), ty = #ty i})
	    | (P.PURE_ARITH{oper=P.LSHIFT, ...}, [v as NUM{ival=0, ...}, _]) => SOME v
	    | (P.PURE_ARITH{oper=P.LSHIFT, ...}, [v, NUM{ival=0, ...}]) => SOME v
	    | (P.PURE_ARITH{oper=P.LSHIFT, kind=P.INT sz}, [NUM i, NUM j]) => (
		SOME(NUM{ival = CA.sShL(sz, #ival i, #ival j), ty = #ty i})
		  handle Overflow => NONE)
	    | (P.PURE_ARITH{oper=P.LSHIFT, kind=P.UINT sz}, [NUM i, NUM j]) =>
		SOME(NUM{ival = CA.uShL(sz, #ival i, #ival j), ty = #ty i})
	    | (P.PURE_ARITH{oper=P.ANDB, ...}, [v as NUM{ival=0, ...}, _]) => SOME v
	    | (P.PURE_ARITH{oper=P.ANDB, ...}, [_, v as NUM{ival=0, ...}]) => SOME v
	    | (P.PURE_ARITH{oper=P.ANDB, kind}, [NUM i, NUM j]) =>
		SOME(NUM{ival = CA.bAnd(sizeOfKind kind, #ival i, #ival j), ty = #ty i})
	    | (P.PURE_ARITH{oper=P.ORB, ...}, [NUM{ival=0, ...}, v]) => SOME v
	    | (P.PURE_ARITH{oper=P.ORB, ...}, [v, NUM{ival=0, ...}]) => SOME v
	    | (P.PURE_ARITH{oper=P.ORB, kind}, [NUM i, NUM j]) =>
		SOME(NUM{ival = CA.bOr(sizeOfKind kind, #ival i, #ival j), ty = #ty i})
	    | (P.PURE_ARITH{oper=P.XORB, ...}, [NUM{ival=0, ...}, v]) => SOME v
	    | (P.PURE_ARITH{oper=P.XORB, ...}, [v, NUM{ival=0, ...}]) => SOME v
	    | (P.PURE_ARITH{oper=P.XORB, kind}, [NUM i, NUM j]) =>
		SOME(NUM{ival = CA.bXor(sizeOfKind kind, #ival i, #ival j), ty = #ty i})
	    | (P.PURE_ARITH{oper=P.NOTB,kind}, [NUM i]) =>
		SOME(NUM{ival = CA.bNot(sizeOfKind kind, #ival i), ty = #ty i})
	    | (P.LENGTH, [STRING s]) => SOME(tagInt(size s))
	    | (P.INT_TO_REAL{to, ...}, [NUM{ival, ...}]) =>
	      (* NOTE: this conversion might lose precision *)
		SOME(REAL{rval = RealLit.fromInt ival, ty=to})
	    | (P.UNWRAP(P.INT sz), [x as VAR v]) => (case get v
		  of {info=WRPinfo(P.INT sz', u), ...} => if (sz = sz')
		       then SOME u
		       else bug "wrap/unwrap float size conflict"
		   | _ => NONE
		 (* end case *))
	    | (P.UNWRAP(P.FLOAT sz), [x as VAR v]) => (case get v
		  of {info=WRPinfo(P.FLOAT sz', u), ...} => if (sz = sz')
		       then SOME u
		       else bug "wrap/unwrap int size conflict"
		   | _ => NONE
		 (* end case *))
	    | _ => NONE
	  (* end case *))

  (* contraction for branch operations *)
    fun branch (get : get_info) = let
	  fun cond (P.UNBOXED, vl) = notCond(P.BOXED, vl)
	    | cond (P.BOXED, [NUM{ty={tag, ...}, ...}]) = SOME(not tag)
	    | cond (P.BOXED, [STRING s]) = SOME true
	    | cond (P.BOXED, [VAR v]) = (case get v
		 of {info=RECinfo _, ...} => SOME true
		  | {info=WRPinfo _, ...} => SOME true
		  | _ => NONE
		(* end case *))
	    | cond (P.CMP{oper=P.LT, ...}, [VAR v, VAR w]) = if v=w then SOME false else NONE
	    | cond (P.CMP{oper=P.LTE, ...}, [VAR v, VAR w]) = if v=w then SOME true else NONE
	    | cond (P.CMP{oper=P.LT, kind=P.INT _}, [NUM i, NUM j]) = SOME(#ival i < #ival j)
	    | cond (P.CMP{oper=P.LT, kind=P.UINT sz}, [NUM i, NUM j]) =
		SOME(CA.uLess(sz, #ival i, #ival j))
	    | cond (P.CMP{oper=P.LT, kind=P.UINT sz}, [_, NUM{ival=0, ...}]) =
		SOME false (* no unsigned value is < 0 *)
	    | cond (P.CMP{oper=P.LTE, kind=P.INT _}, [NUM i, NUM j]) =
		SOME(#ival i <= #ival j)
	    | cond (P.CMP{oper=P.LTE, kind=P.UINT sz}, [NUM i, NUM j]) =
		SOME(CA.uLessEq(sz, #ival i, #ival j))
	    | cond (P.CMP{oper=P.LTE, kind=P.UINT sz}, [NUM{ival=0, ...}, _]) =
		SOME true (* 0 is <= all unsigned values *)
	    | cond (P.CMP{oper=P.GT, kind}, [w,v]) = cond (P.CMP{oper=P.LT, kind=kind}, [v,w])
	    | cond (P.CMP{oper=P.GTE, kind}, vl) = notCond (P.CMP{oper=P.LT, kind=kind}, vl)
(* TODO: if both arguments are literals, we can optimize this, but we need to be careful
 * about inexact representations.
 *)
	    | cond (P.CMP{oper=P.EQL, kind=P.FLOAT _}, _) = NONE (* in case of NaN's *)
	    | cond (P.CMP{oper=P.EQL, ...}, [VAR v, VAR w]) = if v=w then SOME true else NONE
	    | cond (P.CMP{oper=P.EQL, ...}, [NUM i, NUM j]) = SOME( #ival i = #ival j)
	    | cond (P.CMP{oper=P.NEQ, kind}, vl) = notCond (P.CMP{oper=P.EQL, kind=kind}, vl)
	    | cond (P.PEQL, [NUM i, NUM j]) = SOME(#ival i = #ival j)
	    | cond (P.PNEQ, vl) = notCond(P.PEQL, vl)
	    | cond _ = NONE
	  and notCond arg = Option.map not (cond arg)
	  in
	    cond
	  end

  end
