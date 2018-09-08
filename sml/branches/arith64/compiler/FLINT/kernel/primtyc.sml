(* primtyc.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure PrimTyc :> PRIM_TYC =
  struct

    fun bug s = ErrorMsg.impossible ("PrimTyc: " ^ s)
    structure BT = BasicTypes

  (*
   * This datatype defines the set of primitive type constructors. They
   * probably don't have to be defined as a datatype. A environment-like
   * thing would serve better. The intermediate language can be thought
   * as a language parameterized by the set of primitive type constructors
   * and primitive functions --- which can be represented by a higher-order
   * functors. By the way, PT_VOID is an object we know nothing but that
   * it is a pointer; or so-called canonical word representations; on a
   * 32-bit machine, it can be a Pointer or a 31-bit integer; on 64-bit
   * machines, it could be something else. In the future, we should also
   * add arrow_kind and tuple_kind, or even array_kind, and vector_kind to
   * denote various possible representation types. (ZHONG)
   *)

    datatype primtyc
      = PT_NUM of int                   (* integer (0 for IntInf.int) *)
      | PT_REAL of int                  (* reals *)
      | PT_STRING                       (* string type; always a pointer *)
      | PT_EXN                          (* exception type *)
      | PT_ARRAY                        (* the polymorphic array tyc *)
      | PT_VECTOR                       (* the polymorphic vector tyc *)
      | PT_REF                          (* the polymorphic reference tyc *)
      | PT_CONT                         (* the general-continuation tyc *)
      | PT_CCONT                        (* the control-continuation tyc *)
      | PT_ARROW                        (* the function tyc *)
      | PT_OBJ
      | PT_CFUN
      | PT_BARRAY
      | PT_RARRAY
      | PT_SLOCK
    (* internal use only *)
      | PT_ETAG
      | PT_VOID

  (** printing out the primitive type constructor *)
    fun pt_print ptyc = (case ptyc
	   of PT_NUM 0  => "INF"
	    | PT_NUM n  => "I" ^ Int.toString n
	    | PT_REAL n => "F" ^ Int.toString n
	    | PT_STRING => "STR"
	    | PT_EXN    => "EXN"
	    | PT_ARRAY  => "ARR"
	    | PT_VECTOR => "VEC"
	    | PT_REF    => "REF"
	    | PT_CONT   => "CONT"
	    | PT_CCONT  => "CCONT"
	    | PT_ARROW  => "FUN"
	    | PT_OBJ    => "OBJ"
	    | PT_CFUN   => "CFN"
	    | PT_BARRAY => "BARR"
	    | PT_RARRAY => "RARR"
	    | PT_SLOCK  => "SLCK"
	    | PT_ETAG   => "ETG"
	    | PT_VOID   => "VOID"
	  (* end case *))

    val ptc_int    = PT_NUM Target.defaultIntSz
    fun ptc_num n  = PT_NUM n
    val ptc_real   = PT_REAL Target.defaultRealSz
    val ptc_string = PT_STRING
    val ptc_exn    = PT_EXN

    val ptc_array  = PT_ARRAY
    val ptc_vector = PT_VECTOR
    val ptc_ref    = PT_REF

    val ptc_cont   = PT_CONT
    val ptc_ccont  = PT_CCONT
    val ptc_arrow  = PT_ARROW

    val ptc_obj    = PT_OBJ
    val ptc_cfun   = PT_CFUN
    val ptc_barray = PT_BARRAY
    val ptc_rarray = PT_RARRAY
    val ptc_slock  = PT_SLOCK

    val ptc_etag   = PT_ETAG
    val ptc_void   = PT_VOID

  (** get the arity of a particular primitive tycon *)
    fun pt_arity ptyc = (case ptyc
	   of PT_NUM _ =>  0
	    | PT_REAL _ => 0
	    | PT_STRING => 0
	    | PT_EXN =>    0
	    | PT_ARRAY =>  1
	    | PT_VECTOR => 1
	    | PT_REF =>    1
	    | PT_CONT =>   1
	    | PT_CCONT =>  1
	    | PT_ARROW =>  2
	    | PT_OBJ =>    0
	    | PT_CFUN =>   0
	    | PT_BARRAY => 0
	    | PT_RARRAY => 0
	    | PT_SLOCK =>  0
	    | PT_ETAG =>   1
	    | PT_VOID =>   0
	  (* end case *))

    val numBaseCode = 17

  (** each primitive type constructor is equipped with a key *)
    fun pt_toint ptyc = (case ptyc
	   of PT_NUM n => numBaseCode + n
	    | PT_REAL 32 => 0
	    | PT_REAL 64 => 1
	    | PT_STRING => 2
	    | PT_EXN =>    3
	    | PT_ARRAY =>  4
	    | PT_VECTOR => 5
	    | PT_REF =>    6
	    | PT_CONT =>   7
	    | PT_CCONT =>  8
	    | PT_ARROW =>  9
	    | PT_OBJ =>    10
	    | PT_CFUN =>   11
	    | PT_BARRAY => 12
	    | PT_RARRAY => 13
	    | PT_SLOCK =>  14
	    | PT_ETAG =>   15
	    | PT_VOID =>   16	(* must be = numBaseCode-1 *)
	    | _ => bug("bogus ptyc: " ^ pt_print ptyc)
	  (* end case *))

    local
    (* must have numBaseCode elements *)
      val ptycvec = #[
	      PT_REAL 32, PT_REAL 64, PT_STRING, PT_EXN, PT_ARRAY, PT_VECTOR, PT_REF,
	      PT_CONT, PT_CCONT, PT_ARROW, PT_OBJ, PT_CFUN, PT_BARRAY, PT_RARRAY,
	      PT_SLOCK, PT_ETAG, PT_VOID
	    ]
    in
    fun pt_fromint k = if (k < numBaseCode)
	  then Vector.sub (ptycvec, k)
	  else PT_NUM(k - numBaseCode)
    end

    fun pt_eq (ptyc1: primtyc, ptyc2: primtyc) = (ptyc1 = ptyc2)

    fun numPrimTyc 0 = raise Fail "numPrimTyc 0"
      | numPrimTyc n = PT_NUM n
    fun realPrimTyc 32 = PT_REAL 32
      | realPrimTyc 64 = PT_REAL 64

    fun numSize (PT_NUM sz) = SOME sz
      | numSize _ = NONE

    fun realSize (PT_REAL sz) = SOME sz
      | realSize _ = NONE

  (* mapping from Types.tycon to primtycs *)
    val primTycons = [
	    (BT.charTycon, PT_NUM Target.defaultIntSz),
	    (BT.intTycon, PT_NUM Target.defaultIntSz),
	    (BT.wordTycon, PT_NUM Target.defaultIntSz),
	    (BT.word8Tycon, PT_NUM Target.defaultIntSz),
	    (BT.int32Tycon, PT_NUM 32),
	    (BT.word32Tycon, PT_NUM 32),
	    (BT.realTycon, PT_REAL 64),
	    (BT.stringTycon, PT_STRING),
	    (BT.exnTycon, PT_EXN),
	    (BT.arrayTycon, PT_ARRAY),
	    (BT.vectorTycon, PT_VECTOR),
	    (BT.refTycon, PT_REF),
	    (BT.contTycon, PT_CONT),
	    (BT.ccontTycon, PT_CCONT),
	    (BT.arrowTycon, PT_ARROW),
	    (BT.objectTycon, PT_OBJ),
	    (BT.c_functionTycon, PT_CFUN),
	    (BT.word8arrayTycon, PT_BARRAY),
	    (BT.real64arrayTycon, PT_RARRAY),
	    (BT.spin_lockTycon, PT_SLOCK),
	    (BT.intinfTycon, PT_NUM 0)
	  ]

    fun pt_fromtyc tyc = let
	  fun find [] = bug(concat[
		  "pt_fromstamp: primitive tycon ", Symbol.name(TypesUtil.tycName tyc), " not found"
		])
	    | find ((tyc', ptyc)::r) = if TypesUtil.eqTycon(tyc, tyc')
		then ptyc
		else find r
	  in
	    find primTycons
	  end

  (** check the boxity of values of each prim tyc *)
    fun unboxed (PT_NUM n) = (n > Target.defaultIntSz)
      | unboxed (PT_REAL _) = true
      | unboxed _ = false

(* appears to be unused
    fun bxupd (PT_INT31 | PT_INT32 | PT_REAL | PT_VOID) = false
      | bxupd PT_LIST = false
      | bxupd _ = true
*)

    fun ubxupd (PT_NUM n) = (n = Target.defaultIntSz)
      | ubxupd _ = false

    fun isvoid (PT_NUM 0) = true
      | isvoid (PT_NUM _ | PT_REAL _ | PT_STRING) = false
      | isvoid _ = true

  end (* structure PrimTyc *)
