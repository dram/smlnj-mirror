(* cfg.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * CFG IR for SML/NJ code generation.
 *)

structure CFG_Prim =
  struct
    datatype numkind = INT | FLT

  (* rounding modes for float conversions *)
      datatype rounding_mode = TO_NEAREST | TO_NEGINF | TO_POSINF | TO_ZERO

  (* allocation operations *)
    datatype alloc
      = RECORD of {desc : IntInf.int, mut : bool}
      | RAW_RECORD of {desc : IntInf.int, kind : numkind, sz : int}
      | RAW_ALLOC of {desc : IntInf.int option, align : int, len : int}

  (* arithmetic operations that may overflow; for the division operators,
   * we assume that the second argument is never zero (i.e., an explicit
   * test for zero is done before the operation).
   *)
    datatype arithop
      = IADD | ISUB | IMUL
(* Quesion: perhaps the division operators should be moved out of `arith`, since
 * the div-by-zero and overflow tests will have to be explicit?
 *)
      | IDIV | IMOD | IQUOT | IREM

  (* arithmetic operations that do not overflow; for the division operators,
   * we distinguish between signed and unsigned operations, and assume that
   * the second argument is never zero (i.e., an explicit test for zero is
   * done before the operation).
   *)
    datatype pureop
      = ADD | SUB
      | SMUL | SDIV | SREM
      | UMUL | UDIV | UREM
      | LSHIFT | RSHIFT | RSHIFTL
      | ORB | XORB | ANDB
      | FADD | FSUB | FMUL | FDIV
      | FNEG | FABS | FSQRT

    datatype pure
      = PURE_ARITH of {oper : pureop, sz : int}
      | EXTEND of {signed : bool, from : int, to : int}
      | INT_TO_REAL of {from : int, to : int}
      | LOAD_RAW of {offset : int, kind : numkind, sz : int}
      | PURE_SUBSCRIPT
      | PURE_RAW_SUBSCRIPT of {kind : numkind, sz : int}

    datatype arith
      = ARITH of {oper : arithop, sz : int}
      | TEST of {from : int, to : int}
      | TESTU of {from : int, to : int}
      | REAL_TO_INT of {mode : rounding_mode, from : int, to : int}

    datatype looker
      = DEREF
      | SUBSCRIPT
      | RAW_SUBSCRIPT of {kind : numkind, sz : int}
      | GET_HDLR | GET_VAR

    datatype setter
      = UNBOXED_UPDATE | UPDATE				(* array update *)
      | UNBOXED_ASSIGN | ASSIGN				(* reference assignment *)
      | RAW_UPDATE of {kind : numkind, sz : int}	(* raw array update *)
      | RAW_STORE of {kind : numkind, sz : int}		(* raw store to base+offset *)
      | SET_HDLR | SET_VAR

  (* fcmpop conforms to the IEEE std 754 predicates. *)
    datatype fcmpop = datatype CPS.P.fcmpop

  (* comparison operators
   * NOTE: this type is defined in the ArithOps structure (ElabData/prim/arithops.sml)
   *)
    datatype cmpop = datatype CPS.P.cmpop

  (* These are two-way branches dependent on pure inputs *)
    datatype branch
      = CMP of {oper: cmpop, signed: bool, sz : int}
      | FCMP of {oper: fcmpop, sz: int}
      | FSGN of int
      | BOXED | UNBOXED
      | PEQL | PNEQ

  end

structure CFG =
  struct

    datatype ty
      = NUMt of int
      | FLTt of int
      | PTRt
      | FUNt
      | CNTt

    datatype frag_kind
      = GC_CHECK
      | INTERNAL

    datatype calling_conv
      = STD_FUN		(* escaping function *)
      | STD_CONT	(* escaping continuation *)
      | KNOWN_CHK	(* non-escaping function with GC check *)
      | KNOWN		(* non-escaping function *)

    datatype rcc_kind
      = FAST_RCC
      | REENTRANT_RCC

  (* fragment/function parameters *)
    type param = LambdaVar.lvar * ty

  (* branch probabilities are measured in percent (1..99).  We use 0 to
   * represent the absence of probability information.
   *)
    type probability = int

    datatype exp
      = VAR of LambdaVar.lvar
      | LABEL of LambdaVar.lvar
      | NUM of {iv : IntInf.int, signed : bool, sz : int}
      | LOOKER of CFG_Prim.looker * exp list
      | PURE of CFG_Prim.pure * exp list
      | SELECT of int * exp
      | OFFSET of int * exp

    datatype stm
      = LET of exp * param * stm
      | ALLOC of CFG_Prim.alloc * exp list * LambdaVar.lvar * stm
      | APPLY of exp list * ty list
      | THROW of exp list * ty list
      | GOTO of calling_conv * LambdaVar.lvar * exp list * ty list
      | SWITCH of exp * stm list
      | BRANCH of CFG_Prim.branch * exp list * probability * stm * stm
      | ARITH of CFG_Prim.arith * exp list * param * stm
      | SETTER of CFG_Prim.setter * exp list * stm
      | RCC of {
	    reentrant : bool,		(* true for reentrant functions *)
	    linkage : string,		(*  *)
	    proto : CTypes.c_proto,	(* function prototype *)
	    args : exp list,		(* arguments; first arg is function pointer *)
	    results : param list,	(* result bindings *)
	    live : param list,		(* variables that are live across the call;
					 * this list is [] for non-reentrant functions.
					 *)
	    k : stm			(* the continuation *)
	  }

  (* an extended basic block *)
    datatype frag = Frag of frag_kind * LambdaVar.lvar * param list * stm

  (* the entry fragment of a cluster *)
    datatype entry = Entry of calling_conv * LambdaVar.lvar * param list * stm

    type attrs = {
	alignHP : int,		(* alignment requirement in bytes for heap pointer *)
	needsBasePtr : bool,	(* true if cluster does PC-relative addressing *)
	hasRCC : bool		(* true if cluster contains raw C Calls *)
      }

  (* a cluster is a maximal flow graph where every known call is to a
   * fragment in the the cluster.
   *)
    datatype cluster = Cluster of attrs * entry * frag list

  end
