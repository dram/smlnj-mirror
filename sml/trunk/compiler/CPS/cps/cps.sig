(* cps.sig
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature CPS =
  sig

    datatype record_kind
      = RK_VECTOR
      | RK_RECORD
      | RK_SPILL
      | RK_ESCAPE
      | RK_EXN
      | RK_CONT
      | RK_FCONT
      | RK_KNOWN
      | RK_BLOCK
      | RK_FBLOCK
      | RK_I32BLOCK

    datatype pkind = VPT | RPT of int | FPT of int

  (* type info for integers: size in bits and tagged vs boxed *)
    type intty = {sz : int, tag : bool}

    datatype cty
      = NUMt of intty	(* integers of the given type *)
      | PTRt of pkind	(* pointer *)
      | FUNt		(* function? *)
      | FLTt of int	(* float of given size *)
      | CNTt		(* continuation *)

    structure P : sig

      (* numkind includes kind and size *)
        datatype numkind = INT of int | UINT of int | FLOAT of int

      (* NOTE: this type is defined in the Primop structure (ElabData/prim/primop.sml) *)
	datatype arithop
	  = ADD | SUB | MUL | DIV | MOD | QUOT | REM | FDIV
	  | LSHIFT | RSHIFT | RSHIFTL | ANDB | ORB | XORB
	  | NEG | ABS | NOTB
	  | FSQRT | FSIN | FCOS | FTAN

	datatype cmpop = GT | GTE | LT | LTE | EQL | NEQ

      (* fcmpop conforms to the IEEE std 754 predicates. *)
	datatype fcmpop
	  = F_EQ (* = *)  | F_ULG (* ?<> *) | F_UN (* ? *)   | F_LEG (* <=> *)
	  | F_GT (* > *)  | F_GE  (* >= *)  | F_UGT (* ?> *) | F_UGE (* ?>= *)
	  | F_LT (* < *)  | F_LE  (* <= *)  | F_ULT (* ?< *) | F_ULE (* ?<= *)
	  | F_LG (* <> *) | F_UE  (* ?= *)  | F_SGN

      (* These are two-way branches dependent on pure inputs *)
	datatype branch
	  = CMP of {oper: cmpop, kind: numkind}
	  | FCMP of {oper: fcmpop, size: int}
	  | BOXED | UNBOXED | PEQL | PNEQ
(* FIXME: make length part of string equality test
          | streq of int | strneq of int (* streq n is defined on strings of length n *)
*)
	  | STREQL | STRNEQ
	      (* streq(n,a,b) is defined only if strings a and b have
		 exactly the same length n>1 *)

      (* These all update the store *)
	datatype setter
	  = NUMUPDATE of {kind: numkind}
	  | UNBOXEDUPDATE | UPDATE
	  | UNBOXEDASSIGN | ASSIGN
	  | SETHDLR | SETVAR | SETSPECIAL
	  | RAWSTORE of {kind: numkind}
	  | RAWUPDATE of cty

      (* These fetch from the store, never have functions as arguments. *)
	datatype looker
	  = DEREF | SUBSCRIPT | NUMSUBSCRIPT of {kind: numkind}
	  | GETSPECIAL | GETHDLR | GETVAR
	  | RAWLOAD of {kind: numkind}

      (* These might raise exceptions, never have functions as arguments.*)
	datatype arith
	  = ARITH of {oper: arithop, kind: numkind}
	  | TEST of {from: int, to: int}
	  | TESTU of {from: int, to: int}
	  | TEST_INF of int
	  | ROUND of {floor: bool, from: numkind, to: numkind}

      (* These don't raise exceptions and don't access the store. *)
	datatype pure
	  = PURE_ARITH of {oper: arithop, kind: numkind}
	  | PURE_NUMSUBSCRIPT of {kind: numkind}
	  | LENGTH | OBJLENGTH | MAKEREF
	  | COPY of {from: int, to: int}
	  | EXTEND of {from: int, to: int}
	  | TRUNC of {from: int, to: int}
	  | COPY_INF of int
	  | EXTEND_INF of int
	  | TRUNC_INF of int
	  | REAL of {from: numkind, to: numkind}
	  | SUBSCRIPTV
	  | GETTAG | MKSPECIAL | CAST | GETCON | GETEXN
	  | BOX | UNBOX
	(* tagging/boxing of numbers; numkind should be either `INT` or `FLOAT` *)
	  | WRAP of numkind | UNWRAP of numkind
          | GETSEQDATA | RECSUBSCRIPT | RAW64SUBSCRIPT | NEWARRAY0
	(* allocate uninitialized words from the heap; optionally
	 * initialize the tag.
	 *)
 	  | RAWRECORD of record_kind option

      end (* P *)

    type lvar

    datatype value
      = VAR of lvar
      | LABEL of lvar			(* function labels after closure conversion *)
      | NUM of intty IntConst.t
      | REAL of int RealConst.t
      | STRING of string
      | VOID

    datatype accesspath
      = OFFp of int
      | SELp of int * accesspath

    datatype fun_kind
      = CONT		(* continuation function *)
      | KNOWN		(* known function *)
      | KNOWN_REC	(* known mutually-recursive functions *)
      | KNOWN_CHECK	(* known function with GC check on entry *)
      | KNOWN_TAIL	(* known tail-recursive function *)
      | KNOWN_CONT	(* known continuation function *)
      | ESCAPE		(* before the closure phase, any user function;
			 * after the closure phase, escaping user function
			 *)
      | NO_INLINE_INTO	(* a user function inside of which no in-line expansions
			 * should be performed.  This is used to mark eta-split
			 * wrapper functions so that cpscontract does not inline
			 * the function body into the wrapper.  Removed during
			 * closure conversion.
			 *)

    datatype cexp
      = RECORD of record_kind * (value * accesspath) list * lvar * cexp
      | SELECT of int * value * lvar * cty * cexp
      | OFFSET of int * value * lvar * cexp
      | APP of value * value list
      | FIX of function list * cexp
(* FIXME: SWITCH is currently restricted to tagged integers, should also support boxed ints *)
    (* `SWITCH(v, id, exps)` is a multiway branch on `v`.  The lvar `id` is used as a unique
     * ID in contraction.
     *)
      | SWITCH of value * lvar * cexp list
    (* `BRANCH(br, args, id, trueExp, falseExp)` is a two-way conditional branch.  The lvar
     * `id` is used as a unique ID in contraction.
     *)
      | BRANCH of P.branch * value list * lvar * cexp * cexp
      | SETTER of P.setter * value list * cexp
      | LOOKER of P.looker * value list * lvar * cty * cexp
      | ARITH of P.arith * value list * lvar * cty * cexp
      | PURE of P.pure * value list * lvar * cty * cexp
      (* experimental "raw C call" (Blume, 1/2001) *)
      (* When non-empty, the string contains the linkage info, which
       * is a string of the form:
       *      shared library name/name of the C function.
       *)
      | RCC of rcc_kind * string * CTypes.c_proto * value list *
	       (lvar * cty) list * cexp

    and rcc_kind = FAST_RCC | REENTRANT_RCC

    withtype function = fun_kind * lvar * lvar list * cty list * cexp

  end (* signature CPS *)
