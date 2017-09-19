(* primop.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure Primop : PRIMOP = 
struct

  (* numkind includes kind and number of bits *)
    datatype numkind 
      = INT of int 
      | UINT of int 
      | FLOAT of int
(* QUESTION: what about IntInf.int? *)
 
    datatype arithop
      = ADD | SUB | MUL | DIV | NEG		(* int or float *)
      | ABS | FSQRT | FSIN | FCOS | FTAN	(* floating point only *)
      | LSHIFT | RSHIFT | RSHIFTL		(* int only *)
      | ANDB | ORB | XORB | NOTB		(* int only *)
      | REM | QUOT | MOD		        (* int only *)

    datatype cmpop
      = GT | GTE | LT | LTE			(* signed comparisons *)
      | LEU | LTU | GEU | GTU			(* unsigned comparisons *)
      | EQL | NEQ 				(* equality *)
      | FSGN					(* floating point only *)

  (* datatype primop:
   * Various primitive operations. Those that are designated "inline" (L:) in
   * the comments are expanded into lambda code in terms of other operators,
   * as are the "checked=true" versions of NUMSUBSCRIPT and NUMUPDATE (L?:).
   * "Environmental" primops (occurring in the InLine structure) are indicated
   * by "E:" in the comment.
   *)
    datatype primop
      = ARITH of {				(* E: arithmetic ops *)
	    oper: arithop, overflow: bool, kind: numkind
	  }
      | INLLSHIFT of numkind			(* E: left shift *)
      | INLRSHIFT of numkind			(* E: right shift *)
      | INLRSHIFTL of numkind			(* E: right shift logical *)
      | CMP of {oper: cmpop, kind: numkind}	(* E: generic compare *)
      | TESTU of int * int         		(* E: conversions to int, e.g. testu_31_31 *)
      | TEST of int * int          		(* E: conversions to int, e.g. test_32_31_w *)
      | TRUNC of int * int        		(* E: truncations to smaller int/word, e.g. trunc_32_31_i *)
      | EXTEND of int * int        		(* E: extensions to int32, word32 *)
      | COPY of int * int          		(* E: conversions, e.g. copy_32_32_ii *)
      | TEST_INF of int            		(* E: intinf conversions, e.g. test_inf_31 *)
      | TRUNC_INF of int           		(* E: intinf truncations, e.g. trunc_inf_31 *)
      | EXTEND_INF of int          		(* E: intinf extensions, e.g. extend_8_inf *)
      | COPY_INF of int            		(* E: conversions to intinf, e.g. copy_8_inf *)
      | ROUND of {				(* E: floor, round *)
	    floor: bool, fromkind: numkind, tokind: numkind
	  }
      | REAL of {				(* E: real, real32 *)
	    fromkind: numkind, tokind: numkind
	  }
      | NUMSUBSCRIPT of {			(* E: L?: ordof, etc. *)
	    kind: numkind, checked: bool, immutable: bool
	  }
      | NUMUPDATE of {				(* E: L?: store, etc. *)
	    kind: numkind, checked: bool
	  } 
      | SUBSCRIPT                  		(* E: polymorphic array subscript *)
      | SUBSCRIPTV				(* E: poly vector subscript *)
      | INLSUBSCRIPT				(* E: L: poly array subscript *)
      | INLSUBSCRIPTV				(* E: L: poly vector subscript *)
      | INLMKARRAY				(* E: L: poly array creation *)
      | PTREQL | PTRNEQ				(* E: pointer equality *)
      | POLYEQL | POLYNEQ			(* E: polymorphic equality *)
      | BOXED | UNBOXED				(* E: boxity tests *)
      | LENGTH					(* E: vector, string, array, ... length *)
      | OBJLENGTH				(* E: length of arbitrary heap object *)
      | CAST					(* E: cast *)
      | GETHDLR | SETHDLR			(* E: get/set exn handler pointer *)
      | GETVAR | SETVAR				(* E: get/set var register *)
      | GETPSEUDO | SETPSEUDO			(* E: get/set pseudo registers *)
      | SETMARK | DISPOSE			(* E: capture/dispose frames *)
      | MAKEREF					(* E: allocate a ref cell *)
      | CALLCC | CAPTURE | THROW		(* E: continuation operations *)
      | ISOLATE					(* E: isolating a function *)
      | DEREF					(* E: dereferencing *)
      | ASSIGN					(* E: assignment *)
      | UPDATE					(* E: array or reference update (maybe boxed) *)
      | INLUPDATE				(* E: L: array update (maybe boxed) *)
      | UNBOXEDUPDATE				(* E: update array of integers WITH tags
						 * removed by Zhong, put back by Matthias
						 * (see FLINT/trans/primopmap.sml) *)
      | GETTAG					(* E: extract the tag portion of an
						 * object's descriptor as an ML int *)
      | MKSPECIAL				(* E: make a special object *)
      | SETSPECIAL				(* E: set the state of a special object *)
      | GETSPECIAL				(* E: get the state of a special object *)
      | INLMIN of numkind			(* E: L: min *)
      | INLMAX of numkind			(* E: L: max *)
      | INLABS of numkind			(* E: L: abs *)
      | INLNOT					(* E: L: bool not operator *)
      | INLCOMPOSE				(* E: L: compose "op o"  operator *)
      | INLBEFORE				(* E: L: "before" operator *) 
      | INLIGNORE				(* E: L: "ignore" function *)
    (* primops to support new array representations *)
      | NEW_ARRAY0				(* E: allocate zero-length array header *)
      | GET_SEQ_DATA				(* E: get data pointer from arr/vec header *)
      | SUBSCRIPT_REC				(* E: record subscript operation *)
      | SUBSCRIPT_RAW64				(* E: raw64 subscript operation *)
      | INLIDENTITY				(* E: polymorphic identity *)
      | CVT64					(* E: convert between external and
						 * internal representation of compiler
						 * simulated 64-bit scalars, e.g. w64p *)
    (* Primops to support C FFI. *)
      | RAW_LOAD of numkind			(* E: load from arbitrary memory location *)
      | RAW_STORE of numkind			(* E: store to arbitrary memory location *)
    (* E: make a call to a C-function;
     * The primop carries C function prototype information and specifies
     * which of its (ML-) arguments are floating point. C prototype
     * information is for use by the backend, ML information is for
     * use by the CPS converter. *)
      | RAW_CCALL of {
	    c_proto: CTypes.c_proto,
	    ml_args: ccall_type list,
	    ml_res_opt: ccall_type option,
	    reentrant: bool
	  } option
   (* Allocate uninitialized storage on the heap.
    * The record is meant to hold short-lived C objects, i.e., they
    * are not ML pointers.  The representation is 
    * the same as RECORD with tag tag_raw32 or tag_fblock.
    *)
      | RAW_RECORD of { fblock: bool }  (* E: *)

    (* non-environmental primops (not found in InLine) *)
      | UNBOXEDASSIGN			(* assignment to integer reference *)

      | WCAST				(* ? *)
      | MARKEXN				(* mark an exception value with a string *)

      | INL_ARRAY			(* L: polymorphic array allocation *)
      | INL_VECTOR			(* L: polymorphic vector allocation *)
      | INL_MONOARRAY of numkind	(* L: monomorphic array allocation *)
      | INL_MONOVECTOR of numkind	(* L: monomorphic vector allocation *)

      | MKETAG				(* make a new exception tag *)
      | WRAP				(* box a value by wrapping it *)
      | UNWRAP				(* unbox a value by unwrapping it *)

    and ccall_type =
	CCI32 |				(* passed as int32 *)
	CCI64 |				(* int64, currently unused *)
	CCR64 |				(* passed as real64 *)
	CCML				(* passed as Unsafe.Object.object *)

  (** default integer arithmetic and comparison operators *)
    val IADD = ARITH{oper=ADD, overflow=true, kind=INT 31}
    val ISUB = ARITH{oper=SUB, overflow=true, kind=INT 31}
    val IMUL = ARITH{oper=MUL, overflow=true, kind=INT 31}
    val IDIV = ARITH{oper=DIV, overflow=true, kind=INT 31}
    val INEG = ARITH{oper=NEG, overflow=true, kind=INT 31}

    val IEQL = CMP{oper=EQL, kind=INT 31}
    val INEQ = CMP{oper=NEQ, kind=INT 31}
    val IGT = CMP{oper=GT, kind=INT 31}
    val ILT = CMP{oper=LT, kind=INT 31}
    val IGE = CMP{oper=GTE, kind=INT 31}
    val ILE = CMP{oper=LTE, kind=INT 31}

  (** default floating-point equality operator *)
    val FEQLd = CMP{oper=EQL, kind=FLOAT 64}

(**************************************************************************
 *               OTHER PRIMOP-RELATED UTILITY FUNCTIONS                   *
 **************************************************************************)

    fun prNumkind (INT bits) = "i" ^ Int.toString bits
      | prNumkind (UINT bits) = "u" ^ Int.toString bits
      | prNumkind (FLOAT bits) = "f" ^ Int.toString bits

    val cvtParam = Int.toString
    fun cvtParams (from, to) = concat [cvtParam from, "_", cvtParam to]

    fun prPrimop (ARITH{oper, overflow, kind}) = let
	  val rator = (case oper
		 of ADD => "add" | SUB => "sub" | MUL => "mul" | DIV => "div" | NEG => "neg"
		  | FSQRT => "fsqrt"
		  | FSIN => "fsin" | FCOS => "fcos" | FTAN => "ftan"
		  | LSHIFT => "lshift" | RSHIFT => "rshift" | RSHIFTL => "rshift_l"
		  | ANDB => "andb" | ORB => "orb" | XORB => "xorb" | NOTB => "notb"
		  | ABS => "abs" | REM => "rem" | QUOT => "quot" | MOD => "mod"
		(* end case *))
	  in
	    concat [ rator, if overflow then "_" else "n_", prNumkind kind]
	  end
      | prPrimop (INLLSHIFT kind) =  "inllshift_"  ^ prNumkind kind
      | prPrimop (INLRSHIFT kind) =  "inlrshift_"  ^ prNumkind kind
      | prPrimop (INLRSHIFTL kind) = "inlrshiftl_" ^ prNumkind kind
      | prPrimop (CMP{oper,kind}) = let
	  val rator = (case oper
		 of GT => ">_" | LT => "<_" | GTE => "<=_" | LTE => "<=_"
		  | GEU => ">=U_" | GTU => ">U_" | LEU => "<=U_" | LTU => "<U_"
		  | EQL => "=_" | NEQ => "<>_" | FSGN => "fsgn_"
		(* end case *))
	  in
	    rator ^ prNumkind kind
	  end
      | prPrimop (TEST arg) = "test_" ^ cvtParams arg
      | prPrimop (TESTU arg) = "test_" ^ cvtParams arg
      | prPrimop (EXTEND arg) = "extend" ^ cvtParams arg
      | prPrimop (TRUNC arg) = "trunc" ^ cvtParams arg
      | prPrimop (COPY arg) = "copy" ^ cvtParams arg
      | prPrimop (TEST_INF i) = "test_inf_" ^ cvtParam i
      | prPrimop (TRUNC_INF i) = "trunc_inf_" ^ cvtParam i
      | prPrimop (EXTEND_INF i) = concat ["extend_", cvtParam i, "_inf"]
      | prPrimop (COPY_INF i) =  concat ["copy_", cvtParam i, "_inf"]
      | prPrimop (ROUND{floor,fromkind,tokind}) = concat [
	    if floor then "floor_" else "round_",
	    prNumkind fromkind, "_", prNumkind tokind
	  ]
      | prPrimop(REAL{fromkind=INT 31,tokind=FLOAT 64}) = "real"
      | prPrimop(REAL{fromkind,tokind}) = concat [
	    "real_", prNumkind fromkind, "_", prNumkind tokind
	  ]
      | prPrimop(NUMSUBSCRIPT{kind,checked,immutable}) = concat [
	    "numsubscript_", prNumkind kind,
	    if checked then "c" else "",
	    if immutable then "v" else ""
	  ]
      | prPrimop (NUMUPDATE{kind,checked}) = concat [
	    "numupdate", prNumkind kind, if checked then  "c" else ""
	  ]
      | prPrimop DEREF = "!"
      | prPrimop ASSIGN = ":="
      | prPrimop UNBOXEDASSIGN = "(unboxed):="
      | prPrimop BOXED = "boxed"
      | prPrimop UNBOXED = "unboxed"
      | prPrimop CAST = "cast"
      | prPrimop WCAST = "wcast"
      | prPrimop PTREQL = "ptreql"
      | prPrimop PTRNEQ = "ptrneq"  
      | prPrimop POLYEQL = "polyeql"
      | prPrimop POLYNEQ = "polyneq"  
      | prPrimop GETHDLR = "gethdlr"
      | prPrimop MAKEREF = "makeref"
      | prPrimop SETHDLR = "sethdlr"
      | prPrimop LENGTH = "length"
      | prPrimop OBJLENGTH = "objlength"
      | prPrimop CALLCC = "callcc"
      | prPrimop CAPTURE = "capture"
      | prPrimop ISOLATE = "isolate"
      | prPrimop THROW = "throw"
      | prPrimop SUBSCRIPT = "subscript"
      | prPrimop UNBOXEDUPDATE = "unboxedupdate"
      | prPrimop UPDATE = "update"
      | prPrimop INLSUBSCRIPT = "inlsubscript"
      | prPrimop INLSUBSCRIPTV = "inlsubscriptv"
      | prPrimop INLUPDATE = "inlupdate"
      | prPrimop INLMKARRAY = "inlmkarray"
      | prPrimop SUBSCRIPTV = "subscriptv"
      | prPrimop GETVAR = "getvar"
      | prPrimop SETVAR = "setvar"
      | prPrimop GETPSEUDO = "getpseudo"
      | prPrimop SETPSEUDO = "setpseudo"
      | prPrimop SETMARK = "setmark"
      | prPrimop DISPOSE = "dispose"
      | prPrimop GETTAG = "gettag"
      | prPrimop MKSPECIAL = "mkspecial"
      | prPrimop SETSPECIAL = "setspecial"
      | prPrimop GETSPECIAL = "getspecial"
      | prPrimop (INLMIN nk) = concat ["inlmin_", prNumkind nk]
      | prPrimop (INLMAX nk) = concat ["inlmax_", prNumkind nk]
      | prPrimop (INLABS nk) = concat ["inlabs_", prNumkind nk]
      | prPrimop INLNOT = "inlnot"
      | prPrimop INLCOMPOSE = "inlcompose"
      | prPrimop INLBEFORE = "inlbefore"
      | prPrimop INLIGNORE = "inlignore"
      | prPrimop (INL_ARRAY) = "inl_array"
      | prPrimop (INL_VECTOR) = "inl_vector"
      | prPrimop (INL_MONOARRAY kind) = concat ["inl_monoarray_", prNumkind kind]
      | prPrimop (INL_MONOVECTOR kind) = concat ["inl_monovector_", prNumkind kind]
      | prPrimop (MARKEXN) = "markexn"
      | prPrimop (MKETAG) = "mketag"
      | prPrimop (WRAP) = "wrap"
      | prPrimop (UNWRAP) = "unwrap"
    (* Primops to support new array representations *)
      | prPrimop (NEW_ARRAY0) = "newarray0"
      | prPrimop (GET_SEQ_DATA) = "getseqdata"
      | prPrimop (SUBSCRIPT_REC) = "subscriptrec"
      | prPrimop (SUBSCRIPT_RAW64) = "subscriptraw64"
    (* Primops to support new experimental C FFI. *)
      | prPrimop (RAW_LOAD nk) = concat ["raw_load(", prNumkind nk, ")"]
      | prPrimop (RAW_STORE nk) = concat ["raw_store(", prNumkind nk, ")"]
      | prPrimop (RAW_CCALL _) = "raw_ccall"
      | prPrimop (RAW_RECORD { fblock }) = 
	  concat ["raw_", if fblock then "fblock" else "iblock", "_record"]
      | prPrimop INLIDENTITY = "inlidentity"
      | prPrimop CVT64 = "cvt64"

  (* should return more than just a boolean:
   * {Store,Continuation}-{read,write}
   *)
    fun effect p = (case p
	   of ARITH{overflow,...} => overflow
	    | (INLRSHIFT _ | INLRSHIFTL _) => false
	    | CMP _ => false
	    | (EXTEND _ | TRUNC _ | COPY _) => false
	    | (PTREQL | PTRNEQ | POLYEQL | POLYNEQ) => false
	    | (BOXED | UNBOXED) => false
	    | (LENGTH | OBJLENGTH) => false
	    | (CAST | WCAST) => false
	    | (INLMIN _ | INLMAX _ | INLNOT | INLCOMPOSE | INLIGNORE) => false
	    | (WRAP | UNWRAP) => false
	    | INLIDENTITY => false
	    | CVT64 => false
	    | _ => true
	  (* end case *))

    fun mayRaise p = (case p
	    of ARITH{overflow,...} => overflow
	     | ROUND _ => true
	     | INLMKARRAY => true
	     | INLSUBSCRIPT => true
	     | INLUPDATE => true
	     | INLSUBSCRIPTV => true
	     | NUMSUBSCRIPT{checked,...} => checked
	     | NUMUPDATE{checked,...} => checked
	     | _ => false
	  (* end case *))

  end  (* structure PrimOp *)
