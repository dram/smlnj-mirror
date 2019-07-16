(* Copyright 1996 by AT&T Bell Laboratories *)
(* primop.sml *)

structure PrimOp : PRIM_OP = 
struct

structure B = BasicTypes 
structure T = Types

(* numkind includes kind and number of bits *)
datatype numkind 
  = INT of int 
  | UINT of int 
  | FLOAT of int
 
datatype arithop
  = + | - | * | / | ~		(* int or float *)
  | ABS				(* floating point only *)
  | LSHIFT | RSHIFT | RSHIFTL	(* int only *)
  | ANDB | ORB | XORB | NOTB	(* int only *)

datatype cmpop = > | >= | < | <= | LEU | LTU | GEU | GTU | EQL | NEQ

(* 
 * Various primitive operations.  Those that are designated "inline" are
 * expanded into lambda code in terms of other operators,
 * as is the "checked=true" version of NUMSUBSCRIPT or NUMUPDATE.
 *)

datatype primop
  = ARITH of {oper: arithop, overflow: bool, kind: numkind}
  | INLLSHIFT of numkind
  | INLRSHIFT of numkind
  | INLRSHIFTL of numkind
  | CMP of {oper: cmpop, kind: numkind}

  | TESTU of int * int		
  | TEST of int * int
  | TRUNC of int * int
  | EXTEND of int * int
  | COPY of int * int 

  | ROUND of {floor: bool, fromkind: numkind, tokind: numkind}
  | REAL of {fromkind: numkind, tokind: numkind}

  | NUMSUBSCRIPT of {kind: numkind, checked: bool, immutable: bool}
  | NUMUPDATE of {kind: numkind, checked: bool}

  | SUBSCRIPT                  (* polymorphic array subscript *)
  | SUBSCRIPTV                 (* poly vector subscript *)
  | INLSUBSCRIPT               (* inline poly array subscript *)
  | INLSUBSCRIPTV              (* inline poly vector subscript *)
  | INLMKARRAY                 (* inline poly array creation *)

  | PTREQL | PTRNEQ            (* pointer equality *)
  | POLYEQL | POLYNEQ          (* polymorphic equality *)
  | BOXED | UNBOXED            (* boxity tests *)
  | LENGTH                     (* vector, string, array, ... length *)
  | OBJLENGTH                  (* length of arbitrary heap object *)
  | CAST
  | GETRUNVEC                  (* get the pointer to the run-vector *)
  | MARKEXN                    (* mark an exception value with a string *)
  | GETHDLR | SETHDLR          (* get/set exn handler pointer *)
  | GETVAR | SETVAR            (* get/set var register *)
  | GETPSEUDO | SETPSEUDO      (* get/set pseudo registers *)
  | SETMARK | DISPOSE          (* capture/dispose frames *)
  | MAKEREF                    (* allocate a ref cell *)
  | CALLCC | CAPTURE | THROW   (* continuation operations *)
  | ISOLATE                    (* isolating a function *)
  | DEREF                      (* dereferencing *)
  | ASSIGN                     (* assignment; shorthand for update(a, 0, v) *)
  | UPDATE                     (* array or reference update (maybe boxed) *)
  | INLUPDATE                  (* inline array update (maybe boxed) *)
  | BOXEDUPDATE                (* boxed array update *)
  | UNBOXEDUPDATE              (* update array of integers WITH tags *)

  | GETTAG                     (* extract the tag portion of an *)
                               (* object's descriptor as an ML int *)
  | MKSPECIAL                  (* make a special object *)
  | SETSPECIAL                 (* set the state of a special object *)
  | GETSPECIAL                 (* get the state of a special object *)
  | USELVAR | DEFLVAR
  | INLDIV | INLMOD | INLREM   (* inline interger arithmetic *)
  | INLMIN |INLMAX | INLABS    (* inline interger arithmetic *) 
  | INLNOT                     (* inline bool not operator *)
  | INLCOMPOSE                 (* inline compose "op o"  operator *)
  | INLBEFORE                  (* inline "before" operator *) 
  | INL_ARRAY                  (* inline polymorphic array allocation *)
  | INL_VECTOR                 (* inline polymorphic vector allocation *)
  | INL_MONOARRAY of numkind   (* inline monomorphic array allocation *)
  | INL_MONOVECTOR of numkind  (* inline monomorphic vector allocation *)

(** default integer arithmetic and comparison operators *)
val IADD = ARITH{oper=op +, overflow=true, kind=INT 31}
val ISUB = ARITH{oper=op -, overflow=true, kind=INT 31}
val IMUL = ARITH{oper=op *, overflow=true, kind=INT 31}
val IDIV = ARITH{oper=op /, overflow=true, kind=INT 31}
val INEG = ARITH{oper=op ~, overflow=true, kind=INT 31}

val IEQL = CMP{oper=EQL, kind=INT 31}
val INEQ = CMP{oper=NEQ, kind=INT 31}
val IGT = CMP{oper=op >, kind=INT 31}
val ILT = CMP{oper=op <, kind=INT 31}
val IGE = CMP{oper=op >=, kind=INT 31}
val ILE = CMP{oper=op <=, kind=INT 31}

(** default floating-point equality operator *)
val FEQLd = CMP{oper=EQL, kind=FLOAT 64}



(**************************************************************************
 *                 BUILDING A COMPLETE LIST OF PRIMOPS                    *
 **************************************************************************)

local

fun bits size oper = ARITH{oper=oper, overflow=false, kind=INT size}
val bits31 = bits 31		
val bits32 = bits 32		

fun int size oper = ARITH{oper=oper, overflow=true, kind=INT size}
val int31 = int 31
val int32 = int 32

fun word size oper = ARITH{oper=oper, overflow=false, kind=UINT size}
val word32 = word 32
val word31 = word 31
val word8  = word 8

fun float size oper = ARITH{oper=oper, overflow=true, kind=FLOAT size}
val float64 = float 64

fun purefloat size oper = ARITH{oper=oper,overflow=false,kind=FLOAT size}
val purefloat64 = purefloat 64	

fun cmp kind oper = CMP{oper=oper, kind=kind}
val int31cmp = cmp (INT 31)
val int32cmp = cmp (INT 32)

val word32cmp = cmp (UINT 32)
val word31cmp = cmp (UINT 31)
val word8cmp  = cmp (UINT 8)

val float64cmp = cmp (FLOAT 64)

val v1 = T.IBOUND 0
val v2 = T.IBOUND 1
val v3 = T.IBOUND 2

fun pa(t1,t2) =  B.tupleTy [t1,t2]
fun tp(t1,t2,t3) =  B.tupleTy [t1,t2,t3]
fun ar(t1,t2) = B.--> (t1, t2)

fun ap(tc,l) = T.CONty(tc, l)
fun cnt t = T.CONty(B.contTycon,[t])
fun ccnt t = T.CONty(B.ccontTycon,[t])
fun rf t = T.CONty(B.refTycon,[t])
fun ay t = T.CONty(B.arrayTycon,[t])
fun vct t = T.CONty(B.vectorTycon,[t])

val bo = B.boolTy
val i = B.intTy
val u = B.unitTy

fun p0 t = SOME t
fun p1 t = SOME(T.POLYty {sign=[false], tyfun=T.TYFUN {arity=1, body=t}})
fun ep1 t = SOME(T.POLYty {sign=[true], tyfun=T.TYFUN {arity=1, body=t}})
fun p2 t = SOME(T.POLYty {sign=[false,false], 
                          tyfun=T.TYFUN {arity=2, body=t}})
fun p3 t = SOME(T.POLYty {sign=[false,false,false], 
                          tyfun=T.TYFUN {arity=3, body=t}})

fun sub kind = NUMSUBSCRIPT{kind=kind, checked=false, immutable=false}
fun chkSub kind = NUMSUBSCRIPT{kind=kind, checked=true, immutable=false}

fun subv kind = NUMSUBSCRIPT{kind=kind, checked=false, immutable=true}
fun chkSubv kind = NUMSUBSCRIPT{kind=kind, checked=true, immutable=true}

fun update kind = NUMUPDATE {kind=kind, checked=false}
fun chkUpdate kind = NUMUPDATE {kind=kind, checked=true}

val numSubTy = p2(ar(pa(v1,i),v2))
val numUpdTy = p2(ar(tp(v1,i,v2),u))

in

val allPrimops =
      [("callcc",	 CALLCC,      	p1(ar(ar(cnt(v1),v1),v1))),
       ("throw",	 THROW,      	p2(ar(cnt(v1),ar(v1,v2)))),
       ("capture",	 CAPTURE,      	p1(ar(ar(ccnt(v1),v1),v1))),
       ("isolate",	 ISOLATE,      	p1(ar(ar(v1,u),cnt(v1)))),
       ("cthrow",	 THROW,      	p2(ar(ccnt(v1),ar(v1,v2)))),
       ("!",		 DEREF,      	p1(ar(rf(v1),v1))),
       (":=",	         ASSIGN,      	p1(ar(pa(rf(v1),v1),u))),
       ("makeref",	 MAKEREF,      	p1(ar(v1,rf(v1)))),
       ("boxed",	 BOXED,      	p1(ar(v1,bo))),
       ("unboxed",	 UNBOXED,      	p1(ar(v1,bo))),
       ("cast",	         CAST,      	p2(ar(v1,v2))),
       ("=",		 POLYEQL,      	ep1(ar(pa(v1,v1),bo))),
       ("<>",	         POLYNEQ,      	ep1(ar(pa(v1,v1),bo))),
       ("ptreql",	 PTREQL,      	p1(ar(pa(v1,v1),bo))),
       ("ptrneq",	 PTRNEQ,      	p1(ar(pa(v1,v1),bo))),
       ("getvar",	 GETVAR,      	p1(ar(u,v1))),
       ("setvar",	 SETVAR,      	p1(ar(v1,u))),
       ("setpseudo",	 SETPSEUDO,     p1(ar(pa(v1,i),u))),
       ("getpseudo",	 GETPSEUDO,     p1(ar(i,v1))),
       ("mkspecial",     MKSPECIAL,     p2(ar(pa(i,v1),v2))),
       ("getspecial",    GETSPECIAL,    p1(ar(v1,i))),
       ("setspecial",    SETSPECIAL,    p1(ar(pa(v1,i),u))),
       ("gethdlr",	 GETHDLR,      	p1(ar(u,cnt(v1)))),
       ("sethdlr",	 SETHDLR,      	p1(ar(cnt(v1),u))),
       ("gettag", 	 GETTAG,      	p1(ar(v1,i))),
       ("setmark",	 SETMARK,      	p1(ar(v1,u))),
       ("dispose",	 DISPOSE,      	p1(ar(v1,u))),
       ("compose",	 INLCOMPOSE,    p3(ar(pa(ar(v2,v3),ar(v1,v2)),
                                              ar(v1,v3)))),
       ("before",	 INLBEFORE,     p2(ar(pa(v1,v2),v1))),
			 
       			 
       ("length",	 LENGTH,      	p1(ar(v1,i))),
       ("objlength",	 OBJLENGTH,     p1(ar(v1,i))),

       (*  
        * I believe the following five primops should not be exported into
        * the InLine structure. (ZHONG) 
        *)
       ("boxedupdate",   BOXEDUPDATE,   NONE),
       ("unboxedupdate", UNBOXEDUPDATE, NONE),
       ("getrunvec",	 GETRUNVEC,     NONE),
       ("uselvar",	 USELVAR,      	NONE),
       ("deflvar",	 DEFLVAR,      	NONE),
       			 
       ("inlnot",	 INLNOT,      	NONE),
       ("floor",         ROUND{floor=true,
                               fromkind=FLOAT 64,
                               tokind=INT 31},      	NONE),
       ("round",         ROUND{floor=false, 
                               fromkind=FLOAT 64,
                               tokind=INT 31},      	NONE),
       ("real",          REAL{fromkind=INT 31,
                              tokind=FLOAT 64},      	NONE),
       			 
       ("ordof",         NUMSUBSCRIPT{kind=INT 8,
                                      checked=false,
                                      immutable=true},  numSubTy),
       ("store",         NUMUPDATE{kind=INT 8,
                                   checked=false},      numUpdTy),
       ("inlbyteof",     NUMSUBSCRIPT{kind=INT 8,
                                      checked=true,
                                      immutable=false}, numSubTy),
       ("inlstore",      NUMUPDATE{kind=INT 8,
                                   checked=true},      	numUpdTy),
       ("inlordof",      NUMSUBSCRIPT{kind=INT 8,
                                      checked=true,
                                      immutable=true},  numSubTy),

       (*** polymorphic array and vector ***)
       ("mkarray",       INLMKARRAY,            p1(ar(pa(i,v1),ay(v1)))),
       ("arrSub", 	 SUBSCRIPT,      	p1(ar(pa(ay(v1),i),v1))),
       ("arrChkSub",	 INLSUBSCRIPT,      	p1(ar(pa(ay(v1),i),v1))),
       ("vecSub",	 SUBSCRIPTV,      	p1(ar(pa(vct(v1),i),v1))),
       ("vecChkSub",	 INLSUBSCRIPTV,      	p1(ar(pa(vct(v1),i),v1))),
       ("arrUpdate",	 UPDATE,      	        p1(ar(tp(ay(v1),i,v1),u))),
       ("arrChkUpdate",  INLUPDATE,      	p1(ar(tp(ay(v1),i,v1),u))),

       (* conversion primops *)
       ("test_32_31",    TEST(32,31),  	        NONE),
       ("testu_31_31",   TESTU(31,31),          NONE),
       ("testu_32_31",   TESTU(32,31),          NONE),
       ("testu_32_32",   TESTU(32,32),   	NONE),
       ("copy_32_32",    COPY(32,32),   	NONE),
       ("copy_31_31",    COPY(31,31),   	NONE),
       ("copy_31_32",    COPY(31,32),   	NONE),
       ("copy_8_32",     COPY(8,32),     	NONE),
       ("copy_8_31",     COPY(8,31),     	NONE),
       ("extend_31_32",  EXTEND(31,32), 	NONE),
       ("extend_8_31",   EXTEND(8,31),   	NONE),
       ("extend_8_32",   EXTEND(8,32), 	        NONE),
       ("trunc_32_31",   TRUNC(32,31),   	NONE),
       ("trunc_31_8",    TRUNC(31,8),   	NONE),
       ("trunc_32_8",    TRUNC(32,8),   	NONE),
       
       (*** integer 31 primops ***)
       ("i31mul",	 int31 (op * ),      	NONE),
       ("i31quot",	 int31 (op /),      	NONE),
       ("i31add", 	 int31 (op +),      	NONE),
       ("i31sub",	 int31 (op -),      	NONE),
       ("i31orb",	 bits31 ORB,      	NONE),
       ("i31andb",	 bits31 ANDB,      	NONE),
       ("i31xorb",	 bits31 XORB,      	NONE),
       ("i31notb",	 bits31 NOTB,      	NONE),
       ("i31neg",	 int31 ~,      	        NONE),
       ("i31lshift",	 bits31 LSHIFT,      	NONE),
       ("i31rshift",	 bits31 RSHIFT,      	NONE),
       ("i31lt",	 int31cmp (op <),      	NONE),
       ("i31le",	 int31cmp (op <=),      NONE),
       ("i31gt",	 int31cmp (op >),      	NONE),
       ("i31ge", 	 int31cmp (op >=),      NONE),
       ("i31ltu",	 word31cmp LTU,      	NONE), 
       ("i31geu",	 word31cmp GEU,      	NONE),
       ("i31mod",        INLMOD,      	        NONE),
       ("i31div",	 INLDIV,      	        NONE),
       ("i31rem",	 INLREM,      	        NONE),
       ("i31max",	 INLMAX,      	        NONE),
       ("i31min",	 INLMIN,      	        NONE),
       ("i31abs",	 INLABS,      	        NONE),
       ("i31eq",	 int31cmp EQL,      	NONE),
       ("i31ne",	 int31cmp NEQ,      	NONE),

       (*** integer 32 primops ***)
       ("i32mul",        int32 (op * ),      	NONE),
       ("i32quot",       int32 (op /),      	NONE),
       ("i32add",        int32 (op +),      	NONE),
       ("i32sub",        int32 (op -),      	NONE),
       ("i32orb",        bits32 ORB,      	NONE),
       ("i32andb",       bits32 ANDB,      	NONE),
       ("i32xorb",       bits32 XORB,      	NONE),
       ("i32lshift",     bits32 LSHIFT,      	NONE),
       ("i32rshift",     bits32 RSHIFT,      	NONE),
       ("i32neg",        int32 ~,      	        NONE),
       ("i32lt",         int32cmp (op <),      	NONE),
       ("i32le",         int32cmp (op <=),      NONE),
       ("i32gt",         int32cmp (op >),      	NONE),
       ("i32ge",         int32cmp (op >=),      NONE),
       ("i32eq",         int32cmp (op EQL),     NONE),
       ("i32ne",         int32cmp (op NEQ),     NONE),

       (*
        * WARNING: the lambda types in translate.sml are all wrong for  
        * this. The inlprimops for these must be parameterized over the 
        * integer kind. 
        *
        *
        * ("i32mod",    INLMOD,      	NONE),
        * ("i32div",    INLDIV,      	NONE),
        * ("i32rem",    INLREM,      	NONE),
        * ("i32max",    INLMAX,      	NONE),
        * ("i32min",    INLMIN,      	NONE),
        * ("i32abs",    INLABS,      	NONE),
        *
        *)

       (*** float 64 primops ***)
       ("f64add", 	 float64 (op +),      	NONE),
       ("f64sub",	 float64 (op -),      	NONE),
       ("f64div", 	 float64 (op /),      	NONE),
       ("f64mul",	 float64 (op * ),      	NONE),
       ("f64neg",	 purefloat64 ~,      	NONE),
       ("f64ge",	 float64cmp (op >=),    NONE),
       ("f64gt",	 float64cmp (op >),     NONE),
       ("f64le",	 float64cmp (op <=),    NONE),
       ("f64lt",	 float64cmp (op <),     NONE),
       ("f64eq",	 float64cmp EQL,      	NONE),
       ("f64ne",	 float64cmp NEQ,      	NONE),
       ("f64abs",	 purefloat64 ABS,      	NONE),

       (*** float64 array ***)	
       ("f64Sub",	 sub (FLOAT 64),      	numSubTy),
       ("f64chkSub",	 chkSub (FLOAT 64),     numSubTy),
       ("f64Update",	 update (FLOAT 64),     numUpdTy),
       ("f64chkUpdate",  chkUpdate (FLOAT 64),  numUpdTy),

       (*** word8 primops ***)
       (* 
        * In the long run, we plan to represent WRAPPED word8 tagged, and 
        * UNWRAPPED untagged. But right now, we represent both of them 
        * tagged, with 23 high-order zero bits and 1 low-order 1 bit.
        * In this representation, we can use the comparison and (some of 
        * the) bitwise operators of word31; but we cannot use the shift 
        * and arithmetic operators.
        *
        * WARNING: THIS IS A TEMPORARY HACKJOB until all the word8 primops 
        * are correctly implemented.
        *
        * ("w8mul",	word8 (op * ),      	NONE),
	* ("w8div",	word8 (op /),      	NONE),
	* ("w8add",	word8 (op +),      	NONE),
	* ("w8sub",	word8 (op -),      	NONE),
        *
        * ("w8notb",	word31 NOTB,      	NONE),
	* ("w8rshift",	word8 RSHIFT,      	NONE),
	* ("w8rshiftl",	word8 RSHIFTL,      	NONE),
	* ("w8lshift",	word8 LSHIFT,      	NONE),
        *
	* ("w8toint",   ROUND{floor=true, 
        *                     fromkind=UINT 8, 
        *                     tokind=INT 31},   NONE),
	* ("w8fromint", REAL{fromkind=INT 31,
        *                    tokind=UINT 8},    NONE),
        *)
  
       ("w8orb",	 word31 ORB,      	NONE),
       ("w8xorb",	 word31 XORB,      	NONE),
       ("w8andb",	 word31 ANDB,      	NONE),
       	 
       ("w8gt",	         word8cmp (op >),      	NONE),
       ("w8ge",	         word8cmp (op >=),      NONE),
       ("w8lt",	         word8cmp (op <),      	NONE),
       ("w8le",		 word8cmp (op <=),      NONE),
       ("w8eq",		 word8cmp EQL,      	NONE),
       ("w8ne",		 word8cmp NEQ,      	NONE),

       (*** word8 array and vector ***)
       ("w8Sub",	 sub (UINT 8),      	numSubTy),
       ("w8chkSub",	 chkSub (UINT 8),      	numSubTy),
       ("w8subv",	 subv (UINT 8),      	numSubTy),
       ("w8chkSubv",	 chkSubv (UINT 8),      numSubTy),
       ("w8update",	 update (UINT 8),      	numUpdTy),
       ("w8chkUpdate",   chkUpdate (UINT 8),    numUpdTy),

       (* word31 primops *)
       ("w31mul",	 word31 (op * ),      	NONE),
       ("w31div",	 word31 (op /),      	NONE),
       ("w31add",	 word31 (op +),      	NONE),
       ("w31sub",	 word31 (op -),      	NONE),
       ("w31orb",	 word31 ORB,      	NONE),
       ("w31xorb",	 word31 XORB,      	NONE),
       ("w31andb",	 word31 ANDB,      	NONE),
       ("w31notb",	 word31 NOTB,      	NONE),
       ("w31rshift",	 word31 RSHIFT,      	NONE),
       ("w31rshiftl",    word31 RSHIFTL,      	NONE),
       ("w31lshift",	 word31 LSHIFT,      	NONE),
       ("w31gt",	 word31cmp (op >),      NONE),
       ("w31ge",	 word31cmp (op >=),     NONE),
       ("w31lt",	 word31cmp (op <),      NONE),
       ("w31le",	 word31cmp (op <=),     NONE),
       ("w31eq",	 word31cmp EQL,      	NONE),
       ("w31ne",	 word31cmp NEQ,      	NONE),
       ("w31ChkRshift",  INLRSHIFT(UINT 31),    NONE),
       ("w31ChkRshiftl", INLRSHIFTL(UINT 31),   NONE),
       ("w31ChkLshift",  INLLSHIFT(UINT 31),    NONE),
       
       (*** word32 primops ***)
       ("w32mul",	 word32 (op * ),      	NONE),
       ("w32div",	 word32 (op /),      	NONE),
       ("w32add",	 word32 (op +),      	NONE),
       ("w32sub",	 word32 (op -),      	NONE),
       ("w32orb",	 word32 ORB,      	NONE),
       ("w32xorb",	 word32 XORB,      	NONE),
       ("w32andb",	 word32 ANDB,      	NONE),
       ("w32notb",	 word32 NOTB,      	NONE),
       ("w32rshift",	 word32 RSHIFT,      	NONE),
       ("w32rshiftl",    word32 RSHIFTL,      	NONE),
       ("w32lshift",	 word32 LSHIFT,      	NONE),
       ("w32gt",	 word32cmp (op >),      NONE),
       ("w32ge",	 word32cmp (op >=),     NONE),
       ("w32lt",	 word32cmp (op <),      NONE),
       ("w32le",	 word32cmp (op <=),     NONE),
       ("w32eq",	 word32cmp EQL,      	NONE),
       ("w32ne",	 word32cmp NEQ,      	NONE),
       ("w32ChkRshift",  INLRSHIFT(UINT 32),    NONE),
       ("w32ChkRshiftl", INLRSHIFTL(UINT 32),   NONE),       
       ("w32ChkLshift",  INLLSHIFT(UINT 32),   NONE)       
      ]

end (* local *)


(**************************************************************************
 *               OTHER PRIMOP-RELATED UTILITY FUNCTIONS                   *
 **************************************************************************)

fun prNumkind (INT 31)      = ""
  | prNumkind (INT bits)    = Int.toString bits
  | prNumkind (UINT 32)     = "u"
  | prNumkind (UINT bits)   = "u" ^ Int.toString bits
  | prNumkind (FLOAT 64)    = "f"
  | prNumkind (FLOAT  bits) = "f" ^ Int.toString bits
       

fun cvtParams(from, to) = Int.toString from ^ "_" ^ Int.toString to

fun prPrimop (ARITH{oper,overflow,kind}) =
      ((case oper 
         of op + => "+" |  op - => "-" |  op * => "*"
          | op / => "/" |  op ~ => "~" | LSHIFT => "lshift" 
          | RSHIFT => "rshift" | RSHIFTL => "rshift_l" | ABS => "abs"
          | ANDB => "andb" | ORB => "orb" | XORB => "xorb" 
          | NOTB => "notb")
       ^ (if overflow then "" else "n")
       ^ prNumkind kind)

  | prPrimop (INLLSHIFT kind) =  "inllshift"  ^ prNumkind kind
  | prPrimop (INLRSHIFT kind) =  "inlrshift"  ^ prNumkind kind
  | prPrimop (INLRSHIFTL kind) = "inlrshiftl" ^ prNumkind kind

  | prPrimop (CMP{oper,kind}) =
      ((case oper 
         of op > => ">" |  op < => "<" | op >= => ">=" | op <= => "<="
          | GEU => ">=U" | GTU => ">U" | LEU => "<=U" | LTU => "<U"
          | EQL => "=" | NEQ => "<>" )
       ^ prNumkind kind)

  | prPrimop(TEST arg) = "test_" ^ cvtParams arg
  | prPrimop(TESTU arg) = "test_" ^ cvtParams arg
  | prPrimop(EXTEND arg) = "extend" ^ cvtParams arg
  | prPrimop(TRUNC arg) = "trunc" ^ cvtParams arg
  | prPrimop(COPY arg) = "copy" ^ cvtParams arg

  | prPrimop(ROUND{floor=true,fromkind=FLOAT 64,tokind=INT 31}) = "floor"
  | prPrimop(ROUND{floor=false,fromkind=FLOAT 64,tokind=INT 31}) = "round"
  | prPrimop(ROUND{floor,fromkind,tokind}) =
      ((if floor then "floor" else "round")
       ^ prNumkind fromkind ^ "_" ^ prNumkind tokind)

  | prPrimop(REAL{fromkind=INT 31,tokind=FLOAT 64}) = "real"
  | prPrimop(REAL{fromkind,tokind}) =
      ("real" ^ prNumkind fromkind ^ "_" ^ prNumkind tokind)
		   
  | prPrimop(NUMSUBSCRIPT{kind,checked,immutable}) = 
      ("numsubscript" ^ prNumkind kind
       ^ (if checked then "c" else "")
       ^ (if immutable then "v" else ""))

  | prPrimop (NUMUPDATE{kind,checked}) = 
      ("numupdate" ^ prNumkind kind ^ (if checked then  "c" else ""))

  | prPrimop DEREF = "!"
  | prPrimop ASSIGN = ":="
  | prPrimop BOXED = "boxed"
  | prPrimop UNBOXED = "unboxed"
  | prPrimop CAST = "cast"
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
  | prPrimop BOXEDUPDATE = "boxedupdate"
  | prPrimop UPDATE = "update"
  | prPrimop INLSUBSCRIPT = "inlsubscript"
  | prPrimop INLSUBSCRIPTV = "inlsubscriptv"
  | prPrimop INLUPDATE = "inlupdate"
  | prPrimop INLMKARRAY = "inlmkarray"
  | prPrimop SUBSCRIPTV = "subscriptv"
  | prPrimop GETRUNVEC = "getrunvec"
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
  | prPrimop USELVAR = "uselvar"
  | prPrimop DEFLVAR = "deflvar"
  | prPrimop INLDIV = "inldiv"
  | prPrimop INLMOD = "inlmod"
  | prPrimop INLREM = "inlrem"
  | prPrimop INLMIN = "inlmin"
  | prPrimop INLMAX = "inlmax"
  | prPrimop INLABS = "inlabs"
  | prPrimop INLNOT = "inlnot"
  | prPrimop INLCOMPOSE = "inlcompose"
  | prPrimop INLBEFORE = "inlbefore"
  | prPrimop (INL_ARRAY) = "inl_array"
  | prPrimop (INL_VECTOR) = "inl_vector"
  | prPrimop (INL_MONOARRAY kind) =
      concat ["inl_monoarray(", prNumkind kind, ")"]
  | prPrimop (INL_MONOVECTOR kind) =
      concat ["inl_monovector(", prNumkind kind, ")"]
  | prPrimop (MARKEXN) = "markexn"


val purePrimop =
  fn DEREF => false
   | ASSIGN => false 
                    (* this should probably should never be called on ASSIGN *)
   | SUBSCRIPT => false
   | BOXEDUPDATE => false
   | UNBOXEDUPDATE => false
   | UPDATE => false
   | CAPTURE => false
   | CALLCC => false
   | ISOLATE => false
   | ARITH{overflow,...} => not overflow
   | NUMSUBSCRIPT{immutable,...} => immutable
   | GETSPECIAL => false
   | SETSPECIAL => false
   | _ => true
  
val mayRaise =
  fn ARITH{overflow,...} => overflow
   | ROUND _ => true
   | INLMKARRAY => true
   | INLSUBSCRIPT => true
   | INLUPDATE => true
   | INLSUBSCRIPTV => true
   | NUMSUBSCRIPT{checked,...} => checked
   | NUMUPDATE{checked,...} => checked
   | _ => false

end  (* structure PrimOp *)

(*
 * $Log: primop.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:36  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.3  1998/01/29 21:40:50  jhr
 *   Implement isolate as a primop to make sure that it indeed
 *   achieve the intended effect (i.e., forgetting the current
 *   stack). [zsh]
 *
 * Revision 1.2  1997/05/05 19:59:41  george
 *   Change the term language into the quasi-A-normal form. Added a new round
 *   of lambda contraction before and after type specialization and
 *   representation analysis. Type specialization including minimum type
 *   derivation is now turned on all the time. Real array is now implemented
 *   as realArray. A more sophisticated partial boxing scheme is added and
 *   used as the default.
 *
 * Revision 1.1.1.1  1997/01/14  01:38:10  george
 *   Version 109.24
 *
 *)
