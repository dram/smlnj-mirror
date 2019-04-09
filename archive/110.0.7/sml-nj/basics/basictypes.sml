(* Copyright 1996 by AT&T Bell Laboratories *)
(* basictypes.sml *)

structure BasicTypes : BASICTYPES = 
struct

local open Access Types Symbol 
      structure EM = ErrorMsg
      structure IP = InvPath
      structure PT = PrimTyc
  fun bug msg = ErrorMsg.impossible("BasicTypes: "^msg)
in

(*** type and dataconstructor symbols ***)
val unitSym      = tycSymbol "unit"
val boolSym      = tycSymbol "bool"
val trueSym	 = varSymbol "true"
val falseSym	 = varSymbol "false"
val listSym	 = tycSymbol "list"
val consSym	 = varSymbol "::"
val nilSym	 = varSymbol "nil"
val refConSym	 = varSymbol "ref"
val refTycSym	 = tycSymbol "ref"
val fragSym      = tycSymbol "frag"
val antiquoteSym = varSymbol "ANTIQUOTE"
val quoteSym     = varSymbol "QUOTE"

(*** primitive type constructors and types ***)

(*** function type constructor ***)
infix -->
val arrowStamp = Stamps.special "->"
val arrowTycon =
    GENtyc{stamp = arrowStamp, path = IP.IPATH [tycSymbol "->"],
	   arity = 2, eq = ref NO, kind = PRIMITIVE(PT.ptc_arrow)}
fun t1 --> t2 = CONty(arrowTycon,[t1,t2])
fun isArrowType(CONty(GENtyc{stamp,...},_)) = Stamps.eq(stamp,arrowStamp)
  | isArrowType(VARty(ref(INSTANTIATED ty))) = isArrowType ty
  | isArrowType _ = false
fun domain(CONty(_,[ty,_])) = ty
  | domain _ = bug "domain"
fun range(CONty(_,[_,ty])) = ty
  | range _ = bug "range"


(*** primitive types ***)

fun mkpt (sym,arity,eqprop,pt) =
    GENtyc{stamp = Stamps.special sym, path = IP.IPATH[tycSymbol sym],
	   arity = arity, eq = ref eqprop, kind = PRIMITIVE pt}

val intTycon = mkpt ("int", 0, YES, PT.ptc_int31)
val intTy = CONty(intTycon, nil)

val int32Tycon = mkpt ("int32", 0, YES, PT.ptc_int32)
val int32Ty = CONty(int32Tycon, nil)

val realTycon = mkpt("real", 0, NO, PT.ptc_real)
val realTy = CONty(realTycon, nil)

val wordTycon = mkpt("word", 0, YES, PT.ptc_int31)
val wordTy = CONty(wordTycon, nil)

val word8Tycon = mkpt("word8", 0, YES, PT.ptc_int31)
val word8Ty = CONty(word8Tycon, nil)

val word32Tycon = mkpt("word32", 0, YES, PT.ptc_int32)
val word32Ty = CONty(word32Tycon, nil)

val stringTycon = mkpt("string", 0, YES, PT.ptc_string)
val stringTy = CONty(stringTycon, nil)

val charTycon = mkpt("char", 0, YES, PT.ptc_int31)
val charTy = CONty(charTycon, nil)

val exnTycon = mkpt("exn", 0, NO, PT.ptc_exn)
val exnTy = CONty(exnTycon, nil)

val contTycon = mkpt("cont", 1, NO, PT.ptc_cont)
val ccontTycon = mkpt("control_cont", 1, NO, PT.ptc_ccont)

val arrayTycon = mkpt("array", 1, OBJ, PT.ptc_array)

val vectorTycon = mkpt("vector", 1, YES, PT.ptc_vector)

val objectTycon = mkpt("object", 0, NO, PT.ptc_obj)

val c_functionTycon = mkpt("c_function", 0, NO, PT.ptc_cfun)

val word8arrayTycon = mkpt("word8array", 0, OBJ, PT.ptc_barray)

val real64arrayTycon = mkpt("real64array", 0, OBJ, PT.ptc_rarray)

val spin_lockTycon = mkpt("spin_lock", 0, NO, PT.ptc_slock)


(*** building record and product types ***)

fun recordTy(fields: (label * ty) list) : ty = 
    CONty(Tuples.mkRECORDtyc(map (fn (a,b) => a) fields),
	  (map (fn(a,b)=>b) fields))

fun tupleTy(tys: ty list) : ty =
    CONty(Tuples.mkTUPLEtyc(length tys), tys)

(* 
 * I believe that unitTycon only needs to be a DEFtyc because of
 * the "structure PrimTypes = struct open PrimTypes end" declaration
 * in boot/built-in.sml.  This in turn is only necessary because of
 * a problem with the access assigned to PrimTypes. - DBM 
 *)
val unitTycon = DEFtyc{stamp=Stamps.special "unit",
                       tyfun=TYFUN{arity=0,body=CONty(Tuples.mkTUPLEtyc 0,[])},
		       strict=[],path=IP.IPATH[unitSym]}
(* val unitTycon = Tuples.mkTUPLEtyc 0 *)
val unitTy = CONty(unitTycon, nil)


(*** predefined datatypes ***)
val alpha = IBOUND 0

(* primitive datatypes *)

(* bool *)

val boolTy0 = CONty(RECtyc 0,nil)
val boolStamp = Stamps.special "bool"
val boolsign = CSIG(0,2)
val booleq = ref YES
val kind = DATATYPE{index=0, lambdatyc=ref NONE,
		    members= #[{tycname=boolSym,stamp=boolStamp, eq=booleq,
				arity=0, sign=boolsign, lambdatyc=ref NONE,
				dcons=[{name=falseSym,rep=CONSTANT 0,
					domain=NONE},
				       {name=trueSym,rep=CONSTANT 1,
					domain=NONE}]}]}
val boolTycon =
    GENtyc{stamp = boolStamp, path = IP.IPATH[boolSym],
	   arity = 0, eq = booleq, kind = kind}
val boolTy = CONty(boolTycon,nil)
val falseDcon = 
    DATACON
      {name = falseSym,
       const = true,
       rep = CONSTANT 0,
       typ = boolTy,
       sign = boolsign}
val trueDcon =
    DATACON
      {name = trueSym,
       const = true,
       rep = CONSTANT 1,
       typ = boolTy,
       sign = boolsign}


(* references *)

val refDom = alpha
val refStamp = Stamps.special "ref"
val refsign = CSIG(1,0)
val refEq = ref OBJ
val kind = DATATYPE{index=0, lambdatyc=ref NONE,
		    members= #[{tycname=refTycSym,stamp=refStamp,eq=refEq,
                                arity=1, sign=refsign, lambdatyc=ref NONE,
			        dcons=[{name=refConSym,rep=REF,
				        domain=SOME refDom}]}]}
val refTycon =
    GENtyc{stamp = refStamp, path = IP.IPATH[refTycSym],
	   arity = 1, eq = refEq, kind = kind}
val refTyfun = TYFUN{arity = 1, body = alpha --> CONty(refTycon, [alpha])}
val refDcon = 
    DATACON
      {name = refConSym,
       const = false,
       rep = REF,
       typ = POLYty{sign = [false], tyfun = refTyfun},
       sign = refsign}
val refPatType = POLYty {sign = [false], tyfun = refTyfun}


(* lists *)

val listStamp = Stamps.special "list"
val consDom = tupleTy[alpha, CONty(RECtyc 0,[alpha])]
val listsign = CSIG(1,1) (* [UNTAGGED,CONSTANT 0], [LISTCONS,LISTNIL] *) 
val listeq = ref YES
val kind = DATATYPE{index=0, lambdatyc=ref NONE,
		    members= #[{tycname=listSym,stamp=listStamp,eq=listeq,
				arity=1, sign=listsign, lambdatyc=ref NONE,
				dcons=[{name=consSym,rep=UNTAGGED,
					domain=SOME consDom},
				       {name=nilSym,rep=CONSTANT 0,domain=NONE}]}]}
val listTycon =
    GENtyc{stamp = listStamp, path = IP.IPATH[listSym], arity = 1,
	   eq = listeq, kind = kind}
val consDcon =
    DATACON 
      {name = consSym,
       const = false,
       rep = UNTAGGED,   (* was LISTCONS *)
       typ = POLYty{sign = [false],
		    tyfun = TYFUN
			    {arity = 1,
			     body = CONty(arrowTycon,
				      [tupleTy[alpha, CONty(listTycon,[alpha])],
				       CONty(listTycon,[alpha])])}},
       sign = listsign}
val nilDcon = 
    DATACON
      {name = nilSym,
       const = true,
       rep = CONSTANT 0, (* was LISTNIL *)
       typ = POLYty {sign = [false],
		     tyfun = TYFUN{arity=1,body=CONty(listTycon,[alpha])}},
       sign = listsign}


(* unrolled lists *)
(* should this type have a different stamp from list? *)
val ulistStamp = Stamps.special "ulist"
val ulistsign = CSIG(1,1) (* [LISTCONS,LISTNIL] *)
val ulistEq = ref YES
val kind = DATATYPE{index=0, lambdatyc=ref NONE,
		    members= #[{tycname=listSym, stamp=ulistStamp, eq=ulistEq,
				arity=1, sign=ulistsign, lambdatyc=ref NONE,
				dcons=[{name=consSym,rep=LISTCONS,
					domain=SOME consDom},
				       {name=nilSym,rep=LISTNIL,domain=NONE}]}]}
val ulistTycon =
    GENtyc{stamp = ulistStamp, path = IP.IPATH[listSym], arity = 1,
	   eq = ulistEq, kind = kind}

val uconsDcon =
   DATACON 
    {name = consSym,
     const = false,
     rep = LISTCONS, 
     typ = POLYty
            {sign = [false],
	     tyfun = TYFUN{arity = 1,
			   body = CONty(arrowTycon,
					[tupleTy[alpha,CONty(ulistTycon,[alpha])],
					 CONty(ulistTycon,[alpha])])}},
     sign = ulistsign}

val unilDcon = 
   DATACON
    {name = nilSym,
     const = true,
     rep = LISTNIL, 
     typ = POLYty {sign = [false],
		   tyfun = TYFUN{arity=1,body=CONty(ulistTycon,[alpha])}},
     sign = ulistsign}


(* frags *)

val antiquoteDom = alpha
val quoteDom = stringTy
val fragStamp = Stamps.special "frag"
val fragsign = CSIG(2, 0) (* [TAGGED 0, TAGGED 1] *)
val frageq = ref YES
val kind = DATATYPE{index=0, lambdatyc=ref NONE,
		    members= #[{tycname=fragSym,stamp=fragStamp, eq=frageq,
				arity=1, sign=fragsign, lambdatyc=ref NONE,
				dcons=[{name=antiquoteSym,rep=TAGGED 0,
					domain=SOME antiquoteDom},
				       {name=quoteSym,rep=TAGGED 1,
					domain=SOME quoteDom}]}]}
(* predefine path as "SMLofNJ.frag", since it will be replicated into
 * the SMLofNJ structure *)
val fragTycon =
    GENtyc{stamp = fragStamp, path = IP.IPATH[fragSym,strSymbol "SMLofNJ"],
	   arity = 1, eq = frageq, kind = kind}
val ANTIQUOTEDcon =
    DATACON
      {name = antiquoteSym,
       const = false,
       rep = TAGGED 0,
       typ = POLYty {sign = [false],
		     tyfun = TYFUN
			    {arity = 1,
			     body = CONty(arrowTycon,
				      [alpha, CONty(fragTycon,[alpha])])}},
       sign = fragsign}
val QUOTEDcon = 
    DATACON
      {name = quoteSym,
       const = false,
       rep = TAGGED 1,
       typ = POLYty {sign = [false],
		     tyfun = TYFUN
			    {arity = 1,
			     body = CONty(arrowTycon,
				      [stringTy, CONty(fragTycon,[alpha])])}},
       sign = fragsign}

end (* local *)
end (* structure BasicTypes *)


(*
 * $Log: basictypes.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:36  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.5  1997/12/03 21:11:00  dbm
 *   Fix for Word8Array.array equality problem (basis/tests/word8array.sml,
 *   test1).
 *   Added word8arrayTycon, etc.
 *
 * Revision 1.4  1997/09/30  02:21:29  dbm
 *   Added "SMLofNJ" to path of frag for top-level printing.
 *
 * Revision 1.3  1997/09/15  15:54:05  dbm
 *   Replaced direct calls of ErrorMsg.impossible with calls of local bug
 *   function.
 *
 * Revision 1.2  1997/03/17  18:44:15  dbm
 * Changes in datatype representation to support datatype replication.
 *
 * Revision 1.1.1.1  1997/01/14  01:38:09  george
 *   Version 109.24
 *
 *)
