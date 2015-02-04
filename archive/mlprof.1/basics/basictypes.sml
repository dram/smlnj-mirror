(* basictypes.sml *)

structure BasicTypes : BASICTYPES = struct

structure Basics=Basics

open Basics Basics.Symbol Symbols

val arrowSym 	 = stringToSymbol("->")
and boolSym	 = stringToSymbol("bool")
and trueSym	 = stringToSymbol("true")
and falseSym	 = stringToSymbol("false")
and unitSym	 = stringToSymbol("unit")
and listSym	 = stringToSymbol("list")
and consSym	 = stringToSymbol("::")
and nilSym	 = stringToSymbol("nil")
and refSym	 = stringToSymbol("ref")
and intSym	 = stringToSymbol("int")
and realSym	 = stringToSymbol("real")
and stringSym	 = stringToSymbol("string")
and exceptionSym = stringToSymbol("exn")
and arraySym	 = stringToSymbol("array")

(* primitive type constructors and types *)

infix -->
val arrowTycon = ref(mkABStyc(arrowSym,2,NONE))
fun t1 --> t2 = CONty(arrowTycon,[t1,t2])

val intTycon = ref(mkABStyc(intSym,0,NONE))
val intTy = CONty(intTycon,nil)

val realTycon  = ref(mkABStyc(realSym,0,NONE))
val realTy = CONty(realTycon,nil)

val stringTycon  = ref(mkABStyc(stringSym,0,NONE))
val stringTy = CONty(stringTycon,nil)

val exnTycon = ref(mkABStyc(exceptionSym,0,NONE))
val exnTy = CONty(exnTycon,nil)

val arrayTycon = ref(mkABStyc(arraySym,1,NONE))


(* building record and product types *)

fun recordTy(fields: (label * ty) list) : ty = 
    CONty(ref(Tuples.mkRECORDtyc(map (fn (a,b) => a) fields)),
	  (map (fn(a,b)=>b) fields))

fun tupleTy(tys: ty list) : ty =
    CONty(ref(Tuples.mkTUPLEtyc(length tys)), tys)

val unitTycon = ref(Tuples.mkTUPLEtyc(0))
val unitTy = CONty(unitTycon, nil)


(* predefined datatypes *)

val alpha = VARty(mkTyvar(IBOUND 0))

(* bool *)

val boolsign = [CONSTANT 0, CONSTANT 1]
val boolTycon = ref(!unitTycon)
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
val _ = boolTycon := mkDATAtyc(boolSym,0,NONE,ref[falseDcon,trueDcon])


(* references *)

val refTycon = ref(!unitTycon)
val refDcon = 
    DATACON
      {name = refSym,
       const = false,
       rep = REF,
       typ = POLYty(TYFUN
	       {arity = 1,
		body = alpha --> CONty(refTycon, [alpha])}),
       sign = [REF]}
val _ = refTycon := mkDATAtyc(refSym,1,NONE,ref[refDcon])


(* lists *)

val listsign = [TRANSB,CONSTANT 0]
val listTycon = ref(!unitTycon)
val consDcon =
    DATACON 
      {name = consSym,
       const = false,
       rep = TRANSB,
       typ = POLYty(TYFUN
	       {arity = 1,
		body = CONty(arrowTycon,
			     [tupleTy[alpha,
				      CONty(listTycon,[alpha])],
			      CONty(listTycon,[alpha])])}),
       sign = listsign}
val nilDcon = 
    DATACON
      {name = nilSym,
       const = true,
       rep = CONSTANT 0,
       typ = POLYty(TYFUN{arity=1,body=CONty(listTycon,[alpha])}),
       sign = listsign}
val _ = listTycon := mkDATAtyc(listSym,1,NONE,ref[consDcon,nilDcon])

end (* structure BasicTypes *)

