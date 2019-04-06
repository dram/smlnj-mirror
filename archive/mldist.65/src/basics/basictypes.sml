(* Copyright 1989 by AT&T Bell Laboratories *)
(* basictypes.sml *)

structure BasicTypes : BASICTYPES = struct

structure Basics=Basics

open Basics Basics.Symbol

val arrowSym 	 = symbol "->"
val boolSym	 = symbol "bool"
val trueSym	 = symbol "true"
val falseSym	 = symbol "false"
val optionSym    = symbol "option"
val NONESym      = symbol "NONE"
val SOMESym      = symbol "SOME"
val unitSym	 = symbol "unit"
val listSym	 = symbol "list"
val consSym	 = symbol "::"
val nilSym	 = symbol "nil"
val refSym	 = symbol "ref"
val intSym	 = symbol "int"
val realSym	 = symbol "real"
val stringSym	 = symbol "string"
val exceptionSym = symbol "exn"
val contSym	 = symbol "cont"
val arraySym	 = symbol "array"

(* primitive type constructors and types *)
val {tycStamps=globalTycStamps,...} = Stampset.globalStamps

infix -->
val arrowStamp = Stampset.newStamp(globalTycStamps)
val arrowTycon = GENtyc{stamp = arrowStamp, path = [arrowSym], arity = 2,
			eq = ref NO, kind = ref(PRIMtyc)}
fun t1 --> t2 = CONty(arrowTycon,[t1,t2])
fun isArrowType(CONty(GENtyc{stamp,...},_)) = (stamp = arrowStamp)
  | isArrowType(VARty(ref(INSTANTIATED ty))) = isArrowType ty
  | isArrowType _ = false
fun domain(CONty(_,[ty,_])) = ty
  | domain _ = ErrorMsg.impossible "domain"
fun range(CONty(_,[_,ty])) = ty
  | range _ = ErrorMsg.impossible "range"

val intTycon = GENtyc{stamp = Stampset.newStamp(globalTycStamps),
		      path = [intSym], arity = 0,
		      eq = ref YES, kind = ref(PRIMtyc)}
val intTy = CONty(intTycon,nil)

val realTycon = GENtyc{stamp = Stampset.newStamp(globalTycStamps),
		       path = [realSym], arity = 0,
		       eq = ref YES, kind = ref(PRIMtyc)}
val realTy = CONty(realTycon,nil)

val stringTycon = GENtyc{stamp = Stampset.newStamp(globalTycStamps),
		         path = [stringSym], arity = 0,
		         eq = ref YES, kind = ref(PRIMtyc)}
val stringTy = CONty(stringTycon,nil)

val exnTycon = GENtyc{stamp = Stampset.newStamp(globalTycStamps),
		      path = [exceptionSym], arity = 0,
		      eq = ref NO, kind = ref(PRIMtyc)}
val exnTy = CONty(exnTycon,nil)

val contTycon = GENtyc{stamp = Stampset.newStamp(globalTycStamps),
		       path = [contSym], arity = 1,
		       eq = ref NO, kind = ref(PRIMtyc)}

val arrayTycon = GENtyc{stamp = Stampset.newStamp(globalTycStamps),
		        path = [arraySym], arity = 1,
		        eq = ref OBJ, kind = ref(PRIMtyc)}

(* building record and product types *)

fun recordTy(fields: (label * ty) list) : ty = 
    CONty(Tuples.mkRECORDtyc(map (fn (a,b) => a) fields),
	  (map (fn(a,b)=>b) fields))

fun tupleTy(tys: ty list) : ty =
    CONty(Tuples.mkTUPLEtyc(length tys), tys)

val unitTycon = Tuples.mkTUPLEtyc 0
val unitTy = CONty(unitTycon, nil)
fun isUnitTy _ = raise Bind

(* predefined datatypes *)

val alpha = VARty(mkTyvar(IBOUND 0))

(* bool *)

val kind = ref (DATAtyc nil)
val boolTycon =
    GENtyc{stamp = Stampset.newStamp(globalTycStamps),
	   path = [boolSym], arity = 0, eq = ref YES, kind = kind}
val boolTy = CONty(boolTycon,nil)
val boolsign = [CONSTANT 0, CONSTANT 1]
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
val _ = kind := DATAtyc [falseDcon,trueDcon]

(* option *)

val kind = ref (DATAtyc nil)
val optionTycon =
    GENtyc{stamp = Stampset.newStamp(globalTycStamps),
	   path = [optionSym], arity = 1, eq = ref YES, kind = kind}
val optionsign = [CONSTANT 0, TAGGED 0]
val NONEDcon = 
    DATACON
      {name = NONESym,
       const = true,
       rep = CONSTANT 0,
       typ = POLYty {sign = [{weakness=infinity,eq=false}], abs=0,
			   tyfun = TYFUN{arity=1,
					 body=CONty(optionTycon,[alpha])}},
       sign = optionsign}
val SOMEDcon =
    DATACON
      {name = SOMESym,
       const = false,
       rep = TAGGED 0,
       typ = POLYty {sign = [{weakness=infinity,eq=false}], abs=0,
		   tyfun = TYFUN
			    {arity = 1,
			     body = CONty(arrowTycon,
				      [alpha, CONty(optionTycon,[alpha])])}},
       sign = optionsign}
val _ = kind := DATAtyc [NONEDcon,SOMEDcon]

(* references *)

val kind = ref (DATAtyc nil)
val refTycon =
    GENtyc{stamp = Stampset.newStamp(globalTycStamps),
	   path = [refSym], arity = 1, eq = ref OBJ, kind = kind}
val refTyfun = TYFUN {arity = 1, body = alpha --> CONty(refTycon, [alpha])}
val refDcon = 
    DATACON
      {name = refSym,
       const = false,
       rep = REF,
       typ = POLYty {sign = [{weakness=1,eq=false}], tyfun = refTyfun, abs=0},
       sign = [REF]}
val refPatType = POLYty {sign = [{weakness=infinity,eq=false}], tyfun = refTyfun, abs=0}
val _ = kind := DATAtyc [refDcon]

(* lists *)
val kind = ref (DATAtyc nil)
val listsign = [TRANSPARENT,CONSTANT 0]
val listTycon =
    GENtyc{stamp = Stampset.newStamp(globalTycStamps),
	   path = [listSym], arity = 1, eq = ref YES, kind = kind}
val consDcon =
    DATACON 
      {name = consSym,
       const = false,
       rep = TRANSPARENT,
       typ = POLYty {sign = [{weakness=infinity,eq=false}], abs=0,
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
       rep = CONSTANT 0,
       typ = POLYty {sign = [{weakness=infinity,eq=false}], abs=0,
		   tyfun = TYFUN{arity=1,body=CONty(listTycon,[alpha])}},
       sign = listsign}
val _ = kind := DATAtyc [consDcon,nilDcon]

end (* structure BasicTypes *)

