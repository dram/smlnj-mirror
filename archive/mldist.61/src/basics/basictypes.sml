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

infix -->
val arrowTycon as GENtyc{stamp=arrowStamp,...} =
			mkABStyc([arrowSym],2,NO,Stampset.globalStamps)
fun t1 --> t2 = CONty(arrowTycon,[t1,t2])
fun isArrowType(CONty(GENtyc{stamp,...},_)) = (stamp = arrowStamp)
  | isArrowType(VARty(ref(INSTANTIATED ty))) = isArrowType ty
  | isArrowType _ = false
fun domain(CONty(_,[ty,_])) = ty
  | domain _ = ErrorMsg.impossible "domain"
fun range(CONty(_,[_,ty])) = ty
  | range _ = ErrorMsg.impossible "range"

val intTycon = mkABStyc([intSym],0,YES,Stampset.globalStamps)
val intTy = CONty(intTycon,nil)

val realTycon  = mkABStyc([realSym],0,YES,Stampset.globalStamps)
val realTy = CONty(realTycon,nil)

val stringTycon  = mkABStyc([stringSym],0,YES,Stampset.globalStamps)
val stringTy = CONty(stringTycon,nil)

val exnTycon = mkABStyc([exceptionSym],0,NO,Stampset.globalStamps)
val exnTy = CONty(exnTycon,nil)

val contTycon = mkABStyc([contSym],1,NO,Stampset.globalStamps)

val arrayTycon = mkABStyc([arraySym],1,OBJ,Stampset.globalStamps)

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

val r = ref (nil: datacon list)
val boolTycon = mkDATAtyc([boolSym],0,r,YES,Stampset.globalStamps)
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

val _ = r := [falseDcon,trueDcon]

(* option *)

val r = ref (nil: datacon list)
val optionTycon =
	 mkDATAtyc([optionSym],1,r,YES,Stampset.globalStamps)
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
val _ = r := [NONEDcon,SOMEDcon]

(* references *)

val r = ref (nil: datacon list)
val refTycon = mkDATAtyc([refSym],1,r,OBJ,Stampset.globalStamps)
val refTyfun = TYFUN {arity = 1, body = alpha --> CONty(refTycon, [alpha])}
val refDcon = 
    DATACON
      {name = refSym,
       const = false,
       rep = REF,
       typ = POLYty {sign = [{weakness=1,eq=false}], tyfun = refTyfun, abs=0},
       sign = [REF]}
val refPatType = POLYty {sign = [{weakness=infinity,eq=false}], tyfun = refTyfun, abs=0}
val _ = r := [refDcon]

(* lists *)
val r = ref (nil: datacon list)
val listsign = [TRANSPARENT,CONSTANT 0]
val listTycon = mkDATAtyc([listSym],1,r,YES,Stampset.globalStamps)
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
val _ = r := [consDcon,nilDcon]

end (* structure BasicTypes *)

