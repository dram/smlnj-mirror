(* basictypes.sml *)

structure BasicTypes : BASICTYPES = struct

structure Basics=Basics

local open Basics Basics.Symbol SymbolTable
 in

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
and byte_arraySym = stringToSymbol("byte_array")

(* primitive type constructors and types *)

val arrowTycon = ref(mkATOMtyc(arrowSym,2));
infix -->
fun t1 --> t2 = CONty(arrowTycon,[t1,t2]);

val intTycon = ref(mkATOMtyc(intSym,0));
val intTy = CONty(intTycon,nil);

val realTycon  = ref(mkATOMtyc(realSym,0));
val realTy = CONty(realTycon,nil);

val stringTycon  = ref(mkATOMtyc(stringSym,0));
val stringTy = CONty(stringTycon,nil);

val exnDcons = ref nil
val exnTycon = ref(mkATOMtyc(exceptionSym,0));
val exnTy = CONty(exnTycon,nil);

val arrayTycon = ref(mkATOMtyc(arraySym,1));

val byte_arrayTycon = ref(mkATOMtyc(byte_arraySym,0));


val byte_arrayTy = CONty(byte_arrayTycon,nil);


(* building record and product types *)

fun recordTy(fields: (label * ty) list) : ty = 
    CONty(ref(Tuples.mkRECORDtyc(map (fn (a,b) => a) fields)),
	  (map (fn(a,b)=>b) fields))

fun tupleTy(tys: ty list) : ty =
    CONty(ref(Tuples.mkTUPLEtyc(length tys)), tys)

val unitTycon = ref(Tuples.mkTUPLEtyc(0))
val unitTy = CONty(unitTycon, nil)


(* predefined datatypes *)

val alpha = mkTyvar(stringToSymbol("'a"),BOUND)

(* bool *)

val booldcons = ref nil
val boolTycon  = ref(mkDATAtyc(boolSym,[], booldcons));
val boolTy = CONty (boolTycon,nil);

val falseDcon = DATACON {name = falseSym,
		     const = true,
		     rep = ref (CONSTANT 0),
		     vtype = boolTy,
		     dcons = booldcons}

val trueDcon = DATACON {name = trueSym,
		     const = true,
		     rep = ref(CONSTANT 1),
		     vtype = boolTy,
		     dcons = booldcons}

val _ = booldcons := [falseDcon,trueDcon]  (* initialization *)


(* references *)

val refdcons = ref nil
val refTycon = ref(mkDATAtyc(refSym,[alpha], refdcons))

val refDcon = DATACON {name=refSym,
		    const=false,
		    rep = ref REF,
		    vtype= VARty alpha --> CONty(refTycon, [VARty alpha]),
		    dcons = refdcons}

val _ = refdcons := [refDcon]  (* initialization *)


(* lists *)

val listdcons = ref nil
val listTycon = ref(mkDATAtyc(listSym, [alpha], listdcons))
val nilRep = ref(CONSTANT 0)
val consRep = ref TRANSB

val nilDcon = 
    DATACON {name = nilSym,
	     const = true,
	     rep = nilRep,
	     vtype = CONty(listTycon,[VARty alpha]),
	     dcons = listdcons}

val consDcon =
    DATACON {name = consSym,
	     const = false,
	     rep = consRep,
	     vtype = CONty(arrowTycon,
		       [tupleTy[VARty alpha,CONty(listTycon,[VARty alpha])],
			CONty(listTycon,[VARty alpha])]),
	     dcons = listdcons}

fun isListDcon(DATACON{rep,...}) = (rep = nilRep) orelse (rep = consRep)

val _ = listdcons := [nilDcon,consDcon]  (* initialization *)


fun newEqualityType() =
     let val tyv = VARty(mkTyvar(stringToSymbol("'eq"),METAARG))
      in tupleTy[tyv,tyv] --> boolTy
     end

fun newAssignType() =
     let val tyv = VARty(mkTyvar(stringToSymbol("'assign"),METAARG))
      in tupleTy[CONty(refTycon,[tyv]),tyv] --> unitTy
     end

fun newUpdateType() =
     let val tyv = VARty(mkTyvar(stringToSymbol("'assign"),METAARG))
      in tupleTy[CONty(arrayTycon,[tyv]),intTy,tyv] --> unitTy
     end


end (* local open Bindings ... *)

end (* structure BasicTypes *)

