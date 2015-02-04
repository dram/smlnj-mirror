(* basictypes.sml *)

structure BasicTypes : BASICTYPES = struct

structure Basics=Basics

local open Basics Basics.Symbol SymbolTable
 in

val arrowSym 	 = StringToSymbol("->")
and boolSym	 = StringToSymbol("bool")
and trueSym	 = StringToSymbol("true")
and falseSym	 = StringToSymbol("false")
and unitSym	 = StringToSymbol("unit")
and listSym	 = StringToSymbol("list")
and consSym	 = StringToSymbol("::")
and nilSym	 = StringToSymbol("nil")
and refSym	 = StringToSymbol("ref")
and intSym	 = StringToSymbol("int")
and realSym	 = StringToSymbol("real")
and stringSym	 = StringToSymbol("string")
and exceptionSym = StringToSymbol("exn")
and arraySym	 = StringToSymbol("array")
and byte_arraySym = StringToSymbol("byte_array")

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

val EXNdcons = ref nil
val exnTycon = ref(mkATOMtyc(exceptionSym,0));
val exnTy = CONty(exnTycon,nil);

val arrayTycon = ref(mkATOMtyc(arraySym,1));

val byte_arrayTycon = ref(mkATOMtyc(byte_arraySym,0));


val byte_arrayTy = CONty(byte_arrayTycon,nil);


(* building record and product types *)

fun RECORDty(fields: (label * ty) list) : ty = 
    CONty(ref(Tuples.mkRECORDtyc(map (fn (a,b) => a) fields)),
	  (map (fn(a,b)=>b) fields))

fun TUPLEty(tys: ty list) : ty =
    CONty(ref(Tuples.mkTUPLEtyc(length tys)), tys)

val unitTycon = ref(Tuples.mkTUPLEtyc(0))
val unitTy = CONty(unitTycon, nil)


(* predefined datatypes *)

(* bool *)

val booldcons = ref nil
val boolTycon  = ref(mkDATAtyc(boolSym,[], booldcons));
val boolTy = CONty (boolTycon,nil);

val FALSEdcon = DATACON {name = falseSym,
		     const = true,
		     rep = ref (CONSTANT 0),
		     vtype = boolTy,
		     dcons = booldcons}

val TRUEdcon = DATACON {name = trueSym,
		     const = true,
		     rep = ref(CONSTANT 1),
		     vtype = boolTy,
		     dcons = booldcons}


(* references *)
val alpha = mkTyvar(StringToSymbol("'a"),BOUND)

val refdcons = ref nil
val refTycon = ref(mkDATAtyc(refSym,[alpha], refdcons))

val REFdcon = DATACON {name=refSym,
		    const=false,
		    rep = ref REF,
		    vtype= VARty alpha --> CONty(refTycon, [VARty alpha]),
		    dcons = refdcons}


(* lists *)

val listdcons = ref nil
val listTycon = ref(mkDATAtyc(listSym, [alpha], listdcons))

val NILdcon = 
    DATACON {name = nilSym,
	     const = true,
	     rep = ref(CONSTANT 0),
	     vtype = CONty(listTycon,[VARty alpha]),
	     dcons = listdcons}

val CONSdcon =
    DATACON {name = consSym,
	     const = false,
	     rep = ref TRANSB,
	     vtype = CONty(arrowTycon,
		       [TUPLEty[VARty alpha,CONty(listTycon,[VARty alpha])],
			CONty(listTycon,[VARty alpha])]),
	     dcons = listdcons}

fun newEqualityType() =
     let val tyv = VARty(mkTyvar(StringToSymbol("'eq"),METAARG))
      in TUPLEty[tyv,tyv] --> boolTy
     end

fun newAssignType() =
     let val tyv = VARty(mkTyvar(StringToSymbol("'assign"),METAARG))
      in TUPLEty[CONty(refTycon,[tyv]),tyv] --> unitTy
     end

fun newUpdateType() =
     let val tyv = VARty(mkTyvar(StringToSymbol("'assign"),METAARG))
      in TUPLEty[CONty(arrayTycon,[tyv]),intTy,tyv] --> unitTy
     end

fun SetBasics () =
	let val (DATAtyc {dcons=booldcons,...}) = !boolTycon
	and (DATAtyc {dcons=listdcons,...}) = !listTycon
        and (DATAtyc {dcons=refdcons,...}) = !refTycon
	 in
		booldcons := [FALSEdcon,TRUEdcon];
		listdcons := [NILdcon,CONSdcon];
		refdcons := [REFdcon]
	end

fun UnsetBasics () =
	let val (DATAtyc {dcons=booldcons,...}) = !boolTycon
	and (DATAtyc {dcons=listdcons,...}) = !listTycon
        and (DATAtyc {dcons=refdcons,...}) = !refTycon
	 in
		booldcons := nil;
		listdcons := nil;
		refdcons := nil
	end


(* initialization of primitive datatypes *)

val () = SetBasics()


end; (* local open Bindings ... *)

end; (* structure BasicTypes *)

