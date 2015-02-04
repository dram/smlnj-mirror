(* basictypes.sml *)

structure BasicTypes : BASICTYPES = struct

structure Basics=Basics

local open Basics Basics.Symbol SymbolTable
      infix before
      fun a before b = a
 in

val tyvarStamp : int ref = ref 0
and tyconStamp : int ref = ref 0

fun mkTyvar(id: symbol, status: tvstatus) : tyvar =
      TYVAR{stamp = !tyvarStamp, name = id, status = ref status}
      before inc tyvarStamp

local
  val name = SymbolTable.StringToSymbol("'x")
in
  fun newTyvar (status : tvstatus) : tyvar =
      TYVAR{name = name, stamp = !tyvarStamp, status = ref status}
	before inc tyvarStamp
end;

fun mkATOMtyc(id: symbol, arity: int) : tycon =
      ATOMtyc{stamp = !tyconStamp, name = id, arity = arity}
      before inc tyconStamp

fun mkVARtyc(ctx: context, id: symbol, arity: int) : tycon =
      VARtyc{stamp = !tyconStamp, context=ctx, name = id, arity = arity}
      before inc tyconStamp

fun mkTYPEtyc(ctx: context, id: symbol, args: tyvar list, body: ty) : tycon =
      TYPEtyc{stamp = !tyconStamp, context = ctx, name = id, params = args, 
    	  def = body}
      before inc tyconStamp

fun mkDATAtyc(ctx: context, id: symbol, args: tyvar list, body: datacon list ref) : tycon =
      DATAtyc{stamp = !tyconStamp, context = ctx, name = id, params = args,
              dcons = body}
      before inc tyconStamp

fun mkRECORDtyc(labels: label list) : tycon =
    RECORDtyc{stamp = Tuples.hashLabels(labels), labels = labels}


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

val exnTycon = ref(mkATOMtyc(exceptionSym,0));
val exnTy = CONty(exnTycon,nil);

val arrayTycon = ref(mkATOMtyc(arraySym,1));

val byte_arrayTycon = ref(mkATOMtyc(byte_arraySym,0));


val byte_arrayTy = CONty(byte_arrayTycon,nil);


(* building record and product types *)

fun RECORDty(fields: (label * ty) list) : ty = 
    CONty(ref(mkRECORDtyc(map (fn (a,b) => a) fields)),
	  (map (fn(a,b)=>b) fields))

fun TUPLEty(tys: ty list) : ty =
    CONty(ref(Tuples.mkTUPLEtyc(length tys)), tys)

val unitTycon = ref(Tuples.mkTUPLEtyc(0))
val unitTy = CONty(unitTycon, nil)


(* predefined datatypes *)

(* bool *)

val boolTycon  = ref(mkDATAtyc(TOPctx,boolSym,[],ref []));
val boolTy = CONty (boolTycon,nil);

val FALSEdcon = DATACON {name = falseSym,
		     const = true,
		     rep = ref (CONSTANT 0),
		     vtype = boolTy,
		     tycon = !boolTycon}

val TRUEdcon = DATACON {name = trueSym,
		     const = true,
		     rep = ref(CONSTANT 1),
		     vtype = boolTy,
		     tycon = !boolTycon}


(* references *)
val alpha = mkTyvar(StringToSymbol("'a"),BOUND)

val refTycon = ref(mkDATAtyc(TOPctx,refSym,[alpha], ref nil))

val REFdcon = DATACON {name=refSym,
		    const=false,
		    rep = ref REF,
		    vtype= VARty alpha --> CONty(refTycon, [VARty alpha]),
		    tycon= !refTycon}


(* lists *)

val listTycon = ref(mkDATAtyc(TOPctx,listSym, [alpha], ref nil))

val NILdcon = 
    DATACON {name = nilSym,
	     const = true,
	     rep = ref(CONSTANT 0),
	     vtype = CONty(listTycon,[VARty alpha]),
	     tycon = !listTycon}

val CONSdcon =
    DATACON {name = consSym,
	     const = false,
	     rep = ref TRANSB,
	     vtype = CONty(arrowTycon,
		       [TUPLEty[VARty alpha,CONty(listTycon,[VARty alpha])],
			CONty(listTycon,[VARty alpha])]),
	     tycon = !listTycon}


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

