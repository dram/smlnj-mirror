(* printtype.sml *)

signature PRINTTYPE = sig
  structure Basics : BASICS
  val tyvarNames : string array
  val typeFormals : int -> string list
  val printTyvar : Basics.tyvar -> unit
  val printTycon : Basics.tycon -> unit
  val printType : Basics.ty -> unit
  val resetPrintType : unit -> unit
end

structure PrintType : PRINTTYPE = struct

structure Basics = Basics

open Basics List2 PrintUtil

fun tvName k =
    "'" ^ (if k < 26 then chr(k+ord"a")
		     else chr(k div 26 + ord"a") ^ chr(k mod 26 + ord"a"))

val tyvarNames: string array = 
    let val names = array(50,"")
	    fun fill k = (update(names,k,tvName k); fill(k+1))
     in fill 0 handle Subscript => ();
	names
    end

fun typeFormals n =
    let fun loop i =
	if i>=n then []
	else (tyvarNames sub i)::loop(i+1)
     in loop 0
    end

local  
  val count = ref(~1)
  val tyvars = ref([]:tyvar list)
in
  fun tyvarName(tv: tyvar) =
      let fun find([],_) =
	        (tyvars	:= !tyvars @ [tv];
		 inc count;
		 !count)
	    | find(tv'::rest,k) =
	        if tv = tv'
		then k
		else find(rest,k-1)
       in tyvarNames sub (find(!tyvars,!count))
      end
  fun resetPrintType() =
      (count := ~1; tyvars := [])
end

val internals = System.Control.internals

fun printTyvar(tv as (ref kind): tyvar) : unit =
     case kind
       of IBOUND n => prstr(tyvarNames sub n)
	| METAARG => (prstr(tyvarName tv); prstr "A")
	| METALAM n => (prstr(tyvarName tv); prstr "L"; print n; ())
	| UBOUND id => (printSym(id); prstr "U")
	| INSTANTIATED _ => (prstr(tyvarName tv); prstr "I")
	    
fun prkind ABStyc = prstr "ABS/"
  | prkind (DEFtyc _) = prstr "DEF/"
  | prkind (DATAtyc _) = prstr "DATA/"
  | prkind (RECORDtyc _) = prstr "RECORD/"

fun printTycon(TYCON{kind=RECORDtyc(labels),...}) =
       printClosedSequence("{",",","}") printSym labels
  | printTycon(TYCON{name,stamp,kind,...}) =
      (if !internals then (prkind kind; print stamp; prstr "/") else ();
       printSym name)
  | printTycon(INDtyc p) =
      (prstr "IND/"; printSequence "," (fn x:int =>(print x; ())) p; prstr "/")
  | printTycon(UNDEFtyc name) =
      (if !internals then (prstr "UNDEF/") else ();
       printSym name)

val TYCON{stamp=arrowStamp,...} = !BasicTypes.arrowTycon

fun printType(ty: ty) : unit =
    case ty
      of VARty(ref(INSTANTIATED ty')) => printType ty'
       | VARty(tv) => printTyvar tv
       | CONty(ref tycon, args) =>
	   (case tycon
	      of TYCON{kind=ABStyc,stamp,...} => 
		   if stamp = arrowStamp
		   then let val [domain,range] = args
			 in prstr "("; printType domain;
			    prstr " -> ";
			    printType range; prstr ")"
			end
		   else (printTypeArgs args; printTycon tycon)	
	       | TYCON{kind=RECORDtyc labels,...} =>
		   if Tuples.isTUPLEtyc(tycon)
		   then printTUPLEty args
		   else printRECORDty(labels, args)
	       | UNDEFtyc _ =>
		   (printTypeArgs args; printTycon tycon; prstr "?")
	       | _ => (printTypeArgs args; printTycon tycon))
       | POLYty(TYFUN{arity,body}) => (prstr "!"; printType body)
       | FLEXRECORDty(ref(CLOSED ty)) => printType ty
       | FLEXRECORDty(ref(OPEN fields)) => printFields fields
       | ERRORty => prstr "wrong"
       | UNDEFty => prstr "type?"

and printTypeArgs nil = ()
  | printTypeArgs [ty] = (printType ty; prstr " ")
  | printTypeArgs tys = printClosedSequence ("(", ",", ") ") printType tys

and printTUPLEty nil = prstr "unit"
  | printTUPLEty tys = printClosedSequence ("(", "*", ")") printType tys

and printField(lab,arg) = (printSym lab; prstr ":"; printType arg)

and printRECORDty([],[]) = prstr "unit"
  | printRECORDty(lab::labels, arg::args) =
      (prstr "{";
       printField(lab,arg);
       app2 
         (fn field => (prstr ","; printField field))
	 (labels,args);
       prstr "}")
  | printRECORDty _ = ErrorMsg.impossible "PrintType.printRECORDty"

and printFields [] = prstr "{...}"	
  | printFields (field::fields) =
      (prstr "{";
       printField field;
       app 
         (fn field => (prstr ","; printField field))
	 fields;
       prstr ",...}");

end (* structure PrintType *)
