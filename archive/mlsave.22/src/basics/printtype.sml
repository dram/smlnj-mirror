(* printtype.sml *)

signature PRINTTYPE = sig
  structure Basics : BASICS
  val typeFormals : int -> string list
  val printTyvar : Basics.tyvar -> unit
  val printTycon : Basics.tycon -> unit
  val printType : Basics.ty -> unit
  val resetPrintType : unit -> unit
end

structure PrintType : PRINTTYPE = struct

structure Basics = Basics

open ErrorMsg Basics List2 PrintUtil

val boundTyvarNames: string array =
    let val names = array(50,"")
	fun fill k =
	    (update(names,k,
	    	    if k < 26
		    then chr(k+ord"a")
		    else chr(k div 26 + ord"a") ^ chr(k mod 26 + ord"a"));
             fill(k+1))
     in fill 0 handle Subscript => ();
	names
    end

val metaTyvarNames: string array = arrayoflist
      ["S","T","U","V","W","X","Y","Z","SS","TT","UU","VV","WW","XX","YY","ZZ",
       "S1","T1","U1","V1","W1","X1","Y1","Z1","S2","T2","U2","V2","W2","X2",
       "Y2","Z2"]

fun typeFormals n =
    let fun loop i =
	if i>=n then []
	else (boundTyvarNames sub i)::loop(i+1)
     in loop 0
    end

local  
  val count = ref(~1)
  val metaTyvars = ref([]:tyvar list)
in
  fun metaTyvarName(tv: tyvar) =
      let fun find([],_) =
	        (metaTyvars := tv::(!metaTyvars);
		 inc count;
		 !count)
	    | find(tv'::rest,k) =
	        if tv = tv'
		then !count - k
		else find(rest,k+1)
       in metaTyvarNames sub (find(!metaTyvars,0))
      end
  fun resetPrintType() =
      (count := ~1; metaTyvars := [])
end

val internals = System.Control.internals

fun printTyvar1(tv as (ref kind): tyvar, sign: {weakness:int, eq:bool} list) : unit =
     case kind
       of IBOUND n =>
	    (let val {weakness=w,eq=e} = nth(sign,n) 
				         handle Nth => {weakness=infinity,eq=false}
	      in if e then prstr "''" else prstr "'";
	         if w < infinity then (print w; ()) else ();
		 prstr(boundTyvarNames sub n)
	     end)
	| META{depth,weakness,eq} => 
	    (if eq then prstr "''" else prstr "'";
	     if weakness < infinity then (print weakness; ()) else ();
	     prstr(metaTyvarName tv);
	     if !internals andalso depth < infinity
	     then (prstr "["; print depth; prstr "]")
	     else ())
	| UBOUND{name,...} =>
	    (printSym name; prstr "U")
	| INSTANTIATED _ => (prstr "'"; prstr(metaTyvarName tv); prstr "I")
	    
fun printTyvar(tv: tyvar) = printTyvar1(tv,[])

fun prkind (ABStyc) = prstr "ABS/"
  | prkind (DEFtyc _) = prstr "DEF/"
  | prkind (DATAtyc _) = prstr "DATA/"
  | prkind (RECORDtyc _) = prstr "RECORD/"

fun printPath(p,stamp) =
    let fun get([id],STRstr{table,env={t,...},...}) =
	      (case !(EnvAccess.lookTYCinTable(table,id))
		of INDtyc[i] => t sub i
		 | tyc => tyc)
	  | get(id::rest,STRstr{table,env={s,...},...}) =
	      let val STRvar{binding,...} = EnvAccess.lookSTRinTable(table,id)
		  val str' = case binding
			       of INDstr i => s sub i 
				| _ => binding
	       in get(rest,str')
	      end
	  | get _ = impossible "get in printPath"
	fun try(name::untried,[]) = 
	     (let val tyc = !(EnvAccess.lookTYC(name))
	       in if TypesUtil.tycStamp tyc = stamp
		  then printSym name
		  else try(untried,[name])
	      end
	      handle Env.Unbound => try(untried,[name])
	           | Table.Notfound_Table => try(untried,[name]))
	  | try(name::untried,tried) =
	     (let val STRvar{binding,...} = EnvAccess.lookSTR name
		  val tyc = get(tried,binding)
	       in if TypesUtil.tycStamp tyc = stamp
		  then printSequence "." printSym (name::tried)
		  else try(untried,name::tried)
	      end
	      handle Env.Unbound => try(untried,name::tried)
	           | Table.Notfound_Table => try(untried,name::tried))
	  | try([],tried) = (print "?."; printSequence "." printSym tried)
     in try(p,[])
    end

fun printTycon(TYCON{kind=RECORDtyc(labels),...}) =
       printClosedSequence("{",",","}") printSym labels
  | printTycon(TYCON{kind=UNDEFtyc _,path=name::_,...}) =
      (printSym name; prstr "?")
  | printTycon(TYCON{path,stamp,kind,...}) =
      (if !internals then (prkind kind; print stamp; prstr "/") else ();
       printPath(path,stamp))
  | printTycon(INDtyc p) =
      (prstr "IND/"; printSequence "," (fn x:int =>(print x; ())) p; prstr "/")

val TYCON{stamp=arrowStamp,...} = !BasicTypes.arrowTycon

fun strength(ty) =
    case ty
      of VARty(ref(INSTANTIATED ty')) => strength(ty')
       | FLEXRECORDty(ref(CLOSED ty')) => strength(ty')
       | CONty(ref tycon, args) =>
	   (case tycon
	      of TYCON{kind=ABStyc, stamp,...} => 
		   if stamp = arrowStamp then 0 else 2
	       | TYCON{kind=RECORDtyc labels,...} =>
		   if Tuples.isTUPLEtyc(tycon) then 1 else 2
	       | _ => 2)
       | _ => 2

fun printType1(ty: ty, sign: {weakness:int,eq:bool} list) : unit =
    let fun prty ty =
	    case ty
	      of VARty(ref(INSTANTIATED ty')) => prty(ty')
	       | VARty(tv) => printTyvar1(tv,sign)
	       | CONty(ref tycon, args) =>
		   (case tycon
		      of TYCON{kind=ABStyc, stamp,...} => 
			   if stamp = arrowStamp
			   then let val [domain,range] = args
				 in if strength domain = 0
				    then (prstr "("; prty domain; prstr ")")
				    else prty domain;
				    prstr " -> ";
				    prty range
				end
			   else (printTypeArgs args; printTycon tycon)	
		       | TYCON{kind=RECORDtyc labels,...} =>
			   if Tuples.isTUPLEtyc(tycon)
			   then printTUPLEty args
			   else printRECORDty(labels, args)
		       | _ => (printTypeArgs args; printTycon tycon))
	       | POLYty{sign,tyfun=TYFUN{arity,body}} => printType1(body,sign)
	       | FLEXRECORDty(ref(CLOSED ty)) => prty ty
	       | FLEXRECORDty(ref(OPEN fields)) => printFields fields
	       | ERRORty => prstr "error"
	       | UNDEFty => prstr "undef"

	and printTypeArgs [] = ()
	  | printTypeArgs [ty] = 
	     (if strength ty <= 1
	      then (prstr "("; prty ty; prstr ")")
	      else prty ty;
	      prstr " ")
	  | printTypeArgs tys = printClosedSequence ("(", ",", ") ") prty tys

	and printTUPLEty [] = prstr "unit"
	  | printTUPLEty tys = 
	      printSequence " * " 
		(fn ty => if strength ty <= 1
			  then (prstr "("; prty ty; prstr ")")
			  else prty ty)
	        tys

	and printField(lab,arg) = (printSym lab; prstr ":"; prty arg)

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

     in prty ty
    end  (* printType1 *)

fun printType(ty: ty) : unit = printType1(ty,[])
    
end (* structure PrintType *)
