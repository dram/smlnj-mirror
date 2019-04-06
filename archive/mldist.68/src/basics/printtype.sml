
signature PRINTTYPE = sig
  val typeFormals : int -> string list
  val printTycon : outstream -> Basics.env -> Basics.tycon -> unit
  val printType : outstream -> Basics.env -> Basics.ty -> unit
  val resetPrintType : unit -> unit
end

structure PrintType : PRINTTYPE = struct

open ErrorMsg Basics List2 PrintUtil

fun boundTyvarName k =
    if k < 26 then chr(k+ord"a")
	      else chr(k quot 26 + ord "a") ^ chr(k rem 26 + ord"a")

fun metaTyvarName' k =
    if k < 26 then chr(25-k+ord "A")
	      else chr(25-(k quot 26) + ord "A") ^ chr(25-(k rem 26) + ord"A")

fun typeFormals n =
    let fun loop i =
	if i>=n then []
	else (boundTyvarName i)::loop(i+1)
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
       in metaTyvarName' (find(!metaTyvars,0))
      end
  fun resetPrintType() =
      (count := ~1; metaTyvars := [])
end

val internals = System.Control.internals

fun printSym (str :outstream) (s: Symbol.symbol) =
  outputc str (Symbol.name s);

fun printSequence str sep pr elems =
  let fun prElems [el] = pr el
	| prElems (el::rest) = (pr el; outputc str sep; prElems rest)
	| prElems [] = ()
   in prElems elems
  end

fun printClosedSequence str (front: string, sep, back:string) pr elems =
  (outputc str front; printSequence str sep pr elems; outputc str back)

fun outint (str :outstream) (i :int) = outputc str (makestring i)

fun printTyvar(tv as (ref kind): tyvar, sign: {weakness:int, eq:bool} list, str :outstream) : unit =
  let val print = outputc str in
     case kind
       of IBOUND n =>
	    (let val {weakness=w,eq=e} = nth(sign,n) 
				         handle Nth => {weakness=infinity,eq=false}
	      in if e then print "''" else print "'";
	         if w < infinity
		 then if !System.Control.weakUnderscore
		      then print "_"
		      else print (makestring w)
		 else ();
		 print(boundTyvarName n)
	     end)
	| META{depth,weakness,eq} => 
	    (if eq then print "''" else print "'";
	     if weakness < infinity then print (makestring weakness) else ();
	     print(metaTyvarName tv);
	     if !internals andalso depth < infinity
	     then (print "["; print (makestring depth); print "]")
	     else ())
	| UBOUND{name,...} =>
	    (printSym str name; print "U")
	| INSTANTIATED _ => (print "'"; print(metaTyvarName tv); print "I")
  end
	    
fun prkind (PRIMtyc) (str :outstream) = outputc str "PRIM/"
  | prkind (FORMtyc) (str :outstream) = outputc str "FORM/"
  | prkind (ABStyc _) (str :outstream) = outputc str "ABS/"
  | prkind (DATAtyc _) (str :outstream) = outputc str "DATA/"

fun printPath env (p,tyc0,str:outstream) =
    let fun get([id],STRstr{table,env=strenv,...}) =
	      (case EnvAccess.lookTYC table id
		of INDtyc i =>
		     (case strenv
		       of REL{t,...} => t sub i
		        | DIR => impossible "printPath.get 1")
		 | SHRtyc p => TypesUtil.getEpathTyc(p,strenv)
		 | tyc => tyc)
	  | get(id::rest,STRstr{table,env=strenv,...}) =
	      let val STRvar{binding,...} = EnvAccess.lookSTR table id
		  val str' = case (binding,strenv)
			       of (INDstr i,REL{s,...}) => s sub i 
			        | (SHRstr (i::r),REL{s,...}) =>
				    TypesUtil.getEpath(r,s sub i)
				| (STRstr _, _) => binding
				| _ => impossible "printPath.get 2"
	       in get(rest,str')
	      end
	  | get _ = impossible "get in printPath"
	fun try(name::untried,[]) = 
	     ((if TypesUtil.equalTycon(EnvAccess.lookTYC env name,tyc0)
		  then printSym str name
		  else try(untried,[name]))
	      handle Env.Unbound => try(untried,[name]))
	  | try(name::untried,tried) =
	     (let val STRvar{binding,...} = EnvAccess.lookSTR env name
	       in if TypesUtil.equalTycon(get(tried,binding),tyc0)
		  then printSequence str "." (printSym str) (name::tried)
		  else try(untried,name::tried)
	      end
	      handle Env.Unbound => try(untried,name::tried))
	  | try([],tried) = (outputc str "?."; 
			     printSequence str "." (printSym str) tried)
     in try(p,[])
    end
    handle e => (print "Compiler Bug: exception escapes printPath\n"; raise e)

val GENtyc{stamp=arrowStamp,...} = BasicTypes.arrowTycon

fun strength(ty) =
    case ty
      of VARty(ref(INSTANTIATED ty')) => strength(ty')
       | FLEXRECORDty(ref(CLOSED ty')) => strength(ty')
       | CONty(tycon, args) =>
	   (case tycon
 	      of GENtyc{kind=ref(PRIMtyc), stamp,...} => 
		   if stamp = arrowStamp then 0 else 2
	       | RECORDtyc labels =>
		   if Tuples.isTUPLEtyc(tycon) then 1 else 2
	       | _ => 2)
       | _ => 2

fun printEqProp YES (str :outstream) = outputc str "YES"
  | printEqProp NO (str :outstream) = outputc str "NO"
  | printEqProp IND (str :outstream) = outputc str "IND"
  | printEqProp OBJ (str :outstream) = outputc str "OBJ"
  | printEqProp DATA (str :outstream) = outputc str "DATA"
  | printEqProp UNDEF (str :outstream) = outputc str "UNDEF"

fun printTycon (str:outstream) env =
  fn (RECORDtyc labels) =>
       printClosedSequence str ("{",",","}") (printSym str) labels
   | (tyc as DEFtyc{path,tyfun=TYFUN{body,...},...}) =>
      (if !internals
       then (outputc str "DEF["; printType str env body; outputc str "]")
       else ();
       printPath env (path,tyc,str))
   | (tyc as GENtyc{path,stamp,eq,kind,...}) =>
      (if !internals then (prkind (!kind); outputc str (makestring stamp); 
			   outputc str "/"; printEqProp(!eq) str; outputc str "/") else ();
       printPath env (path,tyc,str))
   | (INDtyc i) =>
      (outputc str "IND/"; outputc str (makestring i); outputc str "/")
   | (SHRtyc p) =>
	 (outputc str "SHR/"; printSequence str "," (outputc str o makestring) p; outputc str "/")
   | (RELtyc p) =>
      (outputc str "REL/"; printSequence str "," (outputc str o makestring) p; outputc str "/")
   | NULLtyc => outputc str "NULLtyc"

and printType1 env (ty: ty, sign: {weakness:int,eq:bool} list, str:outstream) : unit =
    let val print = outputc str
	fun prty ty =
	    case ty
	      of VARty(ref(INSTANTIATED ty')) => prty(ty')
	       | VARty(tv) => printTyvar(tv,sign,str)
	       | CONty(tycon, args) =>
		   (case tycon
		      of GENtyc{kind=ref(PRIMtyc), stamp,...} => 
			   if stamp = arrowStamp
			   then let val [domain,range] = args
				 in if strength domain = 0
				    then (print "("; prty domain; print ")")
				    else prty domain;
				    print " -> ";
				    prty range
				end
			   else (printTypeArgs args; printTycon str env tycon)
 		       | GENtyc{kind=ref(FORMtyc), stamp,...} => 
 			   (printTypeArgs args; printTycon str env tycon)	
 		       | GENtyc{kind=ref(ABStyc _), stamp,...} => 
 			   (printTypeArgs args; printTycon str env tycon)	
		       | RECORDtyc labels =>
			   if Tuples.isTUPLEtyc(tycon)
			   then printTUPLEty args
			   else printRECORDty(labels, args)
		       | _ => (printTypeArgs args; printTycon str env tycon))
	       | POLYty{sign,abs,tyfun=TYFUN{arity,body}} => printType1 env (body,sign,str)
	       | FLEXRECORDty(ref(CLOSED ty)) => prty ty
	       | FLEXRECORDty(ref(OPEN fields)) => printFields fields
	       | ERRORty => print "error"
	       | UNDEFty => print "undef"

	and printTypeArgs [] = ()
	  | printTypeArgs [ty] = 
	     (if strength ty <= 1
	      then (print "("; prty ty; print ")")
	      else prty ty;
	      print " ")
	  | printTypeArgs tys =
	      printClosedSequence str ("(", ",", ") ") prty tys

	and printTUPLEty [] = print "unit"
	  | printTUPLEty tys = 
	      printSequence str " * " 
		(fn ty => if strength ty <= 1
			  then (print "("; prty ty; print ")")
			  else prty ty)
	        tys

	and printField(lab,arg) = (printSym str lab; print ":"; prty arg)

	and printRECORDty([],[]) = print "unit"
	  | printRECORDty(lab::labels, arg::args) =
	      (print "{";
	       printField(lab,arg);
	       app2 
		 (fn field => (print ","; printField field))
		 (labels,args);
	       print "}")
	  | printRECORDty _ = ErrorMsg.impossible "PrintType.printRECORDty"

	and printFields [] = print "{...}"	
	  | printFields (field::fields) =
	      (print "{";
	       printField field;
	       app 
		 (fn field => (print ","; printField field))
		 fields;
	       print ",...}");

     in prty ty
    end  (* printType1 *)

and printType (str:outstream) (env:Basics.env) (ty:ty) : unit = printType1 env (ty,[],str)
    
end (* structure PrintType *)
