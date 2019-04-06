
signature PRINTTYPE = sig
  val typeFormals : int -> string list
  val printTycon : outstream -> Modules.env -> Types.tycon -> unit
  val printType : outstream -> Modules.env -> Types.ty -> unit
  val resetPrintType : unit -> unit
end

structure PrintType : PRINTTYPE = struct

open ErrorMsg Modules Types List2 PrintUtil

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
	    
fun prkind (kind,str :outstream) =
    output(str,case kind
	        of PRIMtyc => "PRIM"
	         | FORMtyck => "FORM"
		 | ABStyc _=> "ABS"
		 | DATAtyc _ => "DATA")

fun printPath env (p,tyc0,str:outstream) =
       output(str,
	      ModuleUtil.findPath(p,tyc0,TypesUtil.equalTycon,
				  fn (a,b) => ModuleUtil.lookTYC(env,a,b)))

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

fun printEqProp p (str :outstream) =
    let val a = case p
	of NO => "NO"
         | YES => "YES"
         | IND => "IND"
	 | OBJ => "OBJ"
	 | DATA => "DATA"
	 | UNDEF => "UNDEF"
    in output(str,a)
    end

fun printTycon (str:outstream) =
 let val say = outputc str
  in fn env =>
   fn (tyc as GENtyc{path,stamp,eq,kind,...}) =>
      (if !internals then (say "GENtyc{path=";
			   prSymPath path;
			   say ",kind=";
			   prkind (!kind,std_out);
			   say ",stamp=";
			   say (Stamps.stampToString stamp);
			   say ",eq="; printEqProp(!eq) str;
			   say "}")
       else printPath env (path,tyc,str))
   | (tyc as DEFtyc{path,tyfun=TYFUN{body,...}}) =>
      (if !internals
       then (say "DEFtyc{path="; prSymPath path;
	     print ",tyfun=TYFUN{body=";
	     printType str env body; say ",...}}")
       else printPath env (path,tyc,str))
   | (RECORDtyc labels) =>
       printClosedSequence str ("{",",","}") (printSym str) labels
   | (FORMtyc {pos,spec}) =>
      (say "FORMtyc{pos="; say (makestring pos); say ",spec=";
       printTycon str env spec; 
       say "}")
   | (RELtyc {name,pos=(p,i)}) =>
      (if !internals
         then (say "RELtyc{name=";
	       prSymPath name;
	       say ",pos=(";
	       prIntPath p;
	       print ",";
	       print i;
	       say ")}")
	 else ();
       say (formatQid name))
   | ABSFBtyc pos =>
      (say "ABSFBtyc(";
       case pos
       of PARAM p => (say "PARAM "; prIntPath p)
	| SEQ i => (say "SEQ "; print i);
       say ")")
   | NULLtyc => say "NULLtyc"
   | ERRORtyc => say "<error>"
end

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
 		       | GENtyc{kind=ref(FORMtyck), stamp,...} => 
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

and printType (str:outstream) (env:Modules.env) (ty:ty) : unit =
        printType1 env (ty,[],str)
    
end (* structure PrintType *)
