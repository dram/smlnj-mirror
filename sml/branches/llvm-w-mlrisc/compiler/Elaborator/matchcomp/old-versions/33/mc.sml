(* mc.sml *)

signature MC = sig
	structure A : BAREABSYN
	structure L : LAMBDA
	val matchCompile : (A.pat * L.lexp) list -> L.lexp
	val bindCompile : (A.pat * L.lexp) list -> L.lexp
	end

structure MC : MC = struct

structure A : BAREABSYN = BareAbsyn
structure L : LAMBDA = Lambda

open A L
open Access Basics MCopt ErrorMsg

val printDepth = System.Control.Print.printDepth

val patsUsed = ref(nil: int list)
val maybeUsed = ref(nil: int list)
val results = ref(nil: (lvar * lvar list) list)
fun mark taglist (tag : int) =
    let fun newtag tag nil = [tag]
	  | newtag (tag : int) (taglist as (t::more)) =
		if tag = t then taglist
		else if tag < t then tag :: taglist
		else t :: newtag tag more
    in  taglist := newtag tag (!taglist)
     end
fun unused rules =
    let fun find nil nil _ = nil
	  | find (rule::rules) nil i = i :: find rules nil (i+1)
	  | find (rule::rules) (taglist as (tag::tags)) i =
		if tag = i then find rules tags (i+1)
		else i :: find rules taglist (i+1)
	  | find _ _ _ = ErrorMsg.impossible "unused in mc"
    in  find rules (!patsUsed) 0
    end
fun redundant rules =
	if length rules = length (!patsUsed) then nil
	else unused rules
fun areNotRedundant () =
	case maybeUsed of
	      ref nil => ()
	    | ref (tag::_) => (mark patsUsed tag; maybeUsed := nil)
fun areRedundant () = maybeUsed := nil
datatype rhs = EXPAND of lexp * int | FCALL of (lvar * lvar) list * int
fun bind(x,v,EXPAND(result,tag)) = EXPAND(APP(FN(v,result),VAR x),tag)
  | bind(x,v,FCALL(bindings,tag)) = FCALL((v,x)::bindings,tag)
fun layer (x,CONSTRAINTpat(pat,_),rhs) = layer(x,pat,rhs)
  | layer (x,VARpat(VALvar{access=LVAR v,...}),rhs) = bind(x,v,rhs)
  | layer _ = impossible "bad layered pattern in mc"
fun convert(EXPAND(result,tag)) =
     if exists (fn i => i=tag) (!patsUsed)
      then Opt.alphaConvert result
      else (mark patsUsed tag;
	    if exists (fn i => i=tag) (!maybeUsed)
		then Opt.alphaConvert result else result)
  | convert(FCALL(bindings,tag)) =
	let val (f,free) = nth(!results,tag)
	    fun order nil = nil
	      | order (v::tl) =
		let fun f nil = ErrorMsg.impossible "convert in translate/mc.sml"
		      | f ((w,z)::tl) = if v=w then z else f tl
		in  f bindings :: order tl
		end
	    val args = order free
	in  mark patsUsed tag; APP(VAR f,RECORD(map VAR args))
	end
fun convertDefault(EXPAND(result,tag)) = 
     if exists (fn i => i=tag) (!maybeUsed)
      then Opt.alphaConvert result
      else (mark maybeUsed tag;
	    if exists (fn i => i=tag) (!patsUsed)
		then Opt.alphaConvert result else result)
  | convertDefault(FCALL(bindings,tag)) =
	let val (f,free) = nth(!results,tag)
	    fun order nil = nil
	      | order (v::tl) =
		let fun f nil = ErrorMsg.impossible "convertD in translate/mc.sml"
		      | f ((w,z)::tl) = if v=w then z else f tl
		in  f bindings :: order tl
		end
	    val args = order free
	in  mark maybeUsed tag; APP(VAR f,RECORD(map VAR args))
	end
fun freevars pat =
  let val rec f =
	fn WILDpat => nil
	 | VARpat(VALvar{access=LVAR v,...}) => [v]
	 | INTpat _ => nil
	 | REALpat _ => nil
	 | STRINGpat _ => nil
	 | CONpat _ => nil
	 | RECORDpat{pats=ref pats,...} =>
		fold (fn(a,b) => SortedList.merge(f a,b)) pats nil
	 | APPpat(_,pat) => f pat
	 | CONSTRAINTpat(pat,_) => f pat
	 | LAYEREDpat(l,r) => SortedList.merge(f l,f r)
	 | _ => ErrorMsg.impossible "freevars in translate/mc.sml"
  in  f pat
  end

fun setup rules =
  let fun tag(i,(pat,result)::more) =
		(pat,EXPAND(result,i)) :: tag(i+1,more)
	| tag _ = nil
      val arg = mkLvar()
      fun t(i,(pat,result)::more) =
	let val (header,r) = t(i+1,more)
	    val record = mkLvar()
	    val returnit = mkLvar()
	    val free = freevars pat
	    fun f(_,nil) = result
	      | f(i,v::tl) = APP(FN(v,f(i+1,tl)),SELECT(i,VAR record))
	in  results := (returnit,free) :: !results;
	    (header o (fn l => APP(FN(returnit,l),FN(record,f (0,free)))),
	    (pat,FCALL(nil,i)) :: r)
	end
	| t _ = (fn x => x,nil)
  in  patsUsed := nil; maybeUsed := nil;
      if !System.Control.MC.expandResult then (fn x => x,tag(0,rules))
      else let val (header,r) = t(0,rules)
	   in  (fn l => FN(arg,header(APP(l,VAR arg))),r)
(* match compiler must always return a FN *)
	   end
  end

fun bindfields(record,fields,e)=
  let fun select(i, nil) = e
        | select(i, x::xs) = APP(FN(x,select(i+1,xs)),SELECT(i,VAR record))
  in  select(0,fields)
  end

fun andSwitch x =
let
fun andS nil = (nil,nil)
  | andS ((p::fields,rhs)::more) =
     (case p of
       INTpat i =>
	let val (cases,default) = andS more
	    fun addto ((switch as (INTcon j,pats))::more) =
		    if i = j then ((INTcon i,(fields,rhs)::pats)::more)
		    else switch :: addto more
	      | addto nil = [(INTcon i,(fields,rhs)::default)]
	      | addto _ = impossible "983 type error in match compiler"
	in  (addto cases,default)
	end
     | REALpat r =>
	let val (cases,default) = andS more
	fun addto ((switch as (REALcon s,pats))::more) =
		if r = s then ((REALcon r,(fields,rhs)::pats)::more)
		else switch :: addto more
	  | addto nil = [(REALcon r,(fields,rhs)::default)]
	  | addto _ = impossible "48 type error in match compiler"
	in  (addto cases,default)
	end
     | STRINGpat s =>
	let val (cases,default) = andS more
	    fun addto ((switch as (STRINGcon t,pats))::more) =
		  if s = t then ((STRINGcon s,(fields,rhs)::pats)::more)
		  else switch :: addto more
	      | addto nil = [(STRINGcon s,(fields,rhs)::default)]
	      | addto _ = impossible "482 type error in match compiler"
	in  (addto cases,default)
	end
     | CONpat(dcon as DATACON{name=r1,...}) =>
	let val (cases,default) = andS more
	  fun addto ((switch as (DATAcon(DATACON {name=r2,...}),pats))::more) =
		  if Symbol.eq(r1,r2)
		    then (DATAcon dcon,(fields,rhs)::pats)::more
		    else switch :: addto more
	    | addto nil = [(DATAcon dcon,(fields,rhs)::default)]
	    | addto _ = impossible "87 type error in match compiler"
	in  (addto cases,default)
	end
     | APPpat(dcon as DATACON{name=r1,...},p) =>
	let val (cases,default) = andS more
	fun addto ((switch as (DATAcon(DATACON {name=r2,...}),pats))::more) =
		if Symbol.eq(r1,r2)
		then ((DATAcon dcon,(p::fields,rhs)::pats)::more)
		else switch :: addto more
	  | addto nil =
		let fun addwild (fields,rhs) = (WILDpat::fields,rhs)
		in  [(DATAcon dcon,(p::fields,rhs)::(map addwild default))]
		end
	  | addto _ = impossible "444 type error in match compiler"
	in  (addto cases,default)
	end
     | WILDpat =>
	let val (cases,default) = andS more
	fun addto (((con as DATAcon(DATACON{const=false,...})),pats)::more) =
		(con,(WILDpat::fields,rhs)::pats) :: addto more
	  | addto ((con,pats)::more) =
		(con,(fields,rhs)::pats) :: addto more
	  | addto nil = nil
	in  (addto cases,(fields,rhs)::default)
	end
     | VARpat(VALvar{access=LVAR v,...}) =>
	andS ((WILDpat::fields,bind(x,v,rhs))::more)
     | LAYEREDpat(v,p) =>
	andS ((p::fields,layer(x,v,rhs))::more)
     | CONSTRAINTpat(p,_) =>
	andS ((p::fields,rhs)::more)
     | _ => impossible "andS in mc")
  | andS _ = impossible "andS2 in mc"
in  andS
end

fun orSwitch x =
let fun diffPats samefn =
	let fun diff nil = nil
	      | diff ((hd as (p,rhs))::more) =
		 case p of
		    WILDpat => hd::nil
		  | VARpat(VALvar{access=LVAR v,...}) =>
			(WILDpat,bind(x,v,rhs))::nil
		  | LAYEREDpat(v,p) =>
			diff ((p,layer(x,v,rhs))::more)
		  | CONSTRAINTpat(p,_) =>
			diff ((p,rhs)::more)
		  | _ =>
			(if samefn p then diff more
			 else hd::diff more)
			handle Match =>
	      		impossible "orS.diff: type error in match compiler"
	in  diff
	end
fun orS nil =  impossible "orSwitch nil in mc"
  | orS (arg as (p,rhs)::more) =
     case p of
      INTpat i =>
	let val (cases,default) = orS (diffPats (fn INTpat j => i=j) arg)
	in  ((INTcon i,convert rhs)::cases,default)
	end
    | REALpat r =>
	let val (cases,default) = orS (diffPats (fn REALpat s => r=s) arg)
	in  ((REALcon r,convert rhs)::cases,default)
	end
    | STRINGpat s =>
	let val (cases,default) = orS (diffPats (fn STRINGpat t => s=t) arg)
	in  ((STRINGcon s,convert rhs)::cases,default)
	end
    | WILDpat => (nil,SOME(convert rhs))
    | VARpat(VALvar{access=LVAR v,...}) =>
			(nil,SOME(convert(bind(x,v,rhs))))
    | CONSTRAINTpat(p,_) => orS ((p,rhs)::more)
    | LAYEREDpat(v,p) => orS ((p,layer(x,v,rhs))::more)
    | _ => impossible "orS in mc"
in  orS
end

fun mcand (arg as (([_],_)::_),[x]) =
	let val singlelist = fn ([pat],rhs) => (pat,rhs)
			      | _ => impossible "singlelist in match compiler"
	in  APP(mcor (map singlelist arg), VAR x)
	end
  | mcand (arg as (p::fields,rhs)::more,xl as x::xs) =
    let fun mconto (con as DATAcon(con1 as DATACON{const = false,...}),pats) =
	     let val new = mkLvar ()
	     in  (con,APP(FN(new,mcand (opt (pats,new::xs))),DECON (con1,VAR x)))
	     end
	  | mconto (con as DATAcon(DATACON {const = true,...}),pats) =
	     (con,mcand (opt (pats,xs)))
	  | mconto _ = impossible "mconto in mc"
    in
     case p of
       WILDpat =>
	mcand ((fields,rhs)::nil,xs)
     | VARpat(VALvar{access=LVAR v,...}) =>
	mcand ((fields,bind(x,v,rhs))::nil,xs)
     | LAYEREDpat(v,p) =>
	mcand (((p::fields,layer(x,v,rhs))::more),xl)
     | CONSTRAINTpat(p,_) =>
	mcand ((p::fields,rhs)::more,xl)
     | APPpat(DATACON{sign = [_],...},_) =>
	let val newx = mkLvar()
	    val ([(DATAcon dcon,list)],_) = andSwitch x arg
	in  APP(FN(newx,mcand(opt(list,newx::xs))),DECON(dcon,VAR x))
	end
     | APPpat(DATACON{sign,...},_) =>
	let val (cases,default) = andSwitch x arg
	in  SWITCH(VAR x,
		   map mconto cases,
		   if length cases = length sign then NONE
		       else SOME (mcand (opt (default,xs))))
	end
     | CONpat(DATACON{sign=[_],...}) =>
	mcand ((fields,rhs)::nil,xs)
     | CONpat(DATACON{sign,...}) =>
	let val (cases,default) = andSwitch x arg
	in  SWITCH(VAR x,
		   map mconto cases,
		   if length cases = length sign then NONE
		       else SOME (mcand (opt (default,xs))))
	end
     | RECORDpat{pats=ref nil,...} =>
	    mcand ((fields,rhs)::nil,xs)
     | RECORDpat{pats,...} =>
	let val newfields = map (fn _ => mkLvar()) (!pats)
	    val wild = map (fn _ => WILDpat) newfields
	    fun expand nil = nil
	      | expand ((p::fields,rhs)::more) =
		 (case p of
		   RECORDpat{pats,...} =>
			(!pats@fields,rhs) :: expand more
	         | LAYEREDpat(v,p) =>
			expand ((p::fields,layer(x,v,rhs))::more)
	         | CONSTRAINTpat(p,_) =>
			expand ((p::fields,rhs)::more)
	         | WILDpat =>
			(wild@fields,rhs) :: expand more
	         | VARpat(VALvar{access=LVAR v,...}) =>
			    (wild@fields,bind(x,v,rhs)) :: expand more
		 |  _ => impossible "mcand.expand in mc")
	      | expand _ = impossible "mcand.expand2 in mc"
	    in  bindfields(x,newfields,mcand(opt(expand arg,newfields@xs)))
	    end
     | _ => (* INTpat,REALpat,STRINGpat; possibly bad VARpats *)
	let val (cases,default) = andSwitch x arg
	in  SWITCH(VAR x,
		   map (fn (con,pats) => (con,mcand(opt(pats,xs)))) cases,
		   SOME(mcand(opt(default,xs))))
	end
    end
  | mcand _ = impossible "mcand in mc"

and conSwitch x =
let
fun conS nil = (nil,NONE)
  | conS (arg as (p,rhs)::more) =
     case p of
       CONpat(dcon as DATACON{name=r1,...}) =>
	let fun diff nil = nil
	      | diff ((hd as (p,rhs))::more) =
		 case p of
		  CONpat(DATACON{name=r2,...}) =>
			if Symbol.eq(r1,r2) then diff more
			else (hd::diff more)
		| APPpat (_,_) => hd::diff more
		| WILDpat => hd::nil
		| VARpat _ => hd::nil
		| CONSTRAINTpat(p,_) =>
			diff ((p,rhs)::more)
		| LAYEREDpat(v,p) =>
			diff ((p,layer(x,v,rhs))::more)
		| _ => impossible "conS.diff: type error in match compiler"
	    val (cases,default) = conS (diff more)
	in  ((DATAcon dcon,convert rhs)::cases,default)
	end
     | APPpat(dcon as DATACON{name=r1,...},_) =>
	let fun divide nil = (nil,nil)
	      | divide ((hd as (p,rhs))::more) =
		case p of
		  CONpat _ =>
		      let val (same,diff) = divide more
		      in  (same,hd::diff)
		      end
		| APPpat(DATACON{name=r2,...},p) =>
		      let val (same,diff) = divide more 
		      in  if Symbol.eq(r1,r2)
			  then ((p,rhs)::same,diff)
			  else (same,hd::diff)
		      end
		| WILDpat => (hd::nil,hd::nil)
		| VARpat(VALvar{access=LVAR v,...}) =>
			((WILDpat,bind(x,v,rhs))::nil,hd::nil)
		| CONSTRAINTpat(p,_) =>
			divide ((p,rhs)::more)
		| LAYEREDpat(v,p) =>
			divide ((p,layer(x,v,rhs))::more)
		| _ => impossible "conS.divide: type error in match compiler"
	    val con = DATAcon dcon
            val (same,diff) = divide arg
	    val lexp = mcor same	    (* Order imp. here:  side- *)
	    val (cases,default) = conS diff (* effects in redund. chk. *)
        in  ((con,APP(lexp,DECON(dcon,VAR x)))::cases,default)
        end
     | WILDpat => (nil,SOME(convertDefault rhs))
     | VARpat(VALvar{access=LVAR v,...}) =>
			(nil,SOME(convertDefault(bind(x,v,rhs))))
     | LAYEREDpat(v,p) => conS ((p,layer(x,v,rhs))::more)
     | CONSTRAINTpat(p,_) => conS ((p,rhs)::more)
     | _ => impossible "conS: type error in match compiler"
in  conS
end

and mcor nil = impossible "mcor.nil in mc"
  | mcor (arg as (p,rhs)::more) =
      let val x = mkLvar()
      in  case p of
	      CONpat(DATACON{sign=[],...}) => (* exception *)
		let val (cases,default) = conSwitch x arg
		in  areNotRedundant();
		    FN(x,SWITCH(VAR x,cases,default))
		end
	    | APPpat (DATACON{sign=[],...},_) => (* exn *)
		let val (cases,default) = conSwitch x arg
		in  areNotRedundant();
		    FN(x,SWITCH(VAR x,cases,default))
		end
	    | CONpat(DATACON{sign=[_],...}) =>
		FN(x, convert rhs)
	    | CONpat(DATACON{sign,...}) =>
		let val (cases,default) = conSwitch x arg
		in  FN(x,SWITCH(VAR x, cases,
		        (if length cases = length sign
			   then (areRedundant(); NONE)
			   else (areNotRedundant(); default))))
		end
	    | APPpat(DATACON{sign=[_],...},_) =>
		let val ([(con,lexp)],_) = conSwitch x arg
		in  areRedundant();
		    FN(x,lexp)
		end
	    | APPpat(DATACON{sign,...},_) =>
		let val (cases,default) = conSwitch x arg
		in  FN(x,SWITCH(VAR x, cases,
		       (if length cases = length sign
			then (areRedundant(); NONE)
			else (areNotRedundant(); default))))
		end
	    | INTpat _ =>
		let val (cases,default) = orSwitch x arg
		in  FN(x,SWITCH(VAR x,cases,default))
		end
	    | REALpat _ =>
		let val (cases,default) = orSwitch x arg
		in  FN(x,SWITCH(VAR x,cases,default))
		end
	    | STRINGpat _ =>
		let val (cases,default) = orSwitch x arg
		in  FN(x,SWITCH(VAR x,cases,default))
		end
	    | RECORDpat{pats=ref nil,...} =>
		FN(x, convert rhs)
	    | RECORDpat{pats,...} =>
		let val newfields = map (fn _ => mkLvar()) (!pats)
		    val wild = map (fn _ => WILDpat) newfields
		    fun expand nil = nil
		      | expand ((p,rhs)::more) =
			 case p of
			  RECORDpat{pats,...} =>
			  	(!pats,rhs) :: expand more
			| LAYEREDpat(v,p) =>
			  	expand ((p,layer(x,v,rhs))::more)
			| CONSTRAINTpat(p,_) =>
			  	expand ((p,rhs)::more)
			| WILDpat =>
			  	(wild,rhs)::nil
			| VARpat(VALvar{access=LVAR v,...}) =>
			  	(wild,bind(x,v,rhs))::nil
			| _ => impossible "mcor.expand in mc"
		in  FN(x,bindfields(x,newfields,mcand(opt(expand arg,newfields))))
		end
	    | WILDpat =>
		FN(x, convert rhs)
	    | VARpat(VALvar{access=LVAR v,...}) =>
		 FN(x,convert(bind(x,v,rhs)))
	    | LAYEREDpat(v,p) =>
		FN(x,APP(mcor((p,layer(x,v,rhs))::more),VAR x))
	    | CONSTRAINTpat(p,_) =>
		mcor ((p,rhs)::more)
	    | _ => impossible "mcor: type error in match compiler"
      end (* fun mcor *)

open PrintUtil
fun matchPrint nil _ _ = ()
  | matchPrint [(pat,_)] _ _ = () (* never print last rule *)
  | matchPrint ((pat,_)::more) nil _ =
	(print "        "; PrintAbsyn.printPat(pat,!printDepth); print " => ...\n";
	 matchPrint more nil 0)
  | matchPrint ((pat,_)::more) (taglist as (tag::tags)) i =
	if i = tag
	then (print "  -->   "; PrintAbsyn.printPat(pat,!printDepth);
	      print " => ...\n"; matchPrint more tags (i+1))
	else (print "        "; PrintAbsyn.printPat(pat,!printDepth);
	      print " => ...\n"; matchPrint more taglist (i+1))

fun bindPrint ((pat,_)::_) =
      (print "        "; PrintAbsyn.printPat(pat,!printDepth); print " = ...\n")
  | bindPrint _ = impossible "bindPrint in mc"

fun noVarsIn ((pat,_)::_) =
	let fun var WILDpat = true (* might want to flag this *)
	      | var (VARpat _) = true
	      | var (LAYEREDpat _) = true
	      | var (CONSTRAINTpat(p,_)) = var p
	      | var (APPpat(_,p)) = var p
	      | var (RECORDpat{pats=ref patlist,...}) = exists var patlist
	      | var _ = false
	in  not(var pat)
	end
  | noVarsIn _ = impossible "noVarsIn in mc"

open System.Control.MC
fun bindCompile rules = 
let val (header,rules') = setup rules
    val match = header(mcor rules')
    val unused = redundant rules
    val last = length rules - 1
    val printit = if !bindExhaustive andalso not(exists (fn i => i=last) unused)
		  then (warn "binding not exhaustive"; true)
		  else false
    val printit = if !bindContainsVar andalso noVarsIn rules
		  then (warn "binding contains no variables"; true)
		  else printit
in  if !printArgs
	then (warn "MC called with:"; MCprint.printMatch rules)
	else ();
    if printit
	then bindPrint rules
	else ();
    if !printRet
	then (print "MC:  returns with\n"; MCprint.printLexp match; newline())
	else ();
    match
end handle Syntax => (warn "MC called with:"; MCprint.printMatch rules;
		      raise Syntax)
  
fun matchCompile rules = 
let val (header,rules') = setup rules
    val match = header(mcor rules')
    val unused = redundant rules
    val last = length rules - 1
    val printit = if !matchExhaustive andalso not(exists (fn i => i=last) unused)
		  then (warn "match not exhaustive"; true)
		  else false
    val printit = if exists (fn i => i<last) unused andalso !matchRedundant
        	  then (warn "redundant patterns in match"; true)
		  else printit
in  if !printArgs
	then (warn "MC called with:"; MCprint.printMatch rules)
	else ();
    if printit
	then matchPrint rules unused 0
	else ();
    if !printRet
	then (print "MC:  returns with\n"; MCprint.printLexp match; newline())
	else ();
    match
end handle Syntax => (warn "MC called with:"; MCprint.printMatch rules;
		      raise Syntax)
  

end (* struct MC *)
