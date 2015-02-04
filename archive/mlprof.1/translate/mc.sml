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
open Access Basics MCopt PrintUtil PrintBasics PrintAbsyn MCprint ErrorMsg

val printDepth = System.Control.Print.printDepth

local
   val patsUsed = ref nil
   val maybeUsed = ref nil
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

in
   fun resetRedundant () = (patsUsed := nil; maybeUsed := nil)
   fun convert(result,tag) =
     if exists (fn i => i=tag) (!patsUsed)
      then Opt.alphaConvert result
      else (mark patsUsed tag;
	    if exists (fn i => i=tag) (!maybeUsed)
		then Opt.alphaConvert result else result)
   fun redundant rules =
	if length rules = length (!patsUsed) then nil
	else unused rules
   fun convertDefault(result,tag) = 
     if exists (fn i => i=tag) (!maybeUsed)
      then Opt.alphaConvert result
      else (mark maybeUsed tag;
	    if exists (fn i => i=tag) (!patsUsed)
		then Opt.alphaConvert result else result)
   fun areNotRedundant () =
	case maybeUsed of
	      ref nil => ()
	    | ref (tag::_) => (mark patsUsed tag; maybeUsed := nil)
   fun areRedundant () = maybeUsed := nil
end

fun bind(x,v,result) = APP(FN(v,result),VAR x)

fun layer (x,CONSTRAINTpat(pat,_),result) = layer(x,pat,result)
  | layer (x,VARpat(VALvar{access=LVAR v,...}),result) = bind(x,v,result)
  | layer _ = impossible "bad layered pattern in mc"

fun bindfields(record,fields,e)=
    let fun select (i, nil) = e
          | select (i, x::xs) = APP(FN(x,select(i+1,xs)),SELECT(i,VAR record))
    in  select (0,fields)
    end

val untag = map (fn (dcon,lexp,tag) => (dcon,lexp))

val addtag =
	let fun tag i ((pat,result)::more) = (pat,result,i) :: tag (i+1) more
	      | tag _ nil = nil
	in  tag 0
	end

fun andSwitch x =
let
fun andS nil = (nil,nil)
  | andS ((p::fields,result,tag)::more) =
     (case p of
       INTpat i =>
	let val (cases,default) = andS more
	    fun addto ((switch as (INTcon j,pats))::more) =
		    if i = j then ((INTcon i,(fields,result,tag)::pats)::more)
		    else switch :: addto more
	      | addto nil = [(INTcon i,(fields,result,tag)::default)]
	      | addto _ = impossible "983 type error in match compiler"
	in  (addto cases,default)
	end
     | REALpat r =>
	let val (cases,default) = andS more
	fun addto ((switch as (REALcon s,pats))::more) =
		if r = s then ((REALcon r,(fields,result,tag)::pats)::more)
		else switch :: addto more
	  | addto nil = [(REALcon r,(fields,result,tag)::default)]
	  | addto _ = impossible "48 type error in match compiler"
	in  (addto cases,default)
	end
     | STRINGpat s =>
	let val (cases,default) = andS more
	    fun addto ((switch as (STRINGcon t,pats))::more) =
		  if s = t then ((STRINGcon s,(fields,result,tag)::pats)::more)
		  else switch :: addto more
	      | addto nil = [(STRINGcon s,(fields,result,tag)::default)]
	      | addto _ = impossible "482 type error in match compiler"
	in  (addto cases,default)
	end
     | CONpat(dcon as DATACON{name=r1,...}) =>
	let val (cases,default) = andS more
	  fun addto ((switch as (DATAcon(DATACON {name=r2,...}),pats))::more) =
		  if Symbol.eq(r1,r2)
		    then (DATAcon dcon,(fields,result,tag)::pats)::more
		    else switch :: addto more
	    | addto nil = [(DATAcon dcon,(fields,result,tag)::default)]
	    | addto _ = impossible "87 type error in match compiler"
	in  (addto cases,default)
	end
     | APPpat(dcon as DATACON{name=r1,...},p) =>
	let val (cases,default) = andS more
	fun addto ((switch as (DATAcon(DATACON {name=r2,...}),pats))::more) =
		if Symbol.eq(r1,r2)
		then ((DATAcon dcon,(p::fields,result,tag)::pats)::more)
		else switch :: addto more
	  | addto nil =
		let fun addwild (fields,result,tag) = (WILDpat::fields,result,tag)
		in  [(DATAcon dcon,(p::fields,result,tag)::(map addwild default))]
		end
	  | addto _ = impossible "444 type error in match compiler"
	in  (addto cases,default)
	end
     | WILDpat =>
	let val (cases,default) = andS more
	fun addto (((con as DATAcon(DATACON{const=false,...})),pats)::more) =
		(con,(WILDpat::fields,result,tag)::pats) :: addto more
	  | addto ((con,pats)::more) =
		(con,(fields,result,tag)::pats) :: addto more
	  | addto nil = nil
	in  (addto cases,(fields,result,tag)::default)
	end
     | VARpat(VALvar{access=LVAR v,...}) =>
	andS ((WILDpat::fields,bind(x,v,result),tag)::more)
     | LAYEREDpat(v,p) =>
	andS ((p::fields,layer(x,v,result),tag)::more)
     | CONSTRAINTpat(p,_) =>
	andS ((p::fields,result,tag)::more)
     | _ => impossible "andS in mc")
  | andS _ = impossible "andS2 in mc"
in  andS
end

fun orSwitch x =
let fun diffPats samefn =
	let fun diff nil = nil
	      | diff ((hd as (p,result,tag))::more) =
		 case p of
		    WILDpat => hd::nil
		  | VARpat(VALvar{access=LVAR v,...}) =>
			(WILDpat,bind(x,v,result),tag)::nil
		  | LAYEREDpat(v,p) =>
			diff ((p,layer(x,v,result),tag)::more)
		  | CONSTRAINTpat(p,_) =>
			diff ((p,result,tag)::more)
		  | _ =>
			(if samefn p then diff more
			 else hd::diff more)
			handle Match =>
	      		impossible "orS.diff: type error in match compiler"
	in  diff
	end
fun orS nil =  impossible "orSwitch nil in mc"
  | orS (arg as (p,result,tag)::more) =
     case p of
      INTpat i =>
	let val (cases,default) = orS (diffPats (fn INTpat j => i=j) arg)
	in  ((INTcon i,convert(result,tag),tag)::cases,default)
	end
    | REALpat r =>
	let val (cases,default) = orS (diffPats (fn REALpat s => r=s) arg)
	in  ((REALcon r,convert(result,tag),tag)::cases,default)
	end
    | STRINGpat s =>
	let val (cases,default) = orS (diffPats (fn STRINGpat t => s=t) arg)
	in  ((STRINGcon s,convert(result,tag),tag)::cases,default)
	end
    | WILDpat => (nil,SOME(convert(result,tag)))
    | VARpat(VALvar{access=LVAR v,...}) =>
			(nil,SOME(convert(bind(x,v,result),tag)))
    | CONSTRAINTpat(p,_) => orS ((p,result,tag)::more)
    | LAYEREDpat(v,p) => orS ((p,layer(x,v,result),tag)::more)
    | _ => impossible "orS in mc"
in  orS
end

fun mcand (arg as (([_],_,_)::_),[x]) =
	let val singlelist = fn ([pat],result,tag) => (pat,result,tag)
			      | _ => impossible "singlelist in match compiler"
	in  APP(mcor (map singlelist arg), VAR x)
	end
  | mcand (arg as (p::fields,result,tag)::more,xl as x::xs) =
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
	mcand ((fields,result,tag)::nil,xs)
     | VARpat(VALvar{access=LVAR v,...}) =>
	mcand ((fields,bind(x,v,result),tag)::nil,xs)
     | LAYEREDpat(v,p) =>
	mcand (((p::fields,layer(x,v,result),tag)::more),xl)
     | CONSTRAINTpat(p,_) =>
	mcand ((p::fields,result,tag)::more,xl)
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
	mcand ((fields,result,tag)::nil,xs)
     | CONpat(DATACON{sign,...}) =>
	let val (cases,default) = andSwitch x arg
	in  SWITCH(VAR x,
		   map mconto cases,
		   if length cases = length sign then NONE
		       else SOME (mcand (opt (default,xs))))
	end
     | RECORDpat{pats=ref nil,...} =>
	    mcand ((fields,result,tag)::nil,xs)
     | RECORDpat{pats,...} =>
	let val newfields = map (fn _ => mkLvar()) (!pats)
	    val wild = map (fn _ => WILDpat) newfields
	    fun expand nil = nil
	      | expand ((p::fields,result,tag)::more) =
		 (case p of
		   RECORDpat{pats,...} =>
			(!pats@fields,result,tag) :: expand more
	         | LAYEREDpat(v,p) =>
			expand ((p::fields,layer(x,v,result),tag)::more)
	         | CONSTRAINTpat(p,_) =>
			expand ((p::fields,result,tag)::more)
	         | WILDpat =>
			(wild@fields,result,tag) :: expand more
	         | VARpat(VALvar{access=LVAR v,...}) =>
			    (wild@fields,bind(x,v,result),tag) :: expand more
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
  | conS (arg as (p,result,tag)::more) =
     case p of
       CONpat(dcon as DATACON{name=r1,...}) =>
	let fun diff nil = nil
	      | diff ((hd as (p,result,tag))::more) =
		 case p of
		  CONpat(DATACON{name=r2,...}) =>
			if Symbol.eq(r1,r2) then diff more
			else (hd::diff more)
		| APPpat (_,_) => hd::diff more
		| WILDpat => hd::nil
		| VARpat _ => hd::nil
		| CONSTRAINTpat(p,_) =>
			diff ((p,result,tag)::more)
		| LAYEREDpat(v,p) =>
			diff ((p,layer(x,v,result),tag)::more)
		| _ => impossible "conS.diff: type error in match compiler"
	    val (cases,default) = conS (diff more)
	in  ((DATAcon dcon,convert(result,tag))::cases,default)
	end
     | APPpat(dcon as DATACON{name=r1,...},_) =>
	let fun divide nil = (nil,nil)
	      | divide ((hd as (p,result,tag))::more) =
		case p of
		  CONpat _ =>
		      let val (same,diff) = divide more
		      in  (same,hd::diff)
		      end
		| APPpat(DATACON{name=r2,...},p) =>
		      let val (same,diff) = divide more 
		      in  if Symbol.eq(r1,r2)
			  then ((p,result,tag)::same,diff)
			  else (same,hd::diff)
		      end
		| WILDpat => (hd::nil,hd::nil)
		| VARpat(VALvar{access=LVAR v,...}) =>
			((WILDpat,bind(x,v,result),tag)::nil,hd::nil)
		| CONSTRAINTpat(p,_) =>
			divide ((p,result,tag)::more)
		| LAYEREDpat(v,p) =>
			divide ((p,layer(x,v,result),tag)::more)
		| _ => impossible "conS.divide: type error in match compiler"
	    val con = DATAcon dcon
            val (same,diff) = divide arg
	    val lexp = mcor same	    (* Order imp. here:  side- *)
	    val (cases,default) = conS diff (* effects in redund. chk. *)
        in  ((con,APP(lexp,DECON(dcon,VAR x)))::cases,default)
        end
     | WILDpat => (nil,SOME(convertDefault(result,tag)))
     | VARpat(VALvar{access=LVAR v,...}) =>
			(nil,SOME(convertDefault(bind(x,v,result),tag)))
     | LAYEREDpat(v,p) => conS ((p,layer(x,v,result),tag)::more)
     | CONSTRAINTpat(p,_) => conS ((p,result,tag)::more)
     | _ => impossible "conS: type error in match compiler"
in  conS
end

and mcor nil = impossible "mcor.nil in mc"
  | mcor (arg as (p,result,tag)::more) =
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
		FN(x, convert(result,tag))
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
		in  FN(x,SWITCH(VAR x,untag cases,default))
		end
	    | REALpat _ =>
		let val (cases,default) = orSwitch x arg
		in  FN(x,SWITCH(VAR x,untag cases,default))
		end
	    | STRINGpat _ =>
		let val (cases,default) = orSwitch x arg
		in  FN(x,SWITCH(VAR x,untag cases,default))
		end
	    | RECORDpat{pats=ref nil,...} =>
		FN(x, convert(result,tag))
	    | RECORDpat{pats,...} =>
		let val newfields = map (fn _ => mkLvar()) (!pats)
		    val wild = map (fn _ => WILDpat) newfields
		    fun expand nil = nil
		      | expand ((p,result,tag)::more) =
			 case p of
			  RECORDpat{pats,...} =>
			  	(!pats,result,tag) :: expand more
			| LAYEREDpat(v,p) =>
			  	expand ((p,layer(x,v,result),tag)::more)
			| CONSTRAINTpat(p,_) =>
			  	expand ((p,result,tag)::more)
			| WILDpat =>
			  	(wild,result,tag)::nil
			| VARpat(VALvar{access=LVAR v,...}) =>
			  	(wild,bind(x,v,result),tag)::nil
			| _ => impossible "mcor.expand in mc"
		in  FN(x,bindfields(x,newfields,mcand(opt(expand arg,newfields))))
		end
	    | WILDpat =>
		FN(x, convert(result,tag))
	    | VARpat(VALvar{access=LVAR v,...}) =>
		 FN(x,convert(bind(x,v,result),tag))
	    | LAYEREDpat(v,p) =>
		FN(x,APP(mcor((p,layer(x,v,result),tag)::more),VAR x))
	    | CONSTRAINTpat(p,_) =>
		mcor ((p,result,tag)::more)
	    | _ => impossible "mcor: type error in match compiler"
      end (* fun mcor *)

fun matchPrint nil _ _ = ()
  | matchPrint [(pat,_)] _ _ = () (* never print last rule *)
  | matchPrint ((pat,_)::more) nil _ =
	(prstr "        "; printPat(pat,!printDepth); prstr " => ...\n";
	 matchPrint more nil 0)
  | matchPrint ((pat,_)::more) (taglist as (tag::tags)) i =
	if i = tag
	then (prstr "  -->   "; printPat(pat,!printDepth); prstr " => ...\n";
	      matchPrint more tags (i+1))
	else (prstr "        "; printPat(pat,!printDepth); prstr " => ...\n";
	      matchPrint more taglist (i+1))

fun bindPrint ((pat,_)::_) =
      (prstr "        "; printPat(pat,!printDepth); prstr " = ...\n")
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
let val match = (resetRedundant(); mcor(addtag rules))
    val unused = redundant rules
    val last = length rules - 1
    val printit = if !bindExhaustive andalso not(exists (fn i => i=last) unused)
		  then (warn "binding not exhaustive"; true)
		  else false
    val printit = if !bindContainsVar andalso noVarsIn rules
		  then (warn "binding contains no variables"; true)
		  else printit
in  if !printArgs
	then (warn "MC called with:"; printMatch rules)
	else ();
    if printit
	then bindPrint rules
	else ();
    if !printRet
	then (prstr "MC:  returns with\n"; printLexp match; newline())
	else ();
    match
end handle Syntax => (warn "MC called with:"; printMatch rules; raise Syntax)
  
fun matchCompile rules = 
let val match = (resetRedundant(); mcor(addtag rules))
    val unused = redundant rules
    val last = length rules - 1
    val printit = if !matchExhaustive andalso not(exists (fn i => i=last) unused)
		  then (warn "match not exhaustive"; true)
		  else false
    val printit = if exists (fn i => i<last) unused andalso !matchRedundant
        	  then (warn "redundant patterns in match"; true)
		  else printit
in  if !printArgs
	then (warn "MC called with:"; printMatch rules)
	else ();
    if printit
	then matchPrint rules unused 0
	else ();
    if !printRet
	then (prstr "MC:  returns with\n"; printLexp match; newline())
	else ();
    match
end handle Syntax => (warn "MC called with:"; printMatch rules; raise Syntax)
  

end (* struct MC *)
