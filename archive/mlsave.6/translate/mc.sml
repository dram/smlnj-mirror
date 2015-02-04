(* mc.sml *)

signature MC = sig
	structure A : BAREABSYN
	structure L : LAMBDA
	val debug : {printArgs : bool ref,
		     printRet : bool ref,
		     varBindCheck : bool ref,
		     redundantCheck : bool ref,
		     exhaustiveCheck : bool ref
	}
	datatype kind = BIND | MATCH
	val mc : ((A.pat * L.lexp) list) -> (kind -> L.lexp)
	end

structure MC : MC = struct

structure A = BareAbsyn
structure L : LAMBDA = Lambda

datatype kind = BIND | MATCH

open A L
open Access Basics MCopt PrintUtil PrintBasics PrintAbsyn MCprint ErrorMsg

val debug = 
    {printArgs = ref false,
     printRet = ref false,
     varBindCheck = ref true,
     redundantCheck = ref true,
     exhaustiveCheck = ref true}

local
   val patsUsed = ref nil
   val maybeUsed = ref nil
   fun mark taglist (tag : int) =
	let fun newtag tag nil = [tag]
	      | newtag (tag : int) (taglist as (t::more)) =
			if tag = t then taglist
			else if tag < t then tag :: taglist
			else t :: newtag tag more
	in
		taglist := newtag tag (!taglist)
 	end
   fun unused rules =
	let fun rulefind nil nil _ = nil
	      | rulefind (rule::rules) nil i = i :: rulefind rules nil (i+1)
	      | rulefind (rule::rules) (taglist as (tag::tags)) i =
			if tag = i then rulefind rules tags (i+1)
			else i :: rulefind rules taglist (i+1)
	      | rulefind _ _ _ = let exceptionx unused in raisex unused end
	in rulefind rules (!patsUsed) 0
	end

in
   fun resetRedundant () = patsUsed := nil
   fun markNotRedundant tag = mark patsUsed tag
   fun redundant rules =
	if length rules = length (!patsUsed) then nil
	else unused rules
   fun maybeNotRedundant tag = mark maybeUsed tag
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

val namefields = map (fn _ => mkLvar())
val wildrec = map (fn _ => WILDpat)

fun header record fields =
    let fun select (i, nil) e = e
          | select (i, x::xs) e = 
		  APP(FN(x,select (i+1,xs) e),SELECT(i,VAR record))
     in select (0,fields)
    end

fun andSwitch nil _ = (nil,nil)

  | andSwitch ((match as (INTpat i::_,_,_))::pats) x =
	let val (switch,default) = andSwitch pats x
	    fun plus ((match as (INTpat i::fields,result,tag)),
			    (switch as (INTcon j,pats))::switches) =
		    if i = j then ((INTcon i,(fields,result,tag)::pats)::switches)
		    else switch::(plus (match,switches))
	      | plus((INTpat i::fields,result,tag),nil) =
			    [(INTcon i,(fields,result,tag)::default)]
	      | plus arg = impossible "983 type error in match compiler"
	 in
		(plus (match,switch),default)
	end

  | andSwitch ((match as (REALpat r::_,_,_))::pats) x =
	let val (switch,default) = andSwitch pats x
	fun plus ((match as (REALpat r::fields,result,tag)),
			(switch as (REALcon s,pats))::switches) =
		if r = s then ((REALcon r,(fields,result,tag)::pats)::switches)
		else switch::(plus (match,switches))
	  | plus ((REALpat r::fields,result,tag),nil) =
			[(REALcon r,(fields,result,tag)::default)]
	  | plus arg = impossible "48 type error in match compiler"
	 in
		(plus (match,switch),default)
	end

  | andSwitch ((match as (STRINGpat s::_,_,_))::pats) x =
	let val (switch,default) = andSwitch pats x
	fun plus ((match as (STRINGpat s::fields,result,tag)),
			(switch as (STRINGcon t,pats))::switches) =
		if s = t then ((STRINGcon s,(fields,result,tag)::pats)::switches)
		else switch::(plus (match,switches))
	  | plus ((STRINGpat s::fields,result,tag),nil) =
			[(STRINGcon s,(fields,result,tag)::default)]
	  | plus arg = impossible "482 type error in match compiler"
	 in
		(plus (match,switch),default)
	end

  | andSwitch ((match as (CONpat _::_,_,_))::pats) x =
      let val (switch,default) = andSwitch pats x
	  fun plus ((match as (CONpat (dcon as DATACON {name=r1,...})::fields,
			       result,tag)),
		    (switch as (DATAcon(DATACON {name=r2,...}),pats))::switches) =
		  if Symbol.eq(r1,r2)
		    then (DATAcon(dcon),(fields,result,tag)::pats)::switches
		    else switch::(plus (match,switches))
	    | plus ((CONpat dcon::fields,result,tag),nil) =
			  [(DATAcon(dcon),(fields,result,tag)::default)]
	    | plus arg = impossible "87 type error in match compiler"
	 in
		(plus (match,switch),default)
	end

  | andSwitch ((match as (APPpat _::_,_,_))::pats) x =
	let val (switch,default) = andSwitch pats x
	fun plus ((match as (APPpat ((dcon as DATACON {name=r1,...}),p)::fields,
			     result,tag)),
		  (switch as (DATAcon(DATACON {name=r2,...}),pats))::switches) =
		if Symbol.eq(r1,r2) then ((DATAcon(dcon),(p::fields,result,tag)::pats)::switches)
		else switch::(plus (match,switches))
	  | plus ((APPpat (dcon,p)::fields,result,tag),nil) =
		let fun addwild (fields,result,tag) = (WILDpat::fields,result,tag)
		 in
			[(DATAcon(dcon),(p::fields,result,tag)::(map addwild default))]
		end
	  | plus arg = impossible "444 type error in match compiler"
	 in
		(plus (match,switch),default)
	end

  | andSwitch ((match as (WILDpat::fields,result,tag))::pats) x =
	let val (switch,default) = andSwitch pats x
	fun plus ((match as (WILDpat::fields,result,tag)),
		  ((con as DATAcon(DATACON{const=false,...})),pats)::switches) =
		(con,(WILDpat::fields,result,tag)::pats)::(plus (match,switches))
	  | plus ((match as (WILDpat::fields,result,tag)),(con,pats)::switches) =
		(con,(fields,result,tag)::pats)::(plus (match,switches))
	  | plus ((WILDpat::_,_,_),nil) = nil
	  | plus arg = impossible "003 type error in match compiler"
	 in
		(plus (match,switch),(fields,result,tag)::default)
	end

  | andSwitch ((VARpat(VALvar{access=LVAR v,...})::fields,result,tag)::pats) x =
	andSwitch ((WILDpat::fields,bind(x,v,result),tag)::pats) x

  | andSwitch ((LAYEREDpat (v,p)::fields,result,tag)::pats) x =
	andSwitch ((p::fields,layer(x,v,result),tag)::pats) x

  | andSwitch ((CONSTRAINTpat (p,_)::fields,result,tag)::pats) x =
	andSwitch ((p::fields,result,tag)::pats) x

  | andSwitch _ _ = impossible "9987 in mc"

val untag = map (fn (dcon,lexp,tag) => (dcon,lexp))

fun orSwitch (arg as ((INTpat i,result,tag)::pats)) x =
	let fun diff ((head as (INTpat j,_,_))::pats) =
			if i = j then diff pats
			else head::diff pats
	      | diff ((head as (WILDpat,_,_))::_) = head::nil
	      | diff ((VARpat(VALvar{access=LVAR v,...}),result,tag)::pats) =
			(WILDpat,bind(x,v,result),tag)::nil
	      | diff ((LAYEREDpat (v,p),result,tag)::pats) =
			diff ((p,layer(x,v,result),tag)::pats)
	      | diff ((CONSTRAINTpat (p,_),result,tag)::pats) =
			diff ((p,result,tag)::pats)
	      | diff nil = nil
	      | diff arg = impossible "838 type error in match compiler"
	val (switch,default) = orSwitch (diff arg) x
	in markNotRedundant tag;
	   ((INTcon i,result,tag)::switch,default)
	end

  | orSwitch (arg as ((REALpat r,result,tag)::pats)) x =
	let fun diff ((head as (REALpat s,_,_))::pats) =
			if r = s then diff pats
			else head::diff pats
	      | diff ((head as (WILDpat,_,_))::_) = head::nil
	      | diff ((VARpat(VALvar{access=LVAR v,...}),result,tag)::pats) =
			(WILDpat,bind(x,v,result),tag)::nil
	      | diff ((LAYEREDpat (v,p),result,tag)::pats) =
			diff ((p,layer(x,v,result),tag)::pats)
	      | diff ((CONSTRAINTpat (p,_),result,tag)::pats) =
			diff ((p,result,tag)::pats)
	      | diff nil = nil
	      | diff arg = impossible "33 type error in match compiler"
	val (switch,default) = orSwitch (diff arg) x
	in markNotRedundant tag;
	   ((REALcon r,result,tag)::switch,default)
	end

  | orSwitch (arg as ((STRINGpat s,result,tag)::pats)) x =
	let fun diff ((head as (STRINGpat t,_,_))::pats) =
			if s = t then diff pats
			else head::diff pats
	      | diff ((head as (WILDpat,_,_))::_) = head::nil
	      | diff ((VARpat(VALvar{access=LVAR v,...}),result,tag)::pats) =
			(WILDpat,bind(x,v,result),tag)::nil
	      | diff ((LAYEREDpat (v,p),result,tag)::pats) =
			diff ((p,layer(x,v,result),tag)::pats)
	      | diff ((CONSTRAINTpat (p,_),result,tag)::pats) =
			diff ((p,result,tag)::pats)
	      | diff nil = nil
	      | diff arg = impossible "8331 type error in match compiler"
	val (switch,default) = orSwitch (diff arg) x
	in markNotRedundant tag;
	   ((STRINGcon s,result,tag)::switch,default)
	end

  | orSwitch ((WILDpat,result,tag)::_) _ =
		(markNotRedundant tag;
		 (nil,SOME result))

  | orSwitch ((VARpat(VALvar{access=LVAR v,...}),result,tag)::_) x =
		(markNotRedundant tag;
		 (nil,SOME (bind(x,v,result))))

  | orSwitch ((CONSTRAINTpat (p,_),result,tag)::pats) x =
	orSwitch ((p,result,tag)::pats) x

  | orSwitch ((LAYEREDpat (v,p),result,tag)::pats) x = 
	orSwitch ((p,layer(x,v,result),tag)::pats) x

  | orSwitch nil _ =  impossible "orSwitch nil in mc"
  | orSwitch _ _ = impossible "883 in mc"

fun mcand ((arg as (([single],_,_)::_)),[x]) =
	let val rec singlelist = fn
		  nil => nil
		| ([single],result,tag)::pats =>
			(single,result,tag) :: (singlelist pats)
		| _ => impossible "38338 in mc"
	 in
		APP(mcor (singlelist arg), VAR x)
	end
  | mcand (((WILDpat::fields,result,tag)::_),(_::xs)) =
	mcand ([(fields,result,tag)],xs)
  | mcand (((VARpat(VALvar{access=LVAR v,...})::fields,result,tag)::_),(x::xs)) = 
	mcand ([(fields,bind(x,v,result),tag)],xs)
  | mcand (((LAYEREDpat (v,p)::fields,result,tag)::pats),(x::xs)) =
	mcand (((p::fields,layer(x,v,result),tag)::pats),(x::xs))
  | mcand (((CONSTRAINTpat (p,_)::fields,result,tag)::pats),xl) =
	mcand (((p::fields,result,tag)::pats),xl)

  | mcand ((arg as ((APPpat(DATACON{dcons = ref[_],...},_)::_,
		_,_)::_)),x::xs) =
	let val newx = mkLvar()
	    val ([(DATAcon dcon,list)],_) = andSwitch arg x
	 in APP(FN(newx,mcand(opt(list,(newx::xs)))),DECON(dcon,VAR x))
	end

  | mcand ((arg as((APPpat(DATACON{dcons,...},_)::_,_,_)::_)),
					(xl as (x::xs))) =
	let val (switch,default) = andSwitch arg x
	fun matchswitch (x::xs) (con as DATAcon(con1 as DATACON{const = false,...}),pats) =
	     let val new = mkLvar ()
	      in
	     	(con,APP(FN(new,mcand (opt (pats,new::xs))),DECON (con1,VAR x)))
	     end
	  | matchswitch (_::xs) (con as DATAcon(DATACON {const = true,...}),pats) =
	     (con,mcand (opt (pats,xs)))
	  | matchswitch _ _ = impossible "3321 in mc"
	 in
		SWITCH(VAR x,map (matchswitch xl) switch,
		       if length switch = length (!dcons) then NONE
		       else SOME (mcand (opt (default,xs))))
	end

  | mcand ((CONpat(DATACON{dcons=ref [_],...})::fields,result,tag)::_,
		_::xs) = mcand ([(fields,result,tag)],xs)

  | mcand ((arg as((CONpat (DATACON{dcons,...})::_,_,_)::_)),(xl as (x::xs))) =
	let val (switch,default) = andSwitch arg x
	fun matchswitch (x::xs) ((con as DATAcon(con1 as DATACON{const = false,...})),pats) =
	     let val new = mkLvar ()
	      in
	     	(con,APP(FN(new,mcand (opt (pats,new::xs))),DECON (con1,VAR x)))
	     end
	  | matchswitch (_::xs) ((con as DATAcon(DATACON {const = true,...})),pats) =
	     (con,mcand (opt (pats,xs)))
	  | matchswitch _ _ = impossible "589 in mc"
	 in
		SWITCH(VAR x,map (matchswitch xl) switch,
		       if length switch = length (!dcons) then NONE
		           else SOME (mcand (opt (default,xs))))

	end

  | mcand ((arg as ((RECORDpat {pats=ref(nil),...}::fields,result,tag)::_)),(_::xs)) =
	    mcand ([(fields,result,tag)],xs)

  | mcand ((arg as ((RECORDpat {pats=ref list,...}::fields,result,tag)::pats)),(x::xs)) =
	  let val newfields = namefields list
	      val wild = wildrec newfields
	      fun convert ((RECORDpat{pats=ref list,...}::fields,result,tag)::pats) =
			    (list@fields,result,tag) :: convert pats
	        | convert ((LAYEREDpat (v,p)::fields,result,tag)::pats) =
			    convert ((p::fields,layer(x,v,result),tag)::pats)
	        | convert ((CONSTRAINTpat (p,_)::fields,result,tag)::pats) =
			    convert ((p::fields,result,tag)::pats)
	        | convert ((WILDpat::fields,result,tag)::pats) =
			    (wild@fields,result,tag) :: convert pats
	        | convert ((VARpat(VALvar{access=LVAR v,...})::fields,result,tag)::pats) =
			    (wild@fields,bind(x,v,result),tag) :: convert pats
	        | convert nil = nil
		| convert _ = impossible "873 in mc"
	      in
	        header x newfields (mcand(opt(convert arg,newfields@xs)))
	      end

  | mcand (arg,x::xs) =
	let val (switch,default) = andSwitch arg x
	fun matchswitch xs (con,pats) =
		(con,mcand (opt (pats,xs)))
	 in
		SWITCH(VAR x,map (matchswitch xs) switch,
			SOME(mcand(opt(default,xs))))
	end
  | mcand _ = impossible "22465 in mc"

and conSwitch ((CONpat (dcon as DATACON {name=r1,...}),result,tag)::pats) x =
	let fun diff ((head as (CONpat (DATACON {name=r2,...}),_,_))::pats) =
			if Symbol.eq(r1,r2) then diff pats
			else (head::diff pats)
	      | diff ((head as (APPpat (_,_),_,_))::pats) = head::diff pats
	      | diff ((head as (WILDpat,_,_))::_) = [head]
	      | diff ((head as (VARpat _,_,_))::_) = [head]
	      | diff ((CONSTRAINTpat (p,_),result,tag)::pats) =
			diff ((p,result,tag)::pats)
	      | diff ((LAYEREDpat (v,p),result,tag)::pats) =
			diff ((p,layer(x,v,result),tag)::pats)
	      | diff nil = nil
	      | diff arg = impossible "11 type error in match compiler"
	val (switch,default) = conSwitch (diff pats) x
	 in markNotRedundant tag;
	    ((DATAcon(dcon),result)::switch,default)
	end
  | conSwitch (arg as ((APPpat (dcon as DATACON {name=r1,...},_),_,_)::_)) x =
      let fun divide ((head as (CONpat _,_,_))::pats) =
		      let val (same,diff) = divide pats
		       in
			      (same,head::diff)
		      end
	    | divide ((arg as (APPpat (DATACON {name=r2,...},p),result,tag))::pats) =
		      let val (same,diff) = divide pats 
		       in
			      if Symbol.eq(r1,r2)
			      then ((p,result,tag)::same,diff)
			      else (same,arg::diff)
		      end
	    | divide ((head as (WILDpat,_,_))::_) = ([head],[head])
	    | divide ((head as (VARpat(VALvar{access=LVAR v,...}),result,tag))::_) =
		      ([(WILDpat,bind(x,v,result),tag)],[head])
	    | divide ((CONSTRAINTpat (p,_),result,tag)::pats) =
		      divide ((p,result,tag)::pats)
	    | divide ((LAYEREDpat (v,p),result,tag)::pats) =
		      divide ((p,layer(x,v,result),tag)::pats)
	    | divide nil = (nil,nil)
	    | divide arg = impossible "455 type error in match compiler"
	  val con = DATAcon(dcon)
          val (same,diff) = divide arg
	  val lexp = mcor same			  (* Order imp. here:  side- *)
	  val (switch,default) = conSwitch diff x (* effects in redund. chk. *)
       in
	      ((con,APP(lexp,DECON(dcon,VAR x)))::switch,default)
      end

  | conSwitch ((WILDpat,result,tag)::_) _ =
		(maybeNotRedundant tag;
		 (nil,SOME result))

  | conSwitch ((VARpat(VALvar{access=LVAR v,...}),result,tag)::_) x =
		(maybeNotRedundant tag;
		 (nil,SOME (bind(x,v,result))))

  | conSwitch ((LAYEREDpat (v,p),result,tag)::pats) x =
		conSwitch ((p,layer(x,v,result),tag)::pats) x

  | conSwitch ((CONSTRAINTpat (p,_),result,tag)::pats) x =
		conSwitch ((p,result,tag)::pats) x

  | conSwitch nil _ = (nil,NONE)

  | conSwitch arg _ = impossible "394 type error in match compiler"

and mcor (arg as ((CONpat (DATACON{dcons=ref nil,...}),_,_)::_)) = (* exception *)
	let val x = mkLvar()
	val (switch,default) = conSwitch arg x
	 in areNotRedundant();
	    FN(x,SWITCH(VAR x,switch,default))
	end

  | mcor (arg as ((APPpat (DATACON{dcons=ref nil,...},_),_,_)::_)) = (* exn *)
	let val x = mkLvar()
	val (switch,default) = conSwitch arg x
	 in areNotRedundant();
	    FN(x,SWITCH(VAR x,switch,default))
	end

  | mcor ((CONpat(DATACON{dcons = ref [_],...}),result,tag)::_) =
		(markNotRedundant tag;
		 FN(mkLvar(),result))

  | mcor (arg as ((CONpat(DATACON{dcons,...}),_,_)::_)) =
	let val x = mkLvar()
	val (switch,default) = conSwitch arg x
	 in FN(x,SWITCH(VAR x, switch,
	        (if length (!dcons) = length switch
		   then (areRedundant(); NONE)
		   else (areNotRedundant(); default))))
	end

  | mcor (arg as ((APPpat((DATACON{dcons = ref[_],...}),_),_,_)::_)) =
	let val x = mkLvar()
	val ([(con,lexp)],_) = conSwitch arg x
	in areRedundant();
	   FN(x,lexp)
	end

  | mcor (arg as ((APPpat((DATACON{dcons,...}),_),_,_)::_)) =
	let val x = mkLvar()
	val (switch,default) = conSwitch arg x
	in FN(x,SWITCH(VAR x, switch,
	       (if length (!dcons) = length switch
		then (areRedundant(); NONE)
		else (areNotRedundant(); default))))
	end

  | mcor (arg as ((INTpat _,_,_)::_)) =
	let val x = mkLvar()
	val (switch,default) = orSwitch arg x
	in 
		FN(x,SWITCH(VAR x,untag switch,default))
	end

  | mcor (arg as ((REALpat _,_,_)::_)) =
	let val x = mkLvar()
	val (switch,default) = orSwitch arg x
	in
		FN(x,SWITCH(VAR x,untag switch,default))
	end

  | mcor (arg as ((STRINGpat _,_,_)::_)) =
	let val x = mkLvar()
	val (switch,default) = orSwitch arg x
	in
		FN(x,SWITCH(VAR x,untag switch,default))
	end

  | mcor (arg as ((RECORDpat{pats=ref nil,...},result,tag)::_)) =
		(markNotRedundant tag;
		 FN(mkLvar(),result))

  | mcor (arg as ((RECORDpat{pats=ref list,...},result,tag)::pats)) =
	let val x = mkLvar()
	val newfields = namefields list
	val wild = wildrec newfields
	fun convert ((RECORDpat{pats=ref list,...},result,tag)::pats) =
		  (list,result,tag) :: convert pats
	  | convert ((LAYEREDpat (v,p),result,tag)::pats) =
		  convert ((p,layer(x,v,result),tag)::pats)
	  | convert ((CONSTRAINTpat (p,_),result,tag)::pats) =
		  convert ((p,result,tag)::pats)
	  | convert ((WILDpat,result,tag):: _) =
		  (wild,result,tag) :: nil
	  | convert ((VARpat(VALvar{access=LVAR v,...}),result,tag)::_) =
		  (wild,bind(x,v,result),tag) :: nil
	  | convert nil = nil
	  | convert _ = impossible "3883 in mc"
	 in
		FN(x,header x newfields (mcand(opt(convert arg,newfields))))
	end
  | mcor ((WILDpat,result,tag)::_) =
	let val x = mkLvar()
	in markNotRedundant tag;
	   FN(x,result)
	end
  | mcor ((VARpat(VALvar{access=LVAR v,...}),result,tag)::_) =
	let val x = mkLvar()
	in markNotRedundant tag;
	   FN(x,bind(x,v,result))
	end
  | mcor ((LAYEREDpat(v,p),result,tag)::pats) =
	let val x = mkLvar()
	in
		FN(x,APP(mcor((p,layer(x,v,result),tag)::pats),VAR x))
	end
  | mcor ((CONSTRAINTpat(p,_),result,tag)::pats) =
	mcor ((p,result,tag)::pats)
  | mcor nil = impossible "4778 in mc"
  | mcor _ = impossible "4777 type error in match compiler"

fun addtag rules =
	let fun tag ((pat,result)::more) i =
			(pat,result,i) :: tag more (i+1)
	      | tag nil _ = nil
	in tag rules 0
	end

fun printbind ((pat,_)::_) = (prstr "        "; printPat pat; prstr " = ...\n")
   |printbind _ = impossible "838 in mc"

fun printmatch nil _ _ = ()
  | printmatch ((pat,_)::nil) _ _ = () (* never print last rule *)
  | printmatch ((pat,_)::more) nil _ =
	(prstr "        "; printPat pat; prstr " => ...\n";
	 printmatch more nil 0)
  | printmatch ((pat,_)::more) (taglist as (tag::tags)) i =
	if i = tag
	then (prstr "  -->   "; printPat pat; prstr " => ...\n";
	      printmatch more tags (i+1))
	else (prstr "        "; printPat pat; prstr " => ...\n";
	      printmatch more taglist (i+1))

fun noVarsIn ((pat,_)::_) =
	let fun var (VARpat _) = true
	      | var (LAYEREDpat _) = true
	      | var (CONSTRAINTpat(p,_)) = var p
	      | var (APPpat(_,p)) = var p
	      | var (RECORDpat{pats=ref patlist,...}) = exists (var,patlist)
	      | var _ = false
	in not (var pat)
	end
  | noVarsIn _ = impossible "844 in mc"
fun mc rules bindormatch = 
let val {printArgs,printRet,varBindCheck,
	 redundantCheck,exhaustiveCheck,...} = debug
    fun reportErr () = 
    let val unused = redundant rules
	val reportVar = (bindormatch = BIND) andalso (!varBindCheck)
			 andalso noVarsIn rules
	fun saykind MATCH  = "match" | saykind BIND = "binding"
	val last = length rules - 1
	val printit = ref false
    in if not (exists( (fn i => i=last), unused))
	    andalso (!exhaustiveCheck)
          then (warn (saykind bindormatch ^ " not exhaustive");
		printit  := true)
	  else ();

       if exists( (fn i => i<last), unused) andalso !redundantCheck
        then (warn "redundant patterns in match"; printit := true)
	else ();

       if reportVar
       then (warn "binding contains no variables"; printit := true)
       else ();

       if !printit
       then case bindormatch of
		  MATCH => printmatch rules unused 0
		| BIND => printbind rules
       else ()
    end
in
    if (!printArgs)
    then (warn "MC called with:"; printMatch rules)
    else ();

    resetRedundant();
    let val patmatch = mcor (addtag rules)
    in
	reportErr ();

	if (!printRet)
	then (prstr "MC:  returns with\n"; printLexp patmatch; newline())
	else ();

	patmatch
    end
    handlex Syntax => (warn "MC called with:"; printMatch rules; raisex Syntax)
end
  

end (* struct MC *)
