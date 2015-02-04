(* mc.sml *)

signature MC = sig
	structure A : BAREABSYN
	structure L : LAMBDA
	val Debug : {PrintArgs : bool ref,
		     PrintRet : bool ref,
		     VarBindCheck : bool ref,
		     RedundantCheck : bool ref,
		     ExhaustiveCheck : bool ref
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

val Debug = 
    {PrintArgs = ref false,
     PrintRet = ref false,
     VarBindCheck = ref true,
     RedundantCheck = ref true,
     ExhaustiveCheck = ref true}

local
   val PatsUsed = ref nil
   val MaybeUsed = ref nil
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
	in rulefind rules (!PatsUsed) 0
	end

in
   fun ResetRedundant () = PatsUsed := nil
   fun MarkNotRedundant tag = mark PatsUsed tag
   fun Redundant rules =
	if length rules = length (!PatsUsed) then nil
	else unused rules
   fun MaybeNotRedundant tag = mark MaybeUsed tag
   fun AreNotRedundant () =
	case MaybeUsed of
	      ref nil => ()
	    | ref (tag::_) => (mark PatsUsed tag; MaybeUsed := nil)
   fun AreRedundant () = MaybeUsed := nil
end

local
   val exhaustive = ref true
   val UNIT = RECORD nil
   val matchsym = SymbolTable.StringToSymbol "e_match"
   val bindsym = SymbolTable.StringToSymbol "e_bind"
   val EXN = ref matchsym
in
	fun MakeMatch () =  EXN := matchsym
	fun MakeBind () =   EXN := bindsym

	fun ResetExhaustive () = (exhaustive := true)
	fun Exhaustive () = (!exhaustive)

	fun RaiseMatch () = 
         (exhaustive := false;
          SOME (RAISE (CON(EnvAccess.lookEXNinBase(!EXN), UNIT))))
	handlex Table.notfound => NONE

end (* local *)


fun bind(x,v,result) = APP(FN(v,result),VAR x)

fun layer (x,CONSTRAINTpat(pat,_),result) = layer(x,pat,result)
  | layer (x,VARpat(VALvar{access=LVAR v,...}),result) = bind(x,v,result)
  | layer _ = Impossible "bad layered pattern in mc"

val namefields = map (fn _ => mkLvar())
val wildrec = map (fn _ => WILDpat)

fun header record fields =
    let fun select (i, nil) e = e
          | select (i, x::xs) e = 
		  APP(FN(x,select (i+1,xs) e),SELECT(i,VAR record))
     in select (0,fields)
    end

fun AndSwitch nil _ = (nil,nil)

  | AndSwitch ((match as (INTpat i::_,_,_))::pats) x =
	let val (switch,default) = AndSwitch pats x
	    fun plus ((match as (INTpat i::fields,result,tag)),
			    (switch as (INTcon j,pats))::switches) =
		    if i = j then ((INTcon i,(fields,result,tag)::pats)::switches)
		    else switch::(plus (match,switches))
	      | plus((INTpat i::fields,result,tag),nil) =
			    [(INTcon i,(fields,result,tag)::default)]
	      | plus arg = Impossible "983 type error in match compiler"
	 in
		(plus (match,switch),default)
	end

  | AndSwitch ((match as (REALpat r::_,_,_))::pats) x =
	let val (switch,default) = AndSwitch pats x
	fun plus ((match as (REALpat r::fields,result,tag)),
			(switch as (REALcon s,pats))::switches) =
		if r = s then ((REALcon r,(fields,result,tag)::pats)::switches)
		else switch::(plus (match,switches))
	  | plus ((REALpat r::fields,result,tag),nil) =
			[(REALcon r,(fields,result,tag)::default)]
	  | plus arg = Impossible "48 type error in match compiler"
	 in
		(plus (match,switch),default)
	end

  | AndSwitch ((match as (STRINGpat s::_,_,_))::pats) x =
	let val (switch,default) = AndSwitch pats x
	fun plus ((match as (STRINGpat s::fields,result,tag)),
			(switch as (STRINGcon t,pats))::switches) =
		if s = t then ((STRINGcon s,(fields,result,tag)::pats)::switches)
		else switch::(plus (match,switches))
	  | plus ((STRINGpat s::fields,result,tag),nil) =
			[(STRINGcon s,(fields,result,tag)::default)]
	  | plus arg = Impossible "482 type error in match compiler"
	 in
		(plus (match,switch),default)
	end

  | AndSwitch ((match as (CONpat _::_,_,_))::pats) x =
      let val (switch,default) = AndSwitch pats x
	  fun plus ((match as (CONpat (dcon as DATACON {rep=r1,...})::fields,
			       result,tag)),
		    (switch as (DATAcon(DATACON {rep=r2,...}),pats))::switches) =
		  if r1=r2
		    then (DATAcon(dcon),(fields,result,tag)::pats)::switches
		    else switch::(plus (match,switches))
	    | plus ((CONpat dcon::fields,result,tag),nil) =
			  [(DATAcon(dcon),(fields,result,tag)::default)]
	    | plus arg = Impossible "87 type error in match compiler"
	 in
		(plus (match,switch),default)
	end

  | AndSwitch ((match as (APPpat _::_,_,_))::pats) x =
	let val (switch,default) = AndSwitch pats x
	fun plus ((match as (APPpat ((dcon as DATACON {rep=r1,...}),p)::fields,
			     result,tag)),
		  (switch as (DATAcon(DATACON {rep=r2,...}),pats))::switches) =
		if r1 = r2 then ((DATAcon(dcon),(p::fields,result,tag)::pats)::switches)
		else switch::(plus (match,switches))
	  | plus ((APPpat (dcon,p)::fields,result,tag),nil) =
		let fun addwild (fields,result,tag) = (WILDpat::fields,result,tag)
		 in
			[(DATAcon(dcon),(p::fields,result,tag)::(map addwild default))]
		end
	  | plus arg = Impossible "444 type error in match compiler"
	 in
		(plus (match,switch),default)
	end

  | AndSwitch ((match as (WILDpat::fields,result,tag))::pats) x =
	let val (switch,default) = AndSwitch pats x
	fun plus ((match as (WILDpat::fields,result,tag)),
		  ((con as DATAcon(DATACON{const=false,...})),pats)::switches) =
		(con,(WILDpat::fields,result,tag)::pats)::(plus (match,switches))
	  | plus ((match as (WILDpat::fields,result,tag)),(con,pats)::switches) =
		(con,(fields,result,tag)::pats)::(plus (match,switches))
	  | plus ((WILDpat::_,_,_),nil) = nil
	  | plus arg = Impossible "003 type error in match compiler"
	 in
		(plus (match,switch),(fields,result,tag)::default)
	end

  | AndSwitch ((VARpat(VALvar{access=LVAR v,...})::fields,result,tag)::pats) x =
	AndSwitch ((WILDpat::fields,bind(x,v,result),tag)::pats) x

  | AndSwitch ((LAYEREDpat (v,p)::fields,result,tag)::pats) x =
	AndSwitch ((p::fields,layer(x,v,result),tag)::pats) x

  | AndSwitch ((CONSTRAINTpat (p,_)::fields,result,tag)::pats) x =
	AndSwitch ((p::fields,result,tag)::pats) x

val untag = map (fn (dcon,lexp,tag) => (dcon,lexp))

fun OrSwitch (arg as ((INTpat i,result,tag)::pats)) x =
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
	      | diff arg = Impossible "838 type error in match compiler"
	val (switch,default) = OrSwitch (diff arg) x
	in MarkNotRedundant tag;
	   ((INTcon i,result,tag)::switch,default)
	end

  | OrSwitch (arg as ((REALpat r,result,tag)::pats)) x =
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
	      | diff arg = Impossible "33 type error in match compiler"
	val (switch,default) = OrSwitch (diff arg) x
	in MarkNotRedundant tag;
	   ((REALcon r,result,tag)::switch,default)
	end

  | OrSwitch (arg as ((STRINGpat s,result,tag)::pats)) x =
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
	      | diff arg = Impossible "8331 type error in match compiler"
	val (switch,default) = OrSwitch (diff arg) x
	in MarkNotRedundant tag;
	   ((STRINGcon s,result,tag)::switch,default)
	end

  | OrSwitch ((WILDpat,result,tag)::_) _ =
		(MarkNotRedundant tag;
		 (nil,SOME result))

  | OrSwitch ((VARpat(VALvar{access=LVAR v,...}),result,tag)::_) x =
		(MarkNotRedundant tag;
		 (nil,SOME (bind(x,v,result))))

  | OrSwitch ((CONSTRAINTpat (p,_),result,tag)::pats) x =
	OrSwitch ((p,result,tag)::pats) x

  | OrSwitch ((LAYEREDpat (v,p),result,tag)::pats) x = 
	OrSwitch ((p,layer(x,v,result),tag)::pats) x

  | OrSwitch nil _ =  (nil,RaiseMatch())


fun mcand ((arg as (([single],_,_)::_)),[x]) =
	let val rec singlelist = fn
		  nil => nil
		| ([single],result,tag)::pats =>
			(single,result,tag) :: (singlelist pats)
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

  | mcand ((arg as ((APPpat(DATACON{tycon=DATAtyc{dcons = ref[_],...},...},_)::_,
		_,_)::_)),x::xs) =
	let val newx = mkLvar()
	    val ([(DATAcon dcon,list)],_) = AndSwitch arg x
	 in APP(FN(newx,mcand(opt(list,(newx::xs)))),DECON(dcon,VAR x))
	end

  | mcand ((arg as((APPpat(DATACON{tycon=DATAtyc{dcons,...},...},_)::_,_,_)::_)),
					(xl as (x::xs))) =
	let val (switch,default) = AndSwitch arg x
	fun matchswitch (x::xs) (con as DATAcon(con1 as DATACON{const = false,...}),pats) =
	     let val new = mkLvar ()
	      in
	     	(con,APP(FN(new,mcand (opt (pats,new::xs))),DECON (con1,VAR x)))
	     end
	  | matchswitch (_::xs) (con as DATAcon(DATACON {const = true,...}),pats) =
	     (con,mcand (opt (pats,xs)))
	val newdefault =
		(case default of
		  nil => if length switch = length (!dcons) then NONE
			 else RaiseMatch()
		| _ => if length switch = length (!dcons) then NONE
		       else SOME (mcand (opt (default,xs)))
		)
	 in
		SWITCH(VAR x,map (matchswitch xl) switch,newdefault)
	end

  | mcand ((CONpat(DATACON{tycon=DATAtyc{dcons=ref [_],...},...})::fields,result,tag)::_,
		_::xs) = mcand ([(fields,result,tag)],xs)

  | mcand ((arg as((CONpat (DATACON{tycon=DATAtyc{dcons,...},...})::_,_,_)::_)),
					(xl as (x::xs))) =
	let val (switch,default) = AndSwitch arg x
	fun matchswitch (x::xs) ((con as DATAcon(con1 as DATACON{const = false,...})),pats) =
	     let val new = mkLvar ()
	      in
	     	(con,APP(FN(new,mcand (opt (pats,new::xs))),DECON (con1,VAR x)))
	     end
	  | matchswitch (_::xs) ((con as DATAcon(DATACON {const = true,...})),pats) =
	     (con,mcand (opt (pats,xs)))
	val newdefault =
		(case default of
		  nil => if length switch = length (!dcons) then NONE
			 else RaiseMatch()
		| _ => if length switch = length (!dcons) then NONE
		       else SOME (mcand (opt (default,xs)))
		)
	 in
		SWITCH(VAR x,map (matchswitch xl) switch,newdefault)
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
(* old	        | convert ((VARpat _ ::fields,result,tag)::pats) =
			    (wild@fields,result,tag) :: convert pats *)
	        | convert nil = nil
	      in
	        header x newfields (mcand(opt(convert arg,newfields@xs)))
	      end

  | mcand (arg,x::xs) =
	let val (switch,default) = AndSwitch arg x
	fun matchswitch xs (con,pats) =
		(con,mcand (opt (pats,xs)))
	 in
		SWITCH(VAR x,map (matchswitch xs) switch,
			case default of
				nil => RaiseMatch()
				| _ => SOME(mcand(opt(default,xs)))
		)
	end


and ConSwitch ((CONpat (dcon as DATACON {rep=r1,...}),result,tag)::pats) x =
	let fun diff ((head as (CONpat (DATACON {rep=r2,...}),_,_))::pats) =
			if r1=r2 then diff pats
			else (head::diff pats)
	      | diff ((head as (APPpat (_,_),_,_))::pats) = head::diff pats
	      | diff ((head as (WILDpat,_,_))::_) = [head]
	      | diff ((head as (VARpat _,_,_))::_) = [head]
	      | diff ((CONSTRAINTpat (p,_),result,tag)::pats) =
			diff ((p,result,tag)::pats)
	      | diff ((LAYEREDpat (v,p),result,tag)::pats) =
			diff ((p,layer(x,v,result),tag)::pats)
	      | diff nil = nil
	      | diff arg = Impossible "11 type error in match compiler"
	val (switch,default) = ConSwitch (diff pats) x
	 in MarkNotRedundant tag;
	    ((DATAcon(dcon),result)::switch,default)
	end
  | ConSwitch (arg as ((APPpat (dcon as DATACON {rep=r1,...},_),_,_)::_)) x =
      let fun divide ((head as (CONpat _,_,_))::pats) =
		      let val (same,diff) = divide pats
		       in
			      (same,head::diff)
		      end
	    | divide ((arg as (APPpat (DATACON {rep=r2,...},p),result,tag))::pats) =
		      let val (same,diff) = divide pats 
		       in
			      if r1=r2
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
	    | divide arg = Impossible "455 type error in match compiler"
	  val con = DATAcon(dcon)
          val (same,diff) = divide arg
	  val lexp = mcor same			  (* Order imp. here:  side- *)
	  val (switch,default) = ConSwitch diff x (* effects in redund. chk. *)
       in
	      ((con,APP(lexp,DECON(dcon,VAR x)))::switch,default)
      end

  | ConSwitch ((WILDpat,result,tag)::_) _ =
		(MaybeNotRedundant tag;
		 (nil,SOME result))

  | ConSwitch ((VARpat(VALvar{access=LVAR v,...}),result,tag)::_) x =
		(MaybeNotRedundant tag;
		 (nil,SOME (bind(x,v,result))))

  | ConSwitch ((LAYEREDpat (v,p),result,tag)::pats) x =
		ConSwitch ((p,layer(x,v,result),tag)::pats) x

  | ConSwitch ((CONSTRAINTpat (p,_),result,tag)::pats) x =
		ConSwitch ((p,result,tag)::pats) x

  | ConSwitch nil _ = (nil,NONE)

  | ConSwitch arg _ = Impossible "394 type error in match compiler"

and mcor ((CONpat(DATACON{tycon=DATAtyc{dcons = ref [_],...},...}),result,tag)::_) =
		(MarkNotRedundant tag;
		 FN(mkLvar(),result))

  | mcor (arg as ((CONpat(DATACON{tycon=DATAtyc{dcons,...},...}),_,_)::_)) =
	let val x = mkLvar()
	val (switch,default) = ConSwitch arg x
	 in FN(x,SWITCH(VAR x, switch,
	        (if length (!dcons) = length switch
		   then (AreRedundant(); NONE)
		   else (AreNotRedundant();
		         (case default of
			    NONE => RaiseMatch()
			  | SOME _ => default)))))
		      
	end

  | mcor (arg as ((APPpat((DATACON{tycon=DATAtyc{dcons = ref[_],...},...}),_),_,_)::_)) =
	let val x = mkLvar()
	val ([(con,lexp)],_) = ConSwitch arg x
	in AreRedundant();
	   FN(x,lexp)
	end

  | mcor (arg as ((APPpat((DATACON{tycon=DATAtyc{dcons,...},...}),_),_,_)::_)) =
	let val x = mkLvar()
	val (switch,default) = ConSwitch arg x
	in FN(x,SWITCH(VAR x, switch,
	       (if length (!dcons) = length switch
		then (AreRedundant(); NONE)
		else (AreNotRedundant();
		      (case default of
			   NONE => RaiseMatch()
			 | SOME _ => default)))))
	end

  | mcor (arg as ((CONpat (DATACON{tycon=ATOMtyc _,...}),_,_)::_)) = (* exception *)
	let val x = mkLvar()
	val (switch,default) = ConSwitch arg x
	 in AreNotRedundant();
	    FN(x,SWITCH(VAR x,switch,default))
	end

  | mcor (arg as ((APPpat (DATACON{tycon=ATOMtyc _,...},_),_,_)::_)) = (* exn *)
	let val x = mkLvar()
	val (switch,default) = ConSwitch arg x
	 in AreNotRedundant();
	    FN(x,SWITCH(VAR x,switch,default))
	end

  | mcor (arg as ((INTpat _,_,_)::_)) =
	let val x = mkLvar()
	val (switch,default) = OrSwitch arg x
	in 
		FN(x,SWITCH(VAR x,untag switch,default))
	end

  | mcor (arg as ((REALpat _,_,_)::_)) =
	let val x = mkLvar()
	val (switch,default) = OrSwitch arg x
	in
		FN(x,SWITCH(VAR x,untag switch,default))
	end

  | mcor (arg as ((STRINGpat _,_,_)::_)) =
	let val x = mkLvar()
	val (switch,default) = OrSwitch arg x
	in
		FN(x,SWITCH(VAR x,untag switch,default))
	end

  | mcor (arg as ((RECORDpat{pats=ref nil,...},result,tag)::_)) =
		(MarkNotRedundant tag;
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
	 in
		FN(x,header x newfields (mcand(opt(convert arg,newfields))))
	end
  | mcor ((WILDpat,result,tag)::_) =
	let val x = mkLvar()
	in MarkNotRedundant tag;
	   FN(x,result)
	end
  | mcor ((VARpat(VALvar{access=LVAR v,...}),result,tag)::_) =
	let val x = mkLvar()
	in MarkNotRedundant tag;
	   FN(x,bind(x,v,result))
	end
  | mcor ((LAYEREDpat(v,p),result,tag)::pats) =
	let val x = mkLvar()
	in
		FN(x,APP(mcor((p,layer(x,v,result),tag)::pats),VAR x))
	end
  | mcor ((CONSTRAINTpat(p,_),result,tag)::pats) =
	mcor ((p,result,tag)::pats)
  | mcor _ = Impossible "4777 type error in match compiler"

fun addtag rules =
	let fun tag ((pat,result)::more) i =
			(pat,result,i) :: tag more (i+1)
	      | tag nil _ = nil
	in tag rules 0
	end

fun printbind ((pat,_)::_) = (prstr "\t"; printPat pat; prstr " = ...\n")

fun printmatch nil _ _ = ()
  | printmatch ((pat,_)::more) nil _ =
	(prstr "\t"; printPat pat; prstr " => ...\n";
	 printmatch more nil 0)
  | printmatch ((pat,_)::more) (taglist as (tag::tags)) i =
	if i = tag
	then (prstr "  -->\t"; printPat pat; prstr " => ...\n";
	      printmatch more tags (i+1))
	else (prstr "\t"; printPat pat; prstr " => ...\n";
	      printmatch more taglist (i+1))

fun NoVarsIn ((pat,_)::_) =
	let fun Var (VARpat _) = true
	      | Var (LAYEREDpat _) = true
	      | Var (CONSTRAINTpat(p,_)) = Var p
	      | Var (APPpat(_,p)) = Var p
	      | Var (RECORDpat{pats=ref patlist,...}) = exists (Var,patlist)
	      | Var _ = false
	in not (Var pat)
	end

fun mc rules bindormatch = 
let val {PrintArgs,PrintRet,VarBindCheck,
	 RedundantCheck,ExhaustiveCheck,...} = Debug
    fun ReportErr () = 
    let val unused = Redundant rules
	val ReportVar = (bindormatch = BIND) andalso (!VarBindCheck)
			 andalso NoVarsIn rules
	fun saykind MATCH  = "match" | saykind BIND = "binding"
    in if Exhaustive () orelse (not (!ExhaustiveCheck))
          then ()
          else Warn (saykind bindormatch ^ " not exhaustive");

       if null unused orelse (not (!RedundantCheck))
       then ()
       else Warn ("redundant patterns in " ^ saykind bindormatch);

       if ReportVar
       then Warn "binding contains no variables"
       else ();

       if (Exhaustive () orelse (not (!ExhaustiveCheck)))
	  andalso (null unused orelse (not (!RedundantCheck)))
	  andalso (not ReportVar)
       then ()
       else case bindormatch of
		  MATCH => printmatch rules unused 0
		| BIND => printbind rules
    end
in
    if (!PrintArgs)
    then (Warn "MC called with:"; printMatch rules)
    else ();

    ResetRedundant();
    ResetExhaustive();
    (case bindormatch of
	  BIND => MakeBind ()
	| MATCH => MakeMatch ());
    let val patmatch = mcor (addtag rules)
    in
	ReportErr ();

	if (!PrintRet)
	then (prstr "MC:  returns with\n"; printLexp patmatch; newline())
	else ();

	patmatch
    end
end
  

end (* struct MC *)
