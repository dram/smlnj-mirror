signature DEBUGINSTRUM = sig
   val instrumDec: Absyn.dec * (int->string*int*int) -> Absyn.dec
   val quick: bool ref
   val commit: unit -> unit
   val reset:unit -> unit
   exception NoSuchEvent
   val locOfEvent: DebugStatic.event  * (int->string*int*int) -> (string*int)
   val argCnt: DebugStatic.event -> int
   val getEvnAt: DebugStatic.location -> int
end

structure DebugInstrum : DEBUGINSTRUM =
struct

open DebugUtil DebugStatic Access Absyn Basics BasicTypes ErrorMsg

val quick = ref false

val currentCud = ref initialCud
val nextCud = ref initialCud

(* LocSet handling *)

structure LocSet = SortedSet (
struct
  type t = (string * int) * (int list)
  type k = string * int
  fun key (k,d) = k
  fun lt((ast,ai),(bst,bi)) = String.<(ast,bst) orelse ((ast = bst) andalso
		Integer.<(ai,bi))
end)

local 
   open LocSet
in

val emptyLocSet = new()
val currentLocSet = ref emptyLocSet
val nextLocSet = ref emptyLocSet

fun locOfEvent(evt,cp2lnf) =
  let fun f x = 
        let val (filename,lineno,_) = cp2lnf x
        in (filename,lineno)
      end
  in
    case evt of
      VALev(MARKdec(_,s,_)) => f s
    | VALRECev(MARKdec(_,s,_)) => f s
    | FNev(RULE(_,MARKexp(_,s,_))) => f s
    | CASEev(_,RULE(_,MARKexp(_,s,_))) => f s
    | APPev(APPexp(exp,_)) =>
	let fun g (MARKexp(_,s,_)) = f s
	      | g (APPexp(opr,_)) = g opr
	      | g _ = debugPanic "bad APPev marking in instrum.locOfEvent"
        in g exp
        end
    | STRev(MARKdec(_,s,_),_) => f s
    | ABSev(MARKdec(_,s,_),_) => f s
    | FCTev(MARKdec(_,s,_),_) => f s
    | FCTENTev(FCTB{def=MARKstr(_,s,_),...}) => f s
    | FCTAPPev(MARKstr(_,s,_)) => f s
    | STRENDev(MARKstr(_,s,_)) => f s
    | STRVARev(MARKstr(_,s,_)) => f s
    | OPENev(MARKdec(_,s,_)) => f s 
    | LETev(MARKexp(_,s,_)) => f s
    | LOCALev(MARKdec(_,s,_)) => f s
    | LOCINev(MARKdec(_,s,_)) => f s
    | LOCENDev(MARKdec(_,s,_)) => f s
    | IOev => ("",0)
    | NULLev => ("",0)
    | _ => debugPanic "bad event type in instrum.locOfEvent"
  end

fun augmentLocSet (locset,cp2lnf,firstev,evcount,events)=
  let val locIndex = array(evcount, ("",0,0))
      fun d (evn,locset) =           
        let val (filename,lineno) = locOfEvent (hd (events sub evn),cp2lnf)
	    val oldevl = #2 (find (locset,(filename,lineno)))
				handle NotFound => nil
            val evl = oldevl @ [firstev + evn]
	    val lsentry = ((filename,lineno), evl)
            val seqno = length evl
	    val locset' = (update (locset,lsentry))
				handle NotFound => insert(locset,lsentry)
	in Array.update(locIndex, evn, (filename,lineno,seqno));
	   d (evn+1, locset') handle Subscript => (locset',locIndex)
	end 
  in d (0,locset)
  end

exception NoSuchEvent

fun getEvnAt (filename,lineno,seqno) =
      let val (_,evns) = find (!nextLocSet,(filename,lineno))
				handle NotFound => raise NoSuchEvent
      in
	(nth (evns,seqno-1)) handle Nth => raise NoSuchEvent
      end
end (* let open structure LocSet *)


fun argCnt evt =
  case evt of 
    VALev(MARKdec(VALdec vbl,_,_)) => length (vblextract (fn x=>x) vbl)
  | VALRECev(MARKdec(VALRECdec rvbl,_,_)) => length rvbl
  | FNev(RULE(pat,_)) => length (patvars (fn x => x) pat)
  | CASEev(_,RULE(pat,_)) => length (patvars (fn x => x) pat)
  | STRev(_) => 1
  | ABSev(_) => 1
  | _ => 0
   
val toaccess = fn (VALvar{access=LVAR v,name,typ}) =>
		    VARexp(ref (VALvar{access=PATH[v],name=name,typ=typ}))

fun makevar(str,t) = 
     let val name = Symbol.symbol str
	 val lvar = namedLvar name
     in (VALvar{name=[name],access=LVAR(lvar),typ=ref t},
	 VALvar{name=[name],access=PATH[lvar],typ=ref t})
     end

fun anonstrb def = 
     let val name = Symbol.symbol "AnonStruct"
	 val lvar = namedLvar name
     in (STRvar{name=[name],access=PATH[lvar],binding=NULLstr},
         STRB{strvar=STRvar{name=[name],access=LVAR(lvar),binding=NULLstr},
	      def=def,thin=NONE,constraint=NONE})
     end


val alpha = VARty(mkTyvar(IBOUND 0))

val intreftype = CONty(refTycon,[intTy])
val intarraytype = CONty(arrayTycon, [intTy])	(* ?? *)
val objectarraytype = CONty(arrayTycon, [alpha]) (* ??????? *)

val ubassop = VALvar{name= [Symbol.symbol "unboxedassign"],
		    access=INLINE(P.unboxedassign),
		    typ=ref (tupleTy([intreftype,intTy]) --> unitTy)}
val assop = VALvar{name=[Symbol.symbol ":="],
		   access=INLINE(P.:=),
		   typ=ref (tupleTy([CONty(refTycon,[alpha]),alpha]) --> unitTy)}
val subop = VALvar{name=[Symbol.symbol "subscript"],
		   access=INLINE(P.subscript),
		   typ=ref (tupleTy[CONty(arrayTycon,[alpha]),intTy] --> alpha)}  (* ?? *)
val derefop = VALvar{name= [Symbol.symbol "!"],
		    access=INLINE(P.!),
		    typ= ref (CONty(refTycon,[alpha]) --> alpha)}
val addop = VALvar{name=[Symbol.symbol "iadd"],
		    access=INLINE(P.+),
		    typ=ref (tupleTy([intTy,intTy]) --> intTy)}
val updateop = VALvar{name=[Symbol.symbol "unboxedupdate"],
		      access=INLINE(P.unboxedupdate),
	 	      typ = ref(tupleTy([CONty(arrayTycon,[alpha]),intTy,alpha]) --> unitTy)} (* ?? *)
val ineqop = VALvar{name=[Symbol.symbol "ineq"],
		    access=INLINE(P.ineq),
		    typ = ref(tupleTy([intTy,intTy]) --> boolTy)}
val delayop = VALvar{name= [Symbol.symbol "delay"],
		    access=INLINE(P.delay),
		    typ= ref UNDEFty}


fun instrumDec(absyn,cp2lnf) =
let 
val {firstev,evcount,lastbindev,...} = !currentCud
val firstev' = firstev + evcount  (* first event number to use *)
val evNum = ref firstev'	(* next event number to use *)
val evList = ref ([]: event list list)
val elbList = ref ([]: int list)

fun makeevent (ev,elb) =
  (evList := ev::(!evList);
   elbList := elb::(!elbList);
   !evNum before
   inc evNum)

val (timesvarb, timesvara) = makevar ("debugtimes", intreftype)
val (eventtimesb, eventtimesa) = makevar ("eventtimes", intarraytype)
val (breakentryb, breakentrya) = makevar ("breakentry", objectarraytype --> unitTy)
val (arrayb, arraya) = makevar ("array", UNDEFty)

val tacount = ref 0	(* for cosmetics only *)
fun maketimearr len =
  let val (timearrb,timearra) = makevar("TIMEARR"^makestring(!tacount),UNDEFty)
  in inc tacount;
     (VALdec[VB{pat=VARpat(timearrb),
		exp=APPexp(VARexp(ref arraya),
			   TUPLEexp[INTexp len,
				    INTexp 0]),
		tyvars=nil}],
      VARexp (ref timearra))
  end

val GETTIMEexp = APPexp(VARexp(ref subop), 
			TUPLEexp[VARexp(ref timesvara),
		                 INTexp 0])

val BUMPEDTIMEexp = APPexp(VARexp(ref addop),
			   TUPLEexp[GETTIMEexp,
				    INTexp 1])

val ntcount = ref 0  (* for cosmetics only *)
fun makent label = 
  let val (ntb,nta) = makevar (label ^ makestring(!ntcount), intTy)
  in inc ntcount;
     (VALdec[VB{pat=VARpat ntb,
		exp=BUMPEDTIMEexp,
		tyvars=nil}],
      VARexp (ref nta))
  end

fun SETTIMEexp ntimeexp = APPexp(VARexp(ref updateop),
				 TUPLEexp[VARexp(ref timesvara),
					  INTexp 0,
					  ntimeexp])
							   
fun NOTETIMEexp(ntimeexp,evn) = APPexp(VARexp (ref updateop), 
			               TUPLEexp[VARexp(ref eventtimesa),
				                INTexp (evn-firstev'),
				       		ntimeexp])


fun BREAKexp(ntimeexp, evn, args, contexp) = 
      CASEexp(APPexp(VARexp(ref ineqop), 
		     TUPLEexp[ntimeexp, 
		              APPexp(VARexp(ref subop), 
				     TUPLEexp[VARexp(ref timesvara),
					      INTexp 1])]),
	      [RULE(truePat, contexp),
	       RULE(falsePat, SEQexp[APPexp(VARexp (ref breakentrya),
				            TUPLEexp(INTexp evn::args)),
				     contexp]),
	       RULE(WILDpat, INTexp 0)])

fun EVENTexp (evn,lbt,args) = 
     let val (ntimedec,ntimeexp) = makent "newtime"
     in LETexp (ntimedec,
		SEQexp [SETTIMEexp ntimeexp,
			BREAKexp(ntimeexp,evn,lbt::args,
				 NOTETIMEexp(ntimeexp,evn)),
			ntimeexp])
     end

(* The following ref creation and update stuff is done inside the instrumenter
    because ref is a constructor (so cannot be replaced at top level) and
    for efficiency.

we emulate:
fun href (i,obj) = hcreate(1, obj)
fun hass (objr,value) =
        (updatedRList := (weak objr) :: (!updatedList);
	 objr := value)
*)

val (hcreateb, hcreatea) = makevar ("hcreate", UNDEFty)

fun HREFexp objexp =
	APPexp(VARexp(ref hcreatea), TUPLEexp[INTexp 1, objexp])


val (weakb,weaka) = makevar ("weak",UNDEFty)
val (udlb,udla) = makevar ("udl",UNDEFty)

fun HASSexp (opr, objexp, valexp) = 
     let(* val newobj = APPexp(VARexp(ref weaka),objexp)  *)
         val newobj = APPexp(VARexp(ref delayop), 
			     TUPLEexp[INTexp 22,objexp]) 
	 val oldlist = APPexp(VARexp(ref derefop),VARexp(ref udla))
     in
	SEQexp[APPexp(VARexp(ref assop),
	              TUPLEexp[VARexp(ref udla),
			       APPexp(CONexp(consDcon),
				      TUPLEexp[newobj,oldlist])]),
	       APPexp(VARexp(ref opr), 
		      TUPLEexp[objexp,valexp])]
     end

  
fun simplebind ((btexp,bev,bsites,bvars),ev) =
       let val evn = makeevent(ev::bsites,bev)
	   val (bevb,beva) = makevar("BIND" ^ makestring evn,intTy)
	   val evndec = VALdec[VB{pat=VARpat(bevb),
		                    exp=EVENTexp(evn,btexp,bvars),
				    tyvars=nil}]
       in ((VARexp(ref beva),evn,nil,nil),evndec)
       end

fun instrexp(ntexp:exp,
	     b as (btexp:exp,bev:int,bsites:event list,bvars:exp list),
	     exp:exp) : bool * (exp * int * event list * exp list) * exp =
  let 
    fun instrrule (b as (btexp,bev,bsites,bvars), ev, 
			rule as RULE(pat,exp as MARKexp(_,_,_))) = 
          let val vars = (patvars toaccess pat)
	      val (ntdec,ntexp) = makent "NEXTTIME"
	      val (d',b' as (_,_,bsites',bvars'),exp') = 
			instrexp (ntexp,
				  (btexp,bev,ev::bsites,vars @ bvars),
				  exp)
	  in if d' then
	       RULE (pat,LETexp (ntdec,
				 exp'))
	     else (* insert backstop event for rule body *)
               let val evn = makeevent(bsites', bev)
	       in RULE (pat, LETexp(VALdec[VB{pat=WILDpat,
					      exp=EVENTexp(evn,btexp,bvars'),
					      tyvars=nil}],
				    LETexp (ntdec,
				            exp')))
               end
	  end
      | instrrule (b, ev, rule as RULE(pat,exp)) = 
	  let val (_,b',exp') = instrexp(ntexp,b,exp)
	  in RULE(pat,exp')
	  end
    and instr (RECORDexp l) = 
          let fun g ((lab,exp)::rest,d,b,acc) =
		      let val (d',b',exp') = instrexp(ntexp,b,exp)
		      in g (rest,d orelse d',b',(lab,exp') :: acc)
		      end
		| g (nil,d,b,acc) = (d,b,RECORDexp(rev acc))
	  in g (l,false,b,nil)
	  end
      | instr (SEQexp expl) =
	  let fun g (exp::rest,d,b,acc) =
		      let val (d',b',exp') = instrexp(ntexp,b,exp)
		      in g(rest,d orelse d',b',exp'::acc)
		      end
		| g (nil,d,b,acc) = (d,b,SEQexp(rev acc))
	  in g (expl,false,b,nil)
	  end
      | instr (exp as APPexp(opr,arg)) =
	  let fun strip (MARKexp(exp,_,_)) = strip exp
		| strip exp = exp
	      fun newb (b as (_,_,nil,_),_) = b
      	        | newb (_,evn) = (ntexp,evn,nil,nil)
	  in case (strip opr) of
	       VARexp(ref(assopr as VALvar{access=INLINE(P.:=),...})) =>
		 let val RECORDexp[(_,objexp),(_,valexp)] = strip arg
	             val (_,b',objexp') = instr objexp
    	             val (_,b' as (btexp',bev',bsites',bvars'),valexp') = 
				instrexp (ntexp,b',valexp)
                     val (objtmpb,objtmpa) = makevar ("objvar",UNDEFty)
	             val (valtmpb,valtmpa) = makevar ("valvar",UNDEFty)
	             val evn = makeevent(APPev exp::bsites',bev')
	         in (true,
		     newb (b',evn),
		     LETexp(VALdec [VB{pat=VARpat(objtmpb),
				      exp=objexp',
				      tyvars=nil},
				   VB{pat=VARpat(valtmpb),
				      exp=valexp',
				      tyvars=nil}],
			    SEQexp [EVENTexp(evn,btexp',bvars'),
				    HASSexp(assopr,
					    VARexp(ref objtmpa),
					    VARexp(ref valtmpa))]))
		 end
	     | VARexp(ref(VALvar{access=INLINE _,...})) =>
		 let val (d',b',arg') = instr arg
	         in (d',b',APPexp(opr,arg'))
                 end
	     | CONexp(DATACON{rep=REF,...}) =>
	         let val (_,b' as (btexp',bev',bsites',bvars'),arg') = 
				instr arg
                     val (argtmpb, argtmpa) = makevar ("argvar", UNDEFty)
                     val evn = makeevent (APPev exp::bsites',bev')
	         in (true,
	             newb (b',evn),
	             LETexp (VALdec ([VB{pat = VARpat(argtmpb),
			   	         exp = arg',
				         tyvars = nil}]),
                             SEQexp [EVENTexp(evn,btexp',bvars'),
		                     HREFexp(VARexp(ref argtmpa))]))
                 end
	     | CONexp _ =>
		 let val (d',b',arg') = instr arg
	 	 in  (d',b',APPexp(opr, arg'))
	  	 end
             | FNexp body =>  (* really a CASE or equivalent *)
		 let val (_,b',arg') = instr arg
		     val body' = map (fn r => instrrule(b',CASEev(arg,r),r))
							 body
		 in (true,b',APPexp(FNexp body',arg'))
		 end
	     | _ =>
		 let val (_,b',opr') = instr opr
		     val (_,b' as (btexp',bev',bsites',bvars'),arg') = 
			       instrexp (ntexp,b',arg)
		     val (oprtmpb, oprtmpa) = makevar ("oprvar",UNDEFty)
		     val (argtmpb, argtmpa) = makevar ("argvar",UNDEFty)
		     val evn = makeevent(APPev exp::bsites',bev')
		 in (true,
		     newb (b',evn),
		     LETexp (VALdec ([VB{pat = VARpat(oprtmpb), 
					exp = opr', 
					tyvars = nil},
				     VB{pat = VARpat(argtmpb), 
					exp = arg', 
					tyvars = nil}]),
			    SEQexp ([EVENTexp(evn, btexp',bvars'), 
				     APPexp(VARexp(ref oprtmpa), 
					    VARexp(ref argtmpa))])))
		 end
	  end
      | instr (CONSTRAINTexp (e,c)) =
	  let val (d',b',e') = instr e
	  in (d',b',CONSTRAINTexp (e',c))
	  end
      | instr (HANDLEexp (e, HANDLER(h))) = 
          let val (d',b',e') = instr e
	      val (_,_,h') = instr h
	  in (d',b',HANDLEexp (e', HANDLER(h')))
          end
      | instr (RAISEexp e) = 
	  let val (d',b',e') = instr e
	  in (d',b',RAISEexp(e'))
	  end
      | instr (exp as MARKexp(LETexp (ldec,lexp),_,_)) = 
		(* note: must be marked *)
	  let val (b' as (btexp',bev',bsites',bvars'),ldec') =
			 instrdec (b,ldec)
	      val (ntdec,ntexp) = makent "NEXTTIME"
	      val (d',_,lexp') = 
			instrexp (ntexp,b',lexp)
	      val exp' = 
		if d' then 
		  LETexp (SEQdec [ldec',ntdec],lexp')
	        else
		  let val evn = makeevent(LETev exp::bsites', bev')
		  in LETexp (SEQdec [ldec',
				     VALdec[VB{pat=WILDpat,
		  			       exp=EVENTexp(evn,btexp',bvars'),
					       tyvars=nil}],
				     ntdec],
			     lexp')
		  end
	  in (true,b,exp')
	  end
      | instr (CASEexp(exp,rl)) = 
	  let val (_,b',exp') = instr exp
	      val rl' = map (fn r=> instrrule (b',CASEev(exp,r),r)) rl
	  in (true,b',CASEexp(exp', rl'))
	  end
      | instr (FNexp body) =
	  let val body' = map (fn r => instrrule (b,FNev r,r)) body
	  in (false,b,FNexp body')
	  end
      | instr (MARKexp (exp,s,e)) =
	  let val (d',b',exp') = instr exp
	  in (d',b', MARKexp(exp',s,e))
          end
      | instr exp = (false,b,exp)
  in
    instr exp
  end


and instrdec (b as (btexp:exp, bev:int, bsites:event list,bvars: exp list),
	     dec:dec) : 
	((exp * int * (event list) * (exp list)) * dec) =
  let
    fun instr (dec as MARKdec(VALdec vbl,_,_)) =
	    let val (ntdec,ntexp) = makent "NEXTTIME"
		val vars = vblextract toaccess vbl
	        fun g (VB{pat,exp,tyvars}::rest,b,acc) =
			let val (_,b',exp') = instrexp (ntexp,b,exp)
			in g (rest,b',VB{pat=pat,exp=exp',tyvars=tyvars}::acc)
			end
		  | g (nil,(btexp',bev',bsites',bvars'),acc) =
			     ((btexp',bev',VALev dec :: bsites',vars @ bvars'),
				SEQdec[ntdec, VALdec (rev acc)])
	    in g (vbl,b,nil)
	    end
      | instr (dec as MARKdec(VALRECdec rvbl,_,_)) =
            (* N.B. Bodies cannot discharge vars ! *)
	    let val vars =(map (fn RVB{var=VALvar{access=LVAR(var),name,typ},...} => 
				VARexp(ref (VALvar{access=PATH[var],name=name,typ=typ}))) rvbl)
		val b' = (btexp,bev,VALRECev dec::bsites,vars @ bvars)
		val rvbl' = map (fn RVB{var,exp,resultty,tyvars} => 
				     let val (_,_,exp') = instrexp(INTexp 0,b',exp)
				     in RVB {var=var,
					     exp=exp',
					     resultty=resultty,
					     tyvars=tyvars}
				     end) rvbl

	    in (b',VALRECdec rvbl')
	    end
      | instr (SEQdec decl) =
	    let val (b', decl') = instrlist (b, decl)
	    in (b', SEQdec decl')
	    end
      | instr (dec as MARKdec(LOCALdec(localdec, visibledec),_,_)) = 
	    let val (b' as (btexp',bev',bsites',bvars'), localdec') = 
		   instrdec ((btexp,bev,LOCALev dec::bsites,bvars),localdec)
		val (b'' as (btexp'',bev'',bsites'',bvars''), visibledec') =
		   instrdec ((btexp',bev',LOCINev dec::bsites',bvars'),visibledec)
	    in if exists (fn LOCINev _ => true | _ => false) bsites'' then
		 let val (b',indec) = simplebind(b',LOCINev dec)
		     val (b'' as (btexp'',bev'',bsites'',bvars''),visibledec')=
				 instrdec (b',visibledec)
		 in ((btexp'',bev'',LOCENDev dec::bsites'',bvars''),
   	  	     LOCALdec(localdec',SEQdec[indec,visibledec']))
		 end
	       else 
		 ((btexp'',bev'',LOCENDev dec::bsites'',bvars''),
	          LOCALdec(localdec',visibledec'))
	    end
      | instr (ABSTYPEdec {abstycs,withtycs,body}) =
	    let val (b', body') = instr body
	    in (b',ABSTYPEdec {abstycs=abstycs,withtycs=withtycs,body=body'})
	    end
      | instr (dec as MARKdec(STRdec strbl,_,_)) =
            let val (timearrdec,timearrexp) = maketimearr (length strbl)
	        val (bevl,strbl') = instrstrbl timearrexp strbl
		val dec' = SEQdec [timearrdec, STRdec strbl']
	    in ((btexp,
		 bev,
                 STRev (dec,bevl)::bsites,
		 timearrexp::bvars), 
                dec')
	    end
      | instr (dec as MARKdec(ABSdec strbl,_,_)) =
            let val (timearrdec,timearrexp) = maketimearr (length strbl)
	        val (bevl,strbl') = instrstrbl timearrexp strbl
		val dec' = SEQdec [timearrdec, ABSdec strbl']
	    in ((btexp,
		 bev,
                 ABSev (dec,bevl)::bsites,
		 timearrexp::bvars), 
                dec')
	    end
      | instr (dec as MARKdec(FCTdec fctbl,_,_)) =
	    let 
              fun instrfctb(fctb as FCTB{fctvar,param,def,thin,constraint}) =
                let val (b',entdec) = simplebind(b,FCTENTev fctb)
                    val (bev,def') = instrstrexp(b',def)
                in (bev, FCTB{fctvar=fctvar,param=param,
		                 def=LETstr(entdec,def'),
		                 thin=thin,constraint=constraint})
                end
	      val (bevl,fctbl') = unpairlist (map instrfctb fctbl)
	    in ((btexp,bev,FCTev (dec,bevl)::bsites,bvars),FCTdec fctbl')
            end
      | instr (dec as MARKdec(OPENdec _,_,_)) =
	    ((btexp,bev,OPENev dec::bsites,bvars),dec)
      | instr (MARKdec (dec,s,e)) = 
	    let val (b',dec') = instr dec
	    in (b', MARKdec (dec',s,e))
	    end
      | instr dec = (b, dec)

   and instrstrbl timearrexp strbl = 
         let 
           fun dostrb (STRB{strvar,def,thin,constraint},(n,bevl,strbl))= 
	        let val (bev',def') = instrstrexp(b,def)
		    val strb' = STRB{strvar=strvar,def=def',thin=thin,
				       constraint=constraint}
		    val (_,timestrb) = anonstrb (STRUCTstr
		           {body=[VALdec[VB{pat=WILDpat,
				            exp=APPexp(VARexp(ref updateop),
					  	       TUPLEexp[timearrexp,
						   	        INTexp n,
						   		GETTIMEexp]),
					    tyvars=nil}]],
			    locations=nil,
			    str=NULLstr})
		in (n-1,
		    bev' :: bevl,
	            strb' :: (timestrb :: strbl))
		end
	   val (_,bevl,strbl') = fold dostrb strbl (length strbl - 1,[],[])
	 in (bevl,strbl')
         end
    in instr dec
   end

and instrstrexp (b,mstrexp as MARKstr(strexp,_,_)) =
     (case strexp of 
        STRUCTstr{body,locations,str} =>
          let val (b',body') = instrlist(b,body)
              val ((_,bev',_,_),enddec) = 
       	       simplebind(b',STRENDev mstrexp)
          in (bev',STRUCTstr{body=body'@[enddec],locations=locations,
       			str=str})
          end
      | APPstr{oper,argexp,argthin,str} => 
          let val (_,argexp') = instrstrexp (b,argexp)
              val ((_,bev',_,_),appdec) = 
       		simplebind(b,FCTAPPev mstrexp)
              val (strvara, strb) = anonstrb argexp'
          in (bev',
              LETstr(SEQdec[STRdec[strb],appdec], 
       	      APPstr{oper=oper,argexp=VARstr strvara,
       		     argthin=argthin,str=str}))
          end
      | LETstr(dec,strexp) =>
          let val (b',dec') = instrdec(b,dec)
              val (bev',strexp') = instrstrexp(b',strexp)
          in (bev',LETstr(dec',strexp'))
          end
      | VARstr _ =>
          let val ((_,bev',_,_),defdec) = 
       		simplebind(b,STRVARev mstrexp)
          in (bev',LETstr(defdec,strexp))
          end
      | MARKstr _ => debugPanic "instrstrexp: double MARKstr")
  | instrstrexp (_)= debugPanic "instrstrexp: unmarked strexp"

and instrlist (b, decl) = 
   let fun g (dec::rest, b, acc) =
     		let val (b',dec') = instrdec(b,dec)
	        in g(rest,b',dec' :: acc)
	        end
	 | g (nil,b,acc) = (b,rev acc)
   in g (decl,b,nil)
   end

val (lastbindev',absyn') =
  let val ((btexp,bev,bsites,bvars),absyn'') = 
	     instrdec((INTexp(lastTime lastbindev),lastbindev,nil,nil),absyn)
      val evn = makeevent(NULLev::bsites,bev)
      val FINALexp =
           let val (ntimedec,ntimeexp) = makent "newtime"
           in LETexp (ntimedec,
		      SEQexp [SETTIMEexp ntimeexp,
			      APPexp(VARexp (ref breakentrya),
			             TUPLEexp([INTexp evn,btexp] @ bvars)),
			      NOTETIMEexp(ntimeexp,evn)])
     	   end
  in (evn,
      SEQdec [absyn'',
	      VALdec[VB{pat=WILDpat,
		        exp=FINALexp,
		        tyvars=nil}]])
  end


val evcount' = !evNum - firstev'
val events' = arrayoflist (rev (!evList))
val (locset',locindex') = 
	     augmentLocSet (!currentLocSet,cp2lnf,firstev',evcount',events') 

in
  nextCud := 
     {firstev = firstev',
      evcount = evcount',
      lastbindev = lastbindev',
      events = events',
      elb = arrayoflist (rev (!elbList)),
      eventtimes = array(evcount',0),
      locindex = locindex',
      cp2lnf = cp2lnf};
   nextLocSet := locset';
   addCud (!nextCud);
   if (!debugdebug) then
     (print "Entering cud ";print firstev'; print " "; 
      print evcount'; print "\n")
   else ();
   LOCALdec(VALdec [VB {pat = TUPLEpat [VARpat timesvarb,
					VARpat eventtimesb,
					VARpat breakentryb,
					VARpat hcreateb,
					VARpat weakb,
					VARpat udlb,
					VARpat arrayb],
                        exp = APPexp(VARexp(CoreInfo.getDebugVar),
			             INTexp (firstev')),
			tyvars = nil}],
            absyn')
end (* end fun instrumDec *)

fun commit () = (currentCud := !nextCud;
		 currentLocSet := !nextLocSet)

fun reset () = (currentCud := initialCud;
	        nextCud := initialCud;
		currentLocSet := emptyLocSet;
                nextLocSet := emptyLocSet)

end (* struct DebugInstrum *)

