(* DebugInstrum

   Instrument user cde.
   Instrumented code needs to gain access to a variety of structures at 
     run-time, some of them in a dirty fashion.  This is done via
     a special system ref set up in DebugInterface.

*)

signature DEBUGINSTRUM = sig
   val instrumDec: {absyn:Absyn.dec,
		    firstEvn:DebugStatic.evn,
		    lastBindEvn:DebugStatic.evn,
		    lastBindTime:DebugStatic.time} ->
         {absyn:Absyn.dec,
	  evCount:int,
          events:DebugStatic.event list array,
	  elb:DebugStatic.evn array}
   val instrumLevel : int ref
end

structure DebugInstrum : DEBUGINSTRUM =
struct

open Array List DebugUtil DebugStatic Access Absyn BasicTypes ErrorMsg
Variables Types Modules
infix 9 sub
   
val instrumLevel = ref 1  (* normal *) 
(* Possible values:
magnitude:
   1 - normal instrumentation, the old-fashioned way.
   2 - newfangled idea of worst case: 
         - no sequentialization analysis; 
	 - events at top of every function
   3 - another newfangled idea
   4 - and its variant.
sign: > 0 : normal
      < 0 : generate only lvaruses/defs at events; this is intended to
            be a (generous overestimate) of "anti-optimization" costs.
*)



fun makevar(str,t) = 
     let val name = Symbol.varSymbol str
	 val lvar = namedLvar name
     in VALvar{name=[name],access=PATH[lvar],typ=ref t}
     end

local
val count = ref 0
in
fun makenvar str =
     makevar(str ^ (makestring(!count)),UNDEFty)
     before inc count
end

fun fakeuse(name,lvar) = 
	VARexp(ref (VALvar{name=[name],access=PATH[lvar],typ=ref UNDEFty}))
	
fun anonstrvar ()  = 
     let val name = Symbol.strSymbol "AnonStruct"
	 val lvar = namedLvar name
     in STRvar{name=name,access=PATH[lvar],binding=ERROR_STR}
     end

val alpha = VARty(mkTyvar(IBOUND 0))

val intreftype = CONty(refTycon,[intTy])
val intarraytype = CONty(arrayTycon, [intTy])	(* ?? *)
val objectarraytype = CONty(arrayTycon, [alpha]) (* ??????? *)

(* val ubassop = VALvar{name= [Symbol.varSymbol "unboxedassign"],
		    access=INLINE(P.unboxedassign),
		    typ=ref (tupleTy([intreftype,intTy]) --> unitTy)} *)
val assop = VALvar{name=[Symbol.varSymbol ":="],
		   access=INLINE(P.:=),
		   typ=ref (tupleTy([CONty(refTycon,[alpha]),alpha]) --> unitTy)}
val subop = VALvar{name=[Symbol.varSymbol "subscript"],
		   access=INLINE(P.subscript),
		   typ=ref (tupleTy[CONty(arrayTycon,[alpha]),intTy] --> alpha)}  (* ?? *)
val derefop = VALvar{name= [Symbol.varSymbol "!"],
		    access=INLINE(P.!),
		    typ= ref (CONty(refTycon,[alpha]) --> alpha)}
val addop = VALvar{name=[Symbol.varSymbol "iadd"],
		    access=INLINE(P.+),
		    typ=ref (tupleTy([intTy,intTy]) --> intTy)}
val updateop = VALvar{name=[Symbol.varSymbol "unboxedupdate"],
		      access=INLINE(P.unboxedupdate),
	 	      typ = ref(tupleTy([CONty(arrayTycon,[alpha]),intTy,alpha]) --> unitTy)} (* *?? *)
(* val ineqop = VALvar{name=[Symbol.varSymbol "ineq"],
		    access=INLINE(P.ineq),
		    typ = ref(tupleTy([intTy,intTy]) --> boolTy)} *)
val ieqlop = VALvar{name=[Symbol.varSymbol "ieql"],
		    access=INLINE(P.ieql),
		    typ = ref(tupleTy([intTy,intTy]) --> boolTy)}
val delayop = VALvar{name= [Symbol.varSymbol "delay"],
		    access=INLINE(P.delay),
		    typ= ref UNDEFty}
val useop = VALvar{name = [Symbol.varSymbol "mention"],
		   access=INLINE(P.uselvar),
		   typ = ref UNDEFty}
val defop = VALvar{name= [Symbol.varSymbol "define"],
                   access=INLINE(P.deflvar),
		   typ = ref UNDEFty}
(*
val compexp = APPexp(VARexp(ref VALvar{name = [Symbol.varSymbol "boxed"],
                                       access=INLINE(P.boxed)
				       typ = ref UNDEFty}),
		     REALexp "0.0")
*)

fun instrumDec {absyn:dec,firstEvn:evn,lastBindEvn:evn,lastBindTime:time} =

let 
val nextEvn = ref firstEvn (* next event number to use *)
val evList = ref ([]: event list list)
val elbList = ref ([]: int list)

fun makeevent (ev,elb) =
  (evList := ev::(!evList);
   elbList := elb::(!elbList);
   !nextEvn before
   inc nextEvn)

fun saveNextEvent () = (!evList,!elbList,!nextEvn)
fun restoreNextEvent (evList',elbList',nextEvn') = 
    (evList := evList'; elbList := elbList'; nextEvn := nextEvn')

val timesvar =  makevar ("debugtimes", intreftype)
val eventtimes = makevar ("eventtimes", intarraytype)
val breakentry = makevar ("breakentry", objectarraytype --> unitTy)
val arrayvar = makevar ("array", UNDEFty)

val tacount = ref 0	(* for cosmetics only *)
fun maketimearr len =
  let val timearr = makevar("TIMEARR"^makestring(!tacount),UNDEFty)
  in inc tacount;
     (VALdec[VB{pat=VARpat(timearr),
		exp=APPexp(VARexp(ref arrayvar),
			   TUPLEexp[INTexp len,
				    INTexp 0]),
		tyvars=nil}],
      VARexp (ref timearr))
  end

val GETTIMEexp = 
  if !instrumLevel > 0 then
    APPexp(VARexp(ref subop), 
	   TUPLEexp[VARexp(ref timesvar),
		    INTexp 0])
  else APPexp(VARexp(ref defop),unitExp)


fun INCexp exp = 
  if !instrumLevel > 0 then
    APPexp(VARexp(ref addop),
	   TUPLEexp[exp,
		    INTexp 1])
  else exp

val tcount = ref 0  (* for cosmetics only *)

fun makent label = 
  let val nt = makevar (label ^ makestring(!tcount), intTy)
  in inc tcount;
     (VALdec[VB{pat=VARpat nt,
		exp=INCexp(GETTIMEexp),
		tyvars=nil}],
      VARexp (ref nt))
  end


fun SETTIMEexp ntimeexp = APPexp(VARexp(ref updateop),
				 TUPLEexp[VARexp(ref timesvar),
					  INTexp 0,
					  ntimeexp])
							   

fun NOTETIMEexp(ntimeexp,evn) = APPexp(VARexp (ref updateop), 
			               TUPLEexp[VARexp(ref eventtimes),
				                INTexp (evn-firstEvn),
				       		ntimeexp])

fun BREAKexp(ntimeexp, evn, args) =
      CASEexp(APPexp(VARexp(ref ieqlop), 
		     TUPLEexp[ntimeexp, 
		              APPexp(VARexp(ref subop), 
				     TUPLEexp[VARexp(ref timesvar),
					      INTexp 1])]),
	      [RULE(truePat, APPexp(VARexp (ref breakentry),
				    TUPLEexp(INTexp evn::args))),
	       RULE(falsePat, unitExp),
	       RULE(WILDpat, INTexp 0)])

fun EVENTexp (evn,lbt,args) = 
  if !instrumLevel > 0 then
     let val (ntimedec,ntimeexp) = makent "Newtime"
     in LETexp (ntimedec,
		SEQexp [SETTIMEexp ntimeexp,
			BREAKexp(ntimeexp,evn,lbt::args),
			NOTETIMEexp(ntimeexp,evn),
			ntimeexp])
     end
  else 
    SEQexp ((map (fn arg => APPexp(VARexp(ref useop),arg)) (lbt::args))
	    @ [APPexp(VARexp(ref defop),unitExp)])

fun FEVENTexp (evn,lbt,args) =
  if !instrumLevel > 0 then
     let val (ntimedec,ntimeexp) = makent "Newtime"
     in LETexp (ntimedec,
	        SEQexp [SETTIMEexp ntimeexp,
		        APPexp(VARexp (ref breakentry),
			       TUPLEexp(INTexp evn::lbt::args)),
	                NOTETIMEexp(ntimeexp,evn),
			ntimeexp])
     end
  else 
    SEQexp ((map (fn arg => APPexp(VARexp(ref useop),arg)) (lbt::args))
	    @ [APPexp(VARexp(ref defop),unitExp)])

(* Because ref is a constructor that cannot be replaced at top level, 
    we replace it explicitly here: *)

val hcreater = makevar ("hcreater", UNDEFty)
val HREFexp = VARexp(ref hcreater)

(* For efficiency, we implement

fun hass (objr,value) =
        (updatedRList := (weak objr) :: (!updatedList);
	 objr := value)
	
in-line. Note we maintain type information in opr used by the 
garbage collector.
*)


val weakvar = makevar ("weak",UNDEFty)
val udl = makevar ("udl",UNDEFty)
fun HASSexp opr = 
  if !instrumLevel > 0 then
     let val objvar = makevar ("obj",UNDEFty)
	 val valvar = makevar ("val",UNDEFty)
	 val objexp = VARexp(ref objvar)
         val valexp = VARexp(ref valvar)
      (* val newobj = APPexp(VARexp(ref weakvar),objexp)  *)
         val newobj = APPexp(VARexp(ref delayop), 
			     TUPLEexp[INTexp (System.Tags.desc_weak div 2),
				      objexp]) 
	 val oldlist = APPexp(VARexp(ref derefop),VARexp(ref udl))
     in FNexp [RULE(TUPLEpat[VARpat objvar,VARpat valvar],
	            SEQexp[APPexp(VARexp(ref assop),
	                          TUPLEexp[VARexp(ref udl),
			                   APPexp(CONexp(consDcon),
				           TUPLEexp[newobj,oldlist])]),
	                   APPexp(VARexp(ref opr), 
		                  TUPLEexp[objexp,valexp])]),
	       RULE(WILDpat,INTexp 0)]
     end
  else VARexp(ref opr)

fun simplebind ((btexp,bev,bsites,bvars),ev,vars) =
       let val evn = makeevent(ev@bsites,bev)
	   val bevvar = makevar("BIND" ^ makestring evn,intTy)
	   val evndec = VALdec[VB{pat=VARpat(bevvar),
		                    exp=EVENTexp(evn,btexp,vars@bvars),
				    tyvars=nil}]
       in ((VARexp(ref bevvar),evn,nil,nil),evndec)
       end

fun hasfnsite (FNev _::_) = true
  | hasfnsite (HANDLEev _::_) = true
  | hasfnsite (_::rest) = hasfnsite rest
  | hasfnsite (nil) = false


datatype eventstatus =  (* info about events executed in evaluating expression,
		          in decreasing order of knowledge: *)
    FIRST_EV of evn     (* the first event executed reports all sites/vars,
	 	           doesn't add any new ones, and we know its evnum *)
  | SOME_EV             (* we know some event is always executed, but not
			  as above; we may not know its evnum *)
  | MAYBE_EV            (* we don't know if an event is executed or not *)
  | ABS_EV              (* we know that no event is executed, but some
			    event appears inside an abstraction *)
  | NO_EV               (* no event appears at all *)

type binddata = {btexp:exp,   (* representing time of last binding event *)
                 bev:evn,     (* event number of last binding event *)
		 bsites:event list, (* events to discharge *)
		 bvars:exp list}      (* variables to discharge *)


fun instrexp(b as (btexp:exp,   (* last binding time *)
		   bev:evn,     (* last binding event number *)
		   bsites:event list,   (* sites of events to discharge *)
		   bvars:exp list),     (* variables to discharge *)
             exp:exp)           (* expression to instrument *)
  :  (eventstatus *             (* as above *)
      exp)                      (* instrumented expression *)
=
  let 
    fun newbd (FIRST_EV evn,_) =
          let val (nxtdec,nxtexp) = makent "NEXTTIME"
          in ((nxtexp,evn,nil,nil),SOME nxtdec)
          end
      | newbd (_,b) = (b,NONE)
    fun newbdf (stat,b,false) = 
	  let val (b',d) = newbd(stat,b)
	  in (b',d,case d of SOME _ => true | _ => false)
	  end
      | newbdf (stat,b,true) = (b,NONE,true)
    val need_backstop =
       (fn (FIRST_EV _) => false
         | SOME_EV => false
         | MAYBE_EV => true
         | ABS_EV => true
         | NO_EV => true)
    val need_force = 
	(fn (FIRST_EV _) => false
	  | SOME_EV => true
	  | MAYBE_EV => true
	  | ABS_EV => false  (* handled otherwise *)
	  | NO_EV => false)
    val seqstat = 
      fn (NO_EV,stat) => stat
       | (ABS_EV,NO_EV) => ABS_EV
       | (ABS_EV,stat) => stat
       | (MAYBE_EV,SOME_EV) => SOME_EV
       | (MAYBE_EV,FIRST_EV _) => SOME_EV
       | (stat,_) => stat
    fun instr (RECORDexp l) = 
          let fun f ((lab as LABEL{name,...},exp)::rest,accv,accd) = 
 	          let val fieldtmp =
		              makevar ("field:" ^ (Symbol.name name), UNDEFty)
		  in f (rest,
		        (lab,VARexp(ref fieldtmp)) :: accv,
			(fieldtmp,exp) :: accd)
		  end
	        | f (nil,accv,accd) = (rev accv, rev accd)
              val (lv,ld) = f (l,nil,nil)
              fun g ((fieldtmp,exp)::rest,b,f,prevstat) =
	              let val (stat,exp') = instrexp(b,exp)
			  val dec' = VALdec [VB{pat=VARpat(fieldtmp),
				                exp=exp',
				                tyvars=nil}]
			  val (b',d,f') = newbdf(stat,b,f)
			  val stat' = seqstat(prevstat,stat)
		          val (rest',statback,statrest) =
			              g(rest,b',f',stat')
		      in (case (statrest,d) of
			    (NO_EV,_) => dec' :: rest'
			  | (_,SOME nxtdec) => nxtdec :: dec' :: rest'
			  | _ => dec' :: rest',
			  statback,
			  case stat of
			    NO_EV => statrest
			  | _ => stat)
		      end
		| g (nil,_,_,stat) = (nil,stat,NO_EV)
              val (ld',stat,_) = g(ld,b,false,NO_EV)
	      fun h ((lab,_)::rl,(VALdec[VB{exp,...}])::re) = 
		         (lab,exp)::(h(rl,re))
		| h (nil,nil) = nil
		| h _ = impossible "DebugInstrum.instrexp RECexp"
	      val exp' = (* avoid decl's when possible, for cosmetic reasons *)
                   if length ld' > length lv then
		       LETexp(SEQdec ld',RECORDexp lv)
		   else RECORDexp(h (lv,ld'))
	  in (stat,exp')
	  end
      | instr (SEQexp expl) =
	  let fun g(exp::rest,b,f,prevstat) =
	              let val (stat,exp') = instrexp(b,exp)
			  val (b',d,f') = newbdf (stat,b,f)
			  val stat' = seqstat (prevstat,stat)
			  val (rest',statback,statrest) = 
			              g(rest,b',f',stat')
		      in (case (statrest,d) of 
			    (NO_EV,_) => exp' :: rest'
			  | (_,SOME nxtdec) => 
				    [LETexp(nxtdec,SEQexp(exp' :: rest'))]
			  | _ => exp' :: rest',
			  statback,
			  case stat of 
			    NO_EV => statrest
			  | _ => stat)
		      end
	        | g(nil,_,_,stat) = (nil,stat,NO_EV)
	     val (expl',stat,_) = g (expl,b,false,NO_EV)
          in (stat,SEQexp expl')
	  end
      | instr (CONexp(DATACON{rep=REF,...})) = (NO_EV,HREFexp)
      | instr (VARexp(ref(assopr as VALvar{access=INLINE(P.:=),...}))) =
		(NO_EV,HASSexp assopr)
      | instr (exp as APPexp(opr,arg)) =
	  let fun strip (MARKexp(exp,_,_)) = strip exp
		| strip (CONSTRAINTexp(exp,_)) = strip exp
		| strip (SEQexp[exp]) = strip exp
		| strip exp = exp
	      fun normal () =
		 let val (oprstat,opr') = instr opr
		     val (b',d,f) = newbdf(oprstat,b,false)
		     val oprnxtdec = case d of
			               SOME nxtdec => [nxtdec]
				     | NONE => []
		     val (argstat,arg') = instrexp(b',arg)
		     val (b' as (btexp',bev',bsites',bvars'),d,_) = 
			 newbdf (argstat,b',f)
		     val argnxtdec = case d of 
			            SOME nxtdec => [nxtdec] 
				  | NONE => []
		     val evn = makeevent(APPev exp::bsites',bev')
		     val stat = seqstat(seqstat(oprstat,argstat),FIRST_EV evn)
		     val oprtmp = makevar ("oprvar",UNDEFty)
		     val argtmp = makevar ("argvar",UNDEFty)
		 in (stat,
		     LETexp (SEQdec (oprnxtdec @
				     [VALdec [VB{pat = VARpat(oprtmp), 
					         exp = opr', 
					         tyvars = nil}]] @
				     argnxtdec @
				     [VALdec [VB{pat = VARpat(argtmp), 
					         exp = arg', 
					         tyvars = nil}]]),
			    SEQexp [EVENTexp(evn, btexp',bvars'), 
				    APPexp(VARexp(ref oprtmp), 
					   VARexp(ref argtmp))]))
		 end
	  in case (strip opr) of
	       VARexp(ref(VALvar{access=INLINE(P.:=),...})) => normal()
	     | (opr as VARexp(ref(VALvar{access=INLINE prim,...}))) =>
		 let val (stat,arg') = instr arg
		     val (b' as (btexp',bev',bsites',bvars'),d) = 
			      newbd(stat,b)
		 in if (hasfnsite bsites') andalso (Prim.mayRaise prim) then
		      let val evn = makeevent(APPev exp::bsites',bev')
  		          val nxtdec = case d of 
			                 SOME nxtdec => [nxtdec] 
				       | NONE => []
			  val argtmp = makevar ("argvar", UNDEFty)
		      in (seqstat(stat,FIRST_EV evn),
			  LETexp (SEQdec (nxtdec @
					  [VALdec[VB{pat=VARpat(argtmp),
					             exp=arg',
					             tyvars=nil}]]),
				  SEQexp[EVENTexp(evn,btexp',bvars'),
					 APPexp(opr,
					        VARexp(ref argtmp))]))
		      end
		    else
		      (stat,APPexp(opr,arg'))
                 end
	     | CONexp(DATACON{rep=REF,...}) => normal()
	     | CONexp _ =>
		 let val (stat,arg') = instr arg
	 	 in  (stat,APPexp(opr, arg'))
	  	 end
             | FNexp body =>  (* really a CASE or equivalent *)
		 let val (stat,arg') = instr arg
		     val (b',d) = newbd(stat,b)
		     val body' = instrrules (b',fn r => CASEev(arg,r),false) body
		 in (SOME_EV,
		     case d of
		       SOME nxtdec => LETexp(nxtdec,APPexp(FNexp body',arg'))
		     | NONE => APPexp(FNexp body',arg'))
		 end
	     | _ => normal()
	  end
      | instr (CONSTRAINTexp (e,c)) =
	  let val (stat,e') = instr e
	  in (stat,CONSTRAINTexp (e',c))
	  end
      | instr (exp as MARKexp(RAISEexp arg,_,_)) = 
		(* N.B. Must be marked *)
	  let val (stat,arg') = instr arg
	      val (b' as (btexp',bev',bsites',bvars'),d) = newbd(stat,b)
	      val evn = makeevent(RAISEev exp::bsites',bev')
 	      val nxtdec = case d of 
		             SOME nxtdec => [nxtdec] 
			   | NONE => []
	      val argtmp = makevar ("argvar",UNDEFty)
	  in (seqstat(stat,FIRST_EV evn),
	      LETexp (SEQdec (nxtdec @
			      [VALdec [VB{pat = VARpat(argtmp),
				          exp = arg',
				          tyvars = nil}]]),
		      SEQexp [EVENTexp(evn,btexp',bvars'),
			      RAISEexp(VARexp(ref argtmp))]))
	  end
      | instr (exp as MARKexp(LETexp (ldec,lexp),_,_)) = 
		(* note: must be marked *)
	  let val (b' as (btexp',bev',bsites',bvars'),ldec') =
			 instrdec (b,ldec)
	      val (stat,lexp') = instrexp(b',lexp)
	      val exp' = 
		if (need_backstop stat) then
  		  let val evn = makeevent(LETev exp::bsites', bev')
		  in LETexp (SEQdec [ldec',
			             VALdec[VB{pat=WILDpat,
		  		               exp=EVENTexp(evn,btexp',bvars'),
					       tyvars=nil}]],
			     lexp')
		  end
	        else LETexp(ldec',lexp')
	  in (SOME_EV,exp')
	  end
      | instr (CASEexp(exp,rl)) = 
	  let val (stat,exp') = instr exp
	      val (b',d) = newbd(stat,b)
	      val rl' = instrrules (b',fn r => CASEev(exp,r),false) rl
	  in (SOME_EV,
	      case d of
		SOME nxtdec => LETexp(nxtdec,CASEexp(exp', rl'))
	      | NONE => CASEexp(exp',rl'))
	  end
      | instr (HANDLEexp (e, HANDLER(FNexp body))) =
	  if hasfnsite bsites then
	    let val evn = makeevent(bsites,bev)
		val (nxtdec,nxtexp) = makent "NEXTTIME"
		val b' = (nxtexp,evn,nil,nil)
                val (_,e') = instrexp (b',e)
     	        val body' = instrrules(b',HANDLEev,true) body
     	    in (FIRST_EV evn,
		LETexp(nxtdec,
		       SEQexp[EVENTexp(evn,btexp,bvars),
			      HANDLEexp(e',HANDLER(FNexp body'))]))
     	    end
          else
     	    let val (_,e') = instr e
     	        val body' = instrrules(b,HANDLEev,true) body
     	    in (MAYBE_EV, HANDLEexp(e',HANDLER(FNexp body')))
            end
      | instr (FNexp body) =
          if hasfnsite bsites then 
	    let val evn = makeevent(bsites,bev)
		val (nxtdec,nxtexp) = makent "NEXTTIME"
		val b' = (nxtexp,evn,nil,nil)
	        val body' = instrrules(b',FNev,true) body
	    in (FIRST_EV evn,LETexp(nxtdec,
				    SEQexp[EVENTexp(evn,btexp,bvars),
			                   FNexp body']))
	    end
	  else 
	    let val body' = instrrules (b,FNev,true) body
	    in (ABS_EV,FNexp body')
	    end
      | instr (MARKexp (exp,s,e)) =
          let val (stat,exp') = instr exp
          in (stat, MARKexp(exp',s,e))
          end
      | instr exp = (NO_EV,exp)
    and instrrules (b as (btexp,bev,bsites,bvars),evf,force) =
      let 
	(* Kludge: For FN and HANDLE events (force=true) we must avoid the
	   possibility that the FN/HANDLE site will be included in more
	   than one sequentially executed event.  To avoid this, we force
	   a distinct FN/HANDLE event if the initial instrumentation of 
	   the rule body has dangerous stat (SOME_EV or MAYBE_EV). *)
        fun f (rule as RULE(pat,exp as MARKexp(_))) = 
	     let val vars = (patvars (fn v => VARexp(ref v)) pat)
		 val bsites' = (evf rule)::bsites
		 val bvars' = vars@bvars
		 val savedEvent = saveNextEvent()
		 val (stat,exp') = instrexp((btexp,bev,bsites',bvars'),exp)
		      (* N.B. may be thrown away ! *)
	     in if force andalso need_force stat then
                  let val _ = restoreNextEvent savedEvent (* ugh !! *)
		      val evn = makeevent(bsites',bev)
		      val bevvar = makevar ("BIND" ^ makestring evn, intTy)
		      val (_,exp') = 
			       instrexp((VARexp(ref bevvar),evn,nil,nil),exp)
		  in RULE(pat, LETexp(VALdec[VB{pat=VARpat bevvar,
						exp=EVENTexp(evn,btexp,bvars'),
						tyvars=nil}],
				      exp'))
		  end
                else if need_backstop stat then
		  let val evn = makeevent(bsites', bev)
		  in RULE (pat,LETexp(VALdec[VB{pat=WILDpat,
						exp=EVENTexp(evn,btexp,bvars'),
						tyvars=nil}],
				      exp'))
		  end
		else RULE(pat,exp')
	     end
          | f (RULE(pat,exp)) =
	     let val (_,exp') = instrexp(b,exp)
	     in RULE(pat,exp')
             end
      in map f 
      end
  in
    instr exp
  end


and instrexp2(b as (btexp:exp,bev:evn,bsites:event list,bvars:exp list),
	      exp:exp) : 
     exp (* instrumented expression *) =
  let 
    fun instr (RECORDexp l) =
	  let fun f(lab,exp) = (lab,instr exp)
	  in RECORDexp (map f l)
	  end
      | instr (SEQexp expl) = SEQexp(map instr expl)
      | instr (CONexp(DATACON{rep=REF,...})) = HREFexp
      | instr (VARexp(ref(assopr as VALvar{access=INLINE(P.:=),...}))) =
		HASSexp assopr
      | instr (exp as APPexp(opr,arg)) =
	  let fun strip (MARKexp(exp,_,_)) = strip exp
		| strip (CONSTRAINTexp(exp,_)) = strip exp
		| strip (SEQexp[exp]) = strip exp
		| strip exp = exp
	      fun normal () =
		 let val opr' = instr opr
		     val arg' = instr arg
		     val evn = makeevent(APPev exp::bsites,bev)
		     val oprtmp = makevar ("oprvar",UNDEFty)
		     val argtmp = makevar ("argvar",UNDEFty)
		 in LETexp(VALdec[VB{pat = VARpat(oprtmp), 
				     exp = opr', 
				     tyvars = nil},
				  VB{pat = VARpat(argtmp), 
				     exp = arg', 
				     tyvars = nil}],
			   SEQexp [EVENTexp(evn,btexp,bvars), 
				   APPexp(VARexp(ref oprtmp), 
					  VARexp(ref argtmp))])
		 end
	  in case (strip opr) of
	       VARexp(ref(VALvar{access=INLINE(P.:=),...})) => normal()
	     | (opr as VARexp(ref(VALvar{access=INLINE prim,...}))) =>
		 let val arg' = instr arg
		 in APPexp(opr,arg')
                 end
	     | CONexp(DATACON{rep=REF,...}) => normal()
	     | CONexp _ => APPexp(opr, instr arg)
             | FNexp body =>  (* really a CASE or equivalent *)
		 let val arg' = instr arg
		     val body' = instrrules (b,fn r => CASEev(arg,r)) body
		 in APPexp(FNexp body',arg')
		 end
	     | _ => normal()
	  end
      | instr (CONSTRAINTexp (e,c)) = 
	  let val e' = instr e
	  in CONSTRAINTexp(e',c)
          end
      | instr (exp as MARKexp(RAISEexp arg,_,_)) =
		(* N.B. Must be marked *)
	  let val arg' = instr arg
	      val evn = makeevent(RAISEev exp::bsites,bev)
	      val argtmp = makevar ("argvar",UNDEFty)
	  in LETexp (VALdec [VB{pat = VARpat(argtmp),
				exp = arg',
				tyvars = nil}],
		     SEQexp [EVENTexp(evn,btexp,bvars),
			     RAISEexp(VARexp(ref argtmp))])
	  end
      | instr (exp as MARKexp(LETexp (ldec,lexp),_,_)) = 
		(* note: must be marked *)
	  let val (_,ldec') = instrdec (b,ldec)
	      val lexp' = instr lexp
  	      val evn = makeevent(LETev exp::bsites, bev)
	  in LETexp (SEQdec [ldec',
			     VALdec[VB{pat=WILDpat,
				       exp=EVENTexp(evn,btexp,bvars),
				       tyvars=nil}]],
		     lexp')
          end
      | instr (CASEexp(exp,rl)) = 
	  let val exp' = instr exp
	      val rl' = instrrules (b,fn r => CASEev(exp,r)) rl
	  in CASEexp(exp',rl')
	  end
      | instr (HANDLEexp (e, HANDLER(FNexp body))) =
     	    let val e' = instr e
     	        val body' = instrrules(b,HANDLEev) body
     	    in HANDLEexp(e',HANDLER(FNexp body'))
            end
      | instr (FNexp body) = 
	    let val body' = instrrules (b,FNev) body
	    in FNexp body'
	    end
      | instr (MARKexp (exp,s,e)) =
	    let val exp' = instr exp
	    in MARKexp(exp',s,e)
	    end
      | instr exp = exp
    and instrrules (b as (btexp,bev,bsites,bvars),evf) =
      let 
        fun f (rule as RULE(pat,exp as MARKexp(_))) = 
	     let val vars = (patvars (fn v => VARexp(ref v)) pat)
		 val bsites' = (evf rule)::bsites
		 val bvars' = vars@bvars
		 val evn = makeevent(bsites',bev)
		 val bevvar = makevar ("BIND" ^ makestring evn, intTy)
		 val exp' = instrexp2((VARexp(ref bevvar),evn,nil,nil),exp)
	     in RULE(pat, LETexp(VALdec[VB{pat=VARpat bevvar,
					   exp=EVENTexp(evn,btexp,bvars'),
					   tyvars=nil}],
				 exp'))
	     end
          | f (RULE(pat,exp)) =
	     let val exp' = instrexp2(b,exp)
	     in RULE(pat,exp')
             end
      in map f 
      end
  in 
    instr exp
  end

(* The idea of this one is as follows: for every instrumented expression
   we return the flag (d:bool), which is set iff the expr discharges
   (any and all) events on the bsites list (even if that list is empty).
   If d is true, the instrumented expression returns a pair (lbt,value);
   otherwise it just returns the value.
   It is important that every event containing a FNev is executed immediately
   after the event containing the matching APPev.  This implies that each
   event must contain at most one FNev, and that each FNev is contained
   in at most one event in any possible path through the instrumented code.
   We arrange the stronger condition that, if there is a FNev to be 
   discharged, every expression either discharges it or executes no 
   events at all; this means that forcing FNevs at the top of rules 
   should never be necessary.
   We do this by forcing events before LETs, HANDLEs and CASEs where necessary.
   (Note that this means a FNev can never be pushed inside a CASE or LET,
   lest we need to discharge the FNev again in a subsequent event.  This
   is unnecessaary if there are no subsequent events, but our present
   analysis is too stupid to notice this.)
   We also force discharge of FNevs before FNs, to prevent the two FNs in
   one list problem.
   The remaining issue is testing for backstop events in LET bodies and
   FN/CASE rule bodies.  Clearly, if the discharge flag is set, no backstop is
   needed; but this is not a sufficient test because
   LETs, CASEs, and HANDLEs all execute events (so no backstop is necessary),
   but don't in general discharge. (Actually, it is sufficient for FN
   rule bodies, where there will always be a FNev to discharge.)
   Tentative Soln #1: Strengthen the forced discharge before LET,CASE, and 
   HANDLE to insert an event if there are any events *at all* in the
   bsites list.  If we're in the body of a  LET (of any conceivable interest) 
   or a CASE (or of course a FN) there will be such an event.
   Then backstop needed can be equated with d false. THIS is level #3
   Tentative Soln #2: Just allow the extra backstop events. This may be
   a winner on the whole. This is level #4
   N.B. We have abandoned bev just to make coding easier.
*)
and instrexp3(b as (btexp:exp,bsites:event list,bvars:exp list),
	      exp:exp) : bool * exp =
(* bool: d = true iff instrumented expression discharges (any and all) 
             events (even if none to discharge 
   exp: exp' = instrumented expression. Consists of a tuple (lbt,value)
                iff d = true, else just a value.
*)
  let
    fun discharge (allsites:bool,
		   f:(exp * event list * exp list) -> exp) =
	(* Discharge any function events (any events at all if allsites true)
	   before executing expression constructed by f. *)
	  let val (btopt,b as (btexp,bsites,bvars)) =
	         if (allsites andalso (!instrumLevel = 3) 
		     andalso (not (null bsites))) 
		   orelse (hasfnsite bsites) then
		   let val btvar = makenvar "btvar"
		       val evn = makeevent(bsites,0)
		       val btdec = 
			   VALdec[VB{pat=VARpat btvar,
				     exp=EVENTexp(evn,btexp,bvars),
				     tyvars=nil}]
		   in (SOME btdec,(VARexp(ref btvar),nil,nil))
		   end
		 else (NONE,b)
	      val exp' = f b
          in case btopt of
	       SOME btdec =>
		 (true,LETexp(btdec,TUPLEexp[btexp,exp']))   (* !! *)
	     | NONE => (false,exp')
          end
    fun instr (RECORDexp l) = 
          let fun f ((lab as LABEL{name,...},exp)::rest,accv,accd) = 
 	          let val fieldtmp =
		              makevar ("field:" ^ (Symbol.name name), UNDEFty)
		  in f (rest,
		        (lab,VARexp(ref fieldtmp)) :: accv,
			(fieldtmp,exp) :: accd)
		  end
	        | f (nil,accv,accd) = (rev accv, rev accd)
              val (lv,ld) = f (l,nil,nil)
		  (* lv is list of (label,varexp) pairs
		     ld is list of (var,exp) pairs *)
	      fun g (b as (btexp,bsites,bvars),(fieldtmp,exp)::rest) = 
		    let val (d,exp') = instrexp3(b,exp)
		    in if d then
			 let val btvar = makenvar "btvar"
		             val dec' = VALdec[VB{pat=TUPLEpat[VARpat btvar,
							       VARpat fieldtmp],
						  exp=exp',
						  tyvars=nil}]
			     val (_,rest') = g((VARexp(ref btvar),nil,nil),rest)
			 in (SOME(VARexp(ref btvar)),dec'::rest')
                         end
		       else
			 let val dec' = VALdec[VB{pat=VARpat fieldtmp,
						  exp=exp',
						  tyvars=nil}]
			     val (btopt,rest') = g(b,rest)
			 in (btopt,dec'::rest')
			 end
		    end
                | g (_,nil) = (NONE,nil)
	      val (btopt,ld') = g(b,ld)
	      (* avoid decl's when possible, for cosmetic reasons *)
	      fun h ((lab,_)::rl,(VALdec[VB{exp,...}])::re) = 
		         (lab,exp)::(h(rl,re))
		| h (nil,nil) = nil
		| h _ = impossible "DebugInstrum.instrexp RECexp"
	  in case btopt of
	       SOME (btexp') => (true,
				 LETexp(SEQdec ld',
					TUPLEexp[btexp',
						 RECORDexp lv]))  (* !! *)
	     | NONE => (false, RECORDexp(h(lv,ld')))
	  end 
      | instr (SEQexp expl) =
	  let 
	    fun g(usebt,b as (btexp,bsites,bvars),exp::rest) =
		  let val (d,exp') = instrexp3(b,exp)
		  in if d then
		       let val btvar = makenvar "btvar"
			   val valvar = makenvar "valvar"
			   val (_,rest') = 
			       g(false,(VARexp(ref btvar),nil,nil),rest)
			   val useexp = 
			      if usebt then
				TUPLEexp[VARexp(ref btvar),
					 SEQexp(VARexp(ref valvar)::
						rest')]
			      else SEQexp(VARexp(ref valvar)::rest')
		       in (true,
			   [LETexp(VALdec[VB{pat=TUPLEpat[VARpat btvar,
							  VARpat valvar],
					    exp=exp',
					    tyvars=nil}],
				   useexp)])
		       end
		     else 
		       let val (d',rest') = g(usebt,b,rest)
		       in (d',exp'::rest')
		       end
		  end
	      | g(_,_,nil) = (false,nil)
	    val (d,expl') = g(true,b,expl)
          in (d,SEQexp expl')                          (* !! *)
          end
      | instr (CONexp(DATACON{rep=REF,...})) = (false,HREFexp)
      | instr (VARexp(ref(assopr as VALvar{access=INLINE(P.:=),...}))) =
	        (false,HASSexp assopr)
      | instr (exp as APPexp(opr,arg)) =
	  let fun strip (MARKexp(exp,_,_)) = strip exp
		| strip (CONSTRAINTexp(exp,_)) = strip exp
		| strip (SEQexp[exp]) = strip exp
		| strip exp = exp
	      fun normal () =
		 let val oprtmp = makenvar "oprvar"
		     val argtmp = makenvar "argvar"
		     val btvar = makenvar "btvar"
		     val (dopr,opr') = instr opr
		     val oprpat =
			if dopr then
			  TUPLEpat[VARpat btvar,VARpat oprtmp]
			else VARpat oprtmp
		     val (darg,arg') =
		       if dopr then
			 instrexp3((VARexp(ref btvar),nil,nil),arg)
		       else instr arg
		     val argpat = 
		       if darg then
                         if dopr then
			   TUPLEpat[WILDpat, VARpat argtmp]
			 else TUPLEpat[VARpat btvar,VARpat argtmp]
		       else VARpat argtmp
		     val appexp = APPexp(VARexp(ref oprtmp),
					 VARexp(ref argtmp))
		     val fullexp =
		        if dopr orelse darg then 
			  let val evn = makeevent([APPev exp],0)
			  in TUPLEexp[VARexp(ref btvar),
				      SEQexp[EVENTexp(evn,VARexp(ref btvar),nil),
					     appexp]]
			  end
			else 
			  let val evn = makeevent (APPev exp::bsites,0)
			  in TUPLEexp[EVENTexp(evn,btexp,bvars),
				      appexp]
			  end
		 in (true,
		     LETexp(SEQdec[VALdec[VB{pat = oprpat,
					     exp = opr', 
					     tyvars = nil}],
				   VALdec[VB{pat = argpat,
					     exp = arg', 
					     tyvars = nil}]],
			    fullexp))
		 end
 	  in case (strip opr) of
	       VARexp(ref(VALvar{access=INLINE(P.:=),...})) => normal()
	     | (opr as VARexp(ref(VALvar{access=INLINE prim,...}))) =>
		 let val (darg,arg') = instr arg
		     val argtmp = makenvar "argvar"
		 in if darg then
		      let val btvar = makenvar "btvar"
		      in (true,LETexp(VALdec[VB{pat=TUPLEpat[VARpat btvar,
							     VARpat argtmp],
						exp=arg',
						tyvars=nil}],
				      TUPLEexp[VARexp(ref btvar),
					       APPexp(opr,
						      VARexp(ref argtmp))]))
		      end
		    else if (hasfnsite bsites) 
			   andalso (Prim.mayRaise prim) then
		      let val evn = makeevent(APPev exp::bsites,0)
		      in (true,LETexp(VALdec[VB{pat=VARpat argtmp,
						exp=arg',
						tyvars=nil}],
				      TUPLEexp[EVENTexp(evn,btexp,bvars),
					       APPexp(opr,
						      VARexp(ref argtmp))]))
		      end
  		    else (false,APPexp(opr,arg'))
		 end
	     | CONexp(DATACON{rep=REF,...}) => normal()
	     | CONexp _ => 
		 let val (darg,arg') = instr arg
		 in if darg then
		      let val btvar = makenvar "btvar"
			  val argtmp = makenvar "argtmp"
		      in (true,LETexp(VALdec[VB{pat=TUPLEpat[VARpat btvar,
							     VARpat argtmp],
						exp=arg',
						tyvars=nil}],
				      TUPLEexp[VARexp(ref btvar),
					       APPexp(opr,
						      VARexp(ref argtmp))]))
		      end
		    else (false,APPexp(opr,arg'))
		 end
             | FNexp body =>  (* really a CASE or equivalent *)
		 let val (darg,arg') = instr arg
		 in if darg then
		      let val valvar = makenvar "valtmp"
			  val btvar = makenvar "btvar"
			  val body' = instrrules((VARexp(ref btvar),nil,nil),
						fn r => CASEev(arg,r)) body
		      in (true,
			  LETexp(VALdec[VB{pat=TUPLEpat[VARpat btvar,
							VARpat valvar],
					   exp=arg',
					   tyvars=nil}],
				 TUPLEexp[VARexp(ref btvar),
					  APPexp(FNexp body',
						 VARexp(ref valvar))]))
		      end
		    else 
		      discharge (true,fn b =>
		        let val body' = 
				 instrrules(b,fn r => CASEev(arg,r)) body
			in APPexp(FNexp body',arg')
			end)
		 end
	     | _ => normal()
	  end
      | instr (CONSTRAINTexp (e,c)) =
	  let val (d,e') = instr e
          in (d,CONSTRAINTexp(e',c))
	  end
      | instr (exp as MARKexp(RAISEexp arg,_,_)) = 
		(* N.B. Must be marked *)
	  let val argtmp = makenvar "argvar"
	      val (darg,arg') = instr arg
	  in if darg then
	       let val evn = makeevent([RAISEev exp], 0)
		   val btvar = makenvar "btvar"
	       in (true,
		   LETexp(VALdec[VB{pat=TUPLEpat[VARpat btvar,
						 VARpat argtmp],
				    exp=arg',
				    tyvars=nil}],
			  TUPLEexp[VARexp(ref btvar),
				   SEQexp[EVENTexp(evn,VARexp(ref btvar),nil),
					  RAISEexp(VARexp(ref argtmp))]]))
               end
             else 
	       let val evn = makeevent(RAISEev exp::bsites,0)
               in (true,
		   LETexp(VALdec[VB{pat=VARpat argtmp,
			 	    exp=arg',
				    tyvars=nil}],
			  TUPLEexp[EVENTexp(evn,btexp,bvars),
				   RAISEexp (VARexp(ref argtmp))]))
	       end
	   end
      | instr (exp as MARKexp(LETexp (ldec,lexp),_,_)) = 
		(* note: must be marked *)
	  (* We discharge any events before executing LET. *)
	  discharge (true,fn (b as (btexp,bsites,bvars)) =>
	      (* assume instrdec returns a btexp' valid in context of ldec' *)
	    let val ((btexp',_,bsites',bvars'),ldec') =
		           instrdec ((btexp,0,bsites,bvars),ldec)
		val (d',lexp') = instrexp3((btexp',bsites',bvars'),lexp)
	    in if d' then
		   let val valvar = makenvar "valtmp"
		   in LETexp(ldec',
			     LETexp(VALdec[VB{pat=TUPLEpat[WILDpat,
							   VARpat valvar],
					      exp=lexp',
					      tyvars=nil}],
				    VARexp (ref valvar)))
		   end
	       else (* If anything at all was bound by ldec, then if 
		       any event is executed in body it would have discharged.
		       So at this point, we need a backstop event. *)
		   let val evn = makeevent(LETev exp::bsites',0)
		   in LETexp(SEQdec[ldec',
				    VALdec[VB{pat=WILDpat,
					      exp=EVENTexp(evn,btexp',bvars'),
					      tyvars=nil}]],
			     lexp')
		   end
	    end)
      | instr (CASEexp(exp,rl)) = 
	  let val (d,exp') = instr exp
	  in if d then
	       let val valvar = makenvar "valtmp"
		   val btvar = makenvar "btvar"
		   val rl' = instrrules((VARexp(ref btvar),nil,nil),
				       fn r => CASEev(exp,r)) rl
	       in (true,
		   LETexp(VALdec[VB{pat=TUPLEpat[VARpat btvar,VARpat valvar],
				    exp=exp',
				    tyvars=nil}],
			  TUPLEexp[VARexp(ref btvar),
				   CASEexp(VARexp(ref valvar),
					   rl')]))
	       end
	     else 
               discharge (true,fn b =>		 
		 let val rl' = instrrules(b,fn r => CASEev(exp,r)) rl
		 in CASEexp(exp',rl')
		 end)
	  end
	
      | instr (HANDLEexp (e, HANDLER(FNexp body))) =
	  (* We discharge any events before executing HANDLEexp *)
	  discharge (true,fn b => 
	    let val (d',e') = instrexp3(b,e)
		val e' = 
		    if d' then
		      let val valvar = makenvar "valtmp"
		      in LETexp(VALdec[VB{pat=TUPLEpat[WILDpat,VARpat valvar],
					  exp=e',
					  tyvars=nil}],
				VARexp (ref valvar))
		      end
		    else e'
		val body' = instrrules(b,HANDLEev) body
	    in HANDLEexp(e',HANDLER(FNexp body'))
	    end)
      | instr (FNexp body) =
	  (* We discharge any function events before executing FNexp *)
	  discharge (false,fn b =>
	    let val body' = instrrules(b,FNev) body
	    in FNexp body'
	    end)
      | instr (MARKexp (exp,s,e)) =
          let val (d,exp') = instr exp
          in (d, MARKexp(exp',s,e))
          end
      | instr exp = (false,exp)
    and instrrules (b as (btexp,bsites,bvars),evf) =
	(* always return just a value *)
      let 
        fun f (rule as RULE(pat,exp as MARKexp(_))) = 
	     let val vars = (patvars (fn v => VARexp(ref v)) pat)
		 val bsites' = (evf rule)::bsites
		 val bvars' = vars@bvars
		 val (d,exp') = instrexp3((btexp,bsites',bvars'),exp)
	     in if d then
		  let val valvar = makenvar "valtmp"
		  in RULE(pat,
			  LETexp(VALdec[VB{pat=TUPLEpat[WILDpat,
							VARpat valvar],
					   exp=exp',
					   tyvars=nil}],
				 VARexp(ref valvar)))
		  end
		else (* need backstop *)
		  let val evn = makeevent(bsites',0)
		  in RULE(pat,
			  SEQexp[EVENTexp(evn,btexp,bvars'),
				 exp'])
		  end
	      end
          | f (RULE(pat,exp)) =
	     let val (d,exp') = instrexp3(b,exp)
	     in if d then
 	          let val valvar = makenvar "valtmp"
		  in RULE(pat,
			  LETexp(VALdec[VB{pat=TUPLEpat[WILDpat,
							VARpat valvar],
					   exp=exp',
					   tyvars=nil}],
				 VARexp(ref valvar)))
		  end
		else RULE(pat,exp')
             end
      in map f 
      end
  in
    instr exp
  end



and instrdec (b as (btexp:exp, bev:int, bsites:event list,bvars: exp list),
	     dec:dec) : 
	((exp * int * (event list) * (exp list)) * dec) =
  let
    fun instr (dec as MARKdec(VALdec vbl,_,_)) =
 	  if abs(!instrumLevel) = 1 then
	    let val vars = vblextract (fn v => VARexp(ref v)) vbl
		fun newbfd (stat,b,f,d) =
		    case (stat,f,d) of
		       (FIRST_EV evn,true,NONE) => 
			   let val (nxtdec,nxtexp) = makent "NEXTTIME"
			   in ((nxtexp,evn,nil,nil),false,SOME nxtdec)
			   end
		      | (ABS_EV,_,_) => (b,f,d)
		      | (NO_EV,_,_) => (b,f,d)
		      | _ => (b,false,d)
	        fun g (VB{pat,exp,tyvars}::rest,b,f,d,acc) =
		        let val (stat,exp') = instrexp(b,exp)
			    val (b',f',d') = newbfd(stat,b,f,d)
			in g (rest,b',f',d',VB{pat=pat,exp=exp',tyvars=tyvars}::acc)
			end
		  | g (nil,(btexp',bev',bsites',bvars'),_,d,acc) =
			((btexp',bev',VALev dec :: bsites',vars @ bvars'),
			 case d of
			   SOME nxtdec => SEQdec[nxtdec,VALdec(rev acc)]
			 | NONE => VALdec(rev acc))
	    in g (vbl,b,true,NONE,nil)
	    end
          else if abs(!instrumLevel) = 2 then
	    let val vars = vblextract (fn v => VARexp(ref v)) vbl
		fun f (VB{pat,exp,tyvars}) =
		    VB{pat=pat,exp=instrexp2(b,exp),tyvars=tyvars}
	    in ((btexp,bev,VALev dec::bsites,vars@bvars),VALdec(map f vbl))
	    end
          else (* abs(!instrumLevel) = 3 or 4 *)
	    let val vars = vblextract (fn v => VARexp(ref v)) vbl
		fun g(VB{pat,exp,tyvars}::rest,btexp,bsites,bvars) =
		     let val (d,exp') = instrexp3((btexp,bsites,bvars),exp)
		     in if d then
			  let val btvar = makenvar "btvar"
			      val (_,rest') = g(rest,VARexp(ref btvar),nil,nil)
			      val dec' =
			         VALdec[VB{pat=TUPLEpat[VARpat btvar,
							pat],
					   exp=exp',
					   tyvars=nil}]
			  in (SOME(VARexp(ref btvar)),dec'::rest')
			  end
			else
			  let val dec' = VALdec[VB{pat=pat,
						   exp=exp',
						   tyvars=nil}]
			      val (btopt,rest') = g(rest,btexp,bsites,bvars)
			  in (btopt,dec'::rest')
			  end
                     end
		  | g (nil,_,_,_) = (NONE,nil)
	       val (btopt,decl') = g(vbl,btexp,bsites,bvars)
	       val (btexp',bsites',bvars') =
		  case btopt of
		    SOME btexp' => (btexp',nil,nil)
		  | NONE => (btexp,bsites,bvars)
	    in ((btexp',0,VALev dec::bsites',vars@bvars'),
		SEQdec decl')
	    end
      | instr (dec as MARKdec(VALRECdec rvbl,_,_)) =
            (* N.B. Bodies cannot discharge vars ! *)
	  if abs(!instrumLevel) = 1 then
            let val ((btexp',bev',bsites',bvars'),evndecopt) = 
		      if hasfnsite bsites then
		        let val (b',evndec) = simplebind(b,nil,nil)
		        in (b',SOME evndec)
		        end
    		      else (b,NONE)
	        val vars = map (fn RVB{var,...} => VARexp(ref var)) rvbl
		val b' = (btexp',bev',VALRECev dec::bsites',vars @ bvars')
		val rvbl' = map (fn (RVB{var,exp,resultty,tyvars}) =>
				     let val (_,exp') = instrexp(b',exp)
				     in RVB {var=var,
					     exp=exp',
					     resultty=resultty,
					     tyvars=tyvars}
				     end) rvbl
	    in case evndecopt of
		 SOME evndec => (b',SEQdec[evndec,VALRECdec rvbl'])
	       | NONE => (b',VALRECdec rvbl')
	    end
	  else if abs(!instrumLevel) = 2 then
	    let val vars = map (fn RVB{var,...} => VARexp(ref var)) rvbl
		val b' = (btexp,bev,VALRECev dec::bsites,vars @ bvars)
		val rvbl' = map (fn (RVB{var,exp,resultty,tyvars}) =>
				     let val exp' = instrexp2(b',exp)
				     in RVB {var=var,
					     exp=exp',
					     resultty=resultty,
					     tyvars=tyvars}
				     end) rvbl
	    in (b',VALRECdec rvbl')
	    end
	  else (* abs (!instrumLevel) = 3 or 4 *)
	    let val vars = map (fn RVB{var,...} => VARexp(ref var)) rvbl
		val bsites' = VALRECev dec::bsites
		val bvars' = vars@bvars
		fun g (RVB{var,exp,resultty,tyvars}) =
		     let val (d,exp') = instrexp3((btexp,bsites',bvars'),exp)
		     in if d then
			 debugPanic "instrexp3 VALRECdec"
			else RVB{var=var,
				 exp=exp',
				 resultty=resultty,
				 tyvars=tyvars}
		     end
                val rvbl' = map g rvbl
	    in ((btexp,bev,bsites',bvars'),VALRECdec rvbl')
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
		 let val (b',indec) = simplebind(b',[LOCINev dec],nil)
		     val (b'' as (btexp'',bev'',bsites'',bvars''),visibledec')=
				 instrdec (b',visibledec)
		 in ((btexp'',bev'',LOCENDev dec::bsites'',bvars''),
   	  	     LOCALdec(localdec',SEQdec[indec,visibledec']))
		 end
	       else 
		 ((btexp'',bev'',LOCENDev dec::bsites'',bvars''),
	          LOCALdec(localdec',visibledec'))
	    end
      | instr (dec as MARKdec(ABSTYPEdec {abstycs,withtycs,body},_,_)) =
	    let val ((btexp',bev',bsites',bvars'), body') = instr body
	    in ((btexp',bev',TYPEev dec::bsites',bvars'),
		ABSTYPEdec {abstycs=abstycs,withtycs=withtycs,body=body'})
	    end
      | instr (dec as MARKdec(STRdec strbl,_,_)) =
            let val (timearrdec,timearrexp) = maketimearr (length strbl)
	        val (bevl,strbl',strvl) = instrstrbl timearrexp strbl
		val dec' = SEQdec [timearrdec, STRdec strbl']
	    in ((btexp,
		 bev,
                 STRev (dec,bevl)::bsites,
		 timearrexp::(strvl @ bvars)), 
                dec')
	    end
      | instr (dec as MARKdec(ABSdec strbl,_,_)) =
            let val (timearrdec,timearrexp) = maketimearr (length strbl)
	        val (bevl,strbl',strvl) = instrstrbl timearrexp strbl
		val dec' = SEQdec [timearrdec, ABSdec strbl']
	    in ((btexp,
		 bev,
                 ABSev (dec,bevl)::bsites,
		 timearrexp::(strvl @ bvars)), 
                dec')
	    end
      | instr (dec as MARKdec(FCTdec fctbl,_,_)) =
	    let 
              fun instrfctb(fctb as FCTB{fctvar,param,def,thin,constraint}) =
                let val (b',entdec) = simplebind(b,[FCTENTev fctb],nil)
                    val (bev,def') = instrstrexp(b',def)
                in (bev, FCTB{fctvar=fctvar,param=param,
		                 def=LETstr(entdec,def'),
		                 thin=thin,constraint=constraint})
                end
	      fun tovar (FCTB{fctvar=FCTvar{name,access=PATH[lv],...},...}) =
		    fakeuse(name,lv)
	      val fctvl = map tovar fctbl
	      val (bevl,fctbl') = unpairlist (map instrfctb fctbl)
	    in ((btexp,bev,FCTev (dec,bevl)::bsites,fctvl @ bvars),
		FCTdec fctbl')
            end
      | instr (dec as MARKdec(SIGdec _,_,_)) =
	    ((btexp,bev,SIGev dec::bsites,bvars),dec)
      | instr (dec as MARKdec(TYPEdec _,_,_)) =
            ((btexp,bev,TYPEev dec::bsites,bvars),dec)
      | instr (dec as MARKdec(FIXdec _,_,_)) =
	    ((btexp,bev,FIXev dec::bsites,bvars),dec)
      | instr (dec as MARKdec (OVLDdec _,_,_)) =
	    ((btexp,bev,OVLDev dec::bsites,bvars),dec)
      | instr (dec as MARKdec(DATATYPEdec _,_,_)) =
            ((btexp,bev,TYPEev dec::bsites,bvars),dec)
      | instr (dec as MARKdec(EXCEPTIONdec ebl,_,_)) =
	    let fun extract (EBgen{exn,...}) = exn
		  | extract (EBdef{exn,...}) = exn
	        fun tovar (DATACON{name,rep=VARIABLE(PATH [lv]),...}) =
		      fakeuse(name,lv)
		  | tovar (DATACON{name,rep=VARIABLEc(PATH [lv]),...}) =
		      fakeuse(name,lv)
		  | tovar _ = debugPanic "instrDec EXCEPTIONdec"
	        val ebreps = map (tovar o extract) ebl
	    in 
               ((btexp,bev, EXCEPTIONev dec::bsites,ebreps @ bvars),dec)
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
		    val timestrvar = anonstrvar()
		    val timestr = STRUCTstr
		           {body=[VALdec[VB{pat=WILDpat,
				            exp=APPexp(VARexp(ref updateop),
					  	       TUPLEexp[timearrexp,
						   	        INTexp n,
						   		GETTIMEexp]),
					    tyvars=nil}]],
			    locations=nil,
			    str=ERROR_STR}
	            val timestrb = STRB{strvar=timestrvar,
				        def=timestr, thin=NONE,constraint=NONE}
		in (n-1,
		    bev' :: bevl,
	            strb' :: (timestrb :: strbl))
		end
	   val (_,bevl,strbl') = fold dostrb strbl (length strbl - 1,[],[])
  	   fun tovar (STRB{strvar=STRvar{name=n,access=PATH [lv],...},...}) =
		 fakeuse(n,lv)
	   val strvl = map tovar strbl
	 in (bevl,strbl',strvl)
         end
    in instr dec
   end

and instrstrexp (b,mstrexp as MARKstr(strexp,_,_)) =
     (case strexp of 
        STRUCTstr{body,locations,str} =>
          let val (b',body') = instrlist(b,body)
              val ((_,bev',_,_),enddec) = 
       	       simplebind(b',[STRENDev mstrexp],nil)
          in (bev',STRUCTstr{body=body'@[enddec],locations=locations,
       			str=str})
          end
      | APPstr{oper,argexp,argthin,str} => 
          let val (_,argexp') = instrstrexp (b,argexp)
	      val strvar = anonstrvar ()
	      val strb = STRB{strvar=strvar,def=argexp',
			      thin=argthin (*??*),constraint=NONE}
	      val STRvar{access=PATH[lv],...} = strvar
	      val paramv = fakeuse (Symbol.varSymbol "param",lv)
              val ((_,bev',_,_),appdec) = 
       		simplebind(b,[FCTAPPev mstrexp],[paramv])
          in (bev',
              LETstr(SEQdec[STRdec[strb],appdec], 
		     APPstr{oper=oper,argexp=VARstr strvar,
			    argthin=NONE (*??*),str=str}))
          end
      | LETstr(dec,strexp) =>
          let val (b',dec') = instrdec(b,dec)
              val (bev',strexp') = instrstrexp(b',strexp)
          in (bev',LETstr(dec',strexp'))
          end
      | VARstr _ =>
          let val ((_,bev',_,_),defdec) = 
       		simplebind(b,[STRVARev mstrexp],nil)
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

val absyn' =
  let val startevn = makeevent([STARTev absyn],lastBindEvn)
      val bevvar = makevar ("BIND" ^ makestring startevn,intTy)
      val STARTexp = EVENTexp(startevn,INTexp lastBindTime,nil)
      val STARTdec = VALdec[VB{pat=VARpat(bevvar),exp=STARTexp,tyvars=nil}]
      val ((btexp,bev,bsites,bvars),absyn') = 
 	     instrdec((VARexp (ref bevvar),startevn,nil,nil),absyn)
      val endevn = makeevent(ENDev absyn::bsites,bev)
      val ENDexp = FEVENTexp(endevn,btexp,bvars)
      val ENDdec = VALdec[VB{pat=WILDpat,exp=ENDexp,tyvars=nil}]
  in SEQdec [STARTdec,
	     absyn',
	     ENDdec]
  end


in 
  {absyn=LOCALdec(VALdec [VB {pat = TUPLEpat [VARpat timesvar,
					      VARpat eventtimes,
					      VARpat breakentry,
					      VARpat hcreater,
					      VARpat weakvar,
					      VARpat udl,
					      VARpat arrayvar],
                              exp = APPexp(VARexp(CoreInfo.getDebugVar),
					   INTexp (firstEvn)),
  	                      tyvars = nil}],
                  absyn'),
   evCount = !nextEvn - firstEvn,
   events = arrayoflist(rev(!evList)),
   elb = arrayoflist(rev(!elbList))}
end  (* fun instrumDec *)

end  (* structure debugInstrum *)
