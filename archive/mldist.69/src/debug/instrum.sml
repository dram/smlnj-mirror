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
end

structure DebugInstrum : DEBUGINSTRUM =
struct

open DebugUtil DebugStatic Access Absyn Basics BasicTypes ErrorMsg
   
fun makevar(str,t) = 
     let val name = Symbol.varSymbol str
	 val lvar = namedLvar name
     in VALvar{name=[name],access=PATH[lvar],typ=ref t}
     end

fun fakeuse(name,lvar) = 
	VARexp(ref (VALvar{name=[name],access=PATH[lvar],typ=ref UNDEFty}))
	
fun anonstrvar ()  = 
     let val name = Symbol.strSymbol "AnonStruct"
	 val lvar = namedLvar name
     in STRvar{name=[name],access=PATH[lvar],binding=NULLstr}
     end

val alpha = VARty(mkTyvar(IBOUND 0))

val intreftype = CONty(refTycon,[intTy])
val intarraytype = CONty(arrayTycon, [intTy])	(* ?? *)
val objectarraytype = CONty(arrayTycon, [alpha]) (* ??????? *)

val ubassop = VALvar{name= [Symbol.varSymbol "unboxedassign"],
		    access=INLINE(P.unboxedassign),
		    typ=ref (tupleTy([intreftype,intTy]) --> unitTy)}
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
	 	      typ = ref(tupleTy([CONty(arrayTycon,[alpha]),intTy,alpha]) --> unitTy)} (* ?? *)
val ineqop = VALvar{name=[Symbol.varSymbol "ineq"],
		    access=INLINE(P.ineq),
		    typ = ref(tupleTy([intTy,intTy]) --> boolTy)}
val delayop = VALvar{name= [Symbol.varSymbol "delay"],
		    access=INLINE(P.delay),
		    typ= ref UNDEFty}


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

val GETTIMEexp = APPexp(VARexp(ref subop), 
			TUPLEexp[VARexp(ref timesvar),
		                 INTexp 0])

fun INCexp exp = APPexp(VARexp(ref addop),
		        TUPLEexp[exp,
				 INTexp 1])

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


fun BREAKexp(ntimeexp, evn, args, contexp) = 
      CASEexp(APPexp(VARexp(ref ineqop), 
		     TUPLEexp[ntimeexp, 
		              APPexp(VARexp(ref subop), 
				     TUPLEexp[VARexp(ref timesvar),
					      INTexp 1])]),
	      [RULE(truePat, contexp),
	       RULE(falsePat, SEQexp[APPexp(VARexp (ref breakentry),
				            TUPLEexp(INTexp evn::args)),
				     contexp]),
	       RULE(WILDpat, INTexp 0)])

fun EVENTexp (evn,lbt,args) = 
     let val (ntimedec,ntimeexp) = makent "Newtime"
     in LETexp (ntimedec,
		SEQexp [SETTIMEexp ntimeexp,
			BREAKexp(ntimeexp,evn,lbt::args,
				 NOTETIMEexp(ntimeexp,evn)),
			ntimeexp])
     end

fun FEVENTexp (evn,lbt,args) =
     let val (ntimedec,ntimeexp) = makent "Newtime"
     in LETexp (ntimedec,
	        SEQexp [SETTIMEexp ntimeexp,
		        APPexp(VARexp (ref breakentry),
			       TUPLEexp(INTexp evn::lbt::args)),
	                NOTETIMEexp(ntimeexp,evn),
			ntimeexp])
     end


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

fun simplebind ((btexp,bev,bsites,bvars),ev,vars) =
       let val evn = makeevent(ev@bsites,bev)
	   val bevvar = makevar("BIND" ^ makestring evn,intTy)
	   val evndec = VALdec[VB{pat=VARpat(bevvar),
		                    exp=EVENTexp(evn,btexp,vars@bvars),
				    tyvars=nil}]
       in ((VARexp(ref bevvar),evn,nil,nil),evndec)
       end

(*fun hasfnsite (FNev _::_) = true
  | hasfnsite (HANDLEev _::_) = true
  | hasfnsite (_::rest) = hasfnsite rest
  | hasfnsite (nil) = false
*)

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

type binddata = exp *         (* representing time of last binding event *)
                evn *         (* event number of last binding event *)
		event list *  (* events to discharge *)
		exp list      (* variables to discharge *)



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
		 in (*if (hasfnsite bsites') andalso (Prim.mayRaise prim) then
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
		    else *)
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
	 (* if hasfnsite bsites then
	    let val evn = makeevent(bsites,bev)
		val (nxtdec,nxtexp) = makent "NEXTTIME"
		val b' = (nxtexp,evn,nil,nil)
                val (_,e') = instrexp (b',e)
     	        val body' = instrrules(b',HANDLEev) body
     	    in (FIRST_EV evn,
		LETexp(nxtdec,
		       SEQexp[EVENTexp(evn,btexp,bvars),
			      HANDLEexp(e',HANDLER(FNexp body'))]))
     	    end
          else *)
     	    let val (_,e') = instr e
     	        val body' = instrrules(b,HANDLEev,true) body
     	    in (MAYBE_EV, HANDLEexp(e',HANDLER(FNexp body')))
            end
      | instr (FNexp body) =
         (* if hasfnsite bsites then 
	    let val evn = makeevent(bsites,bev)
		val (nxtdec,nxtexp) = makent "NEXTTIME"
		val b' = (nxtexp,evn,nil,nil)
	        val body' = instrrules(b',FNev) body
	    in (FIRST_EV evn,LETexp(nxtdec,
				    SEQexp[EVENTexp(evn,btexp,bvars),
			                   FNexp body']))
	    end
	  else *)
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
	(* Kludge: changed to always insert a FN event *)
        fun f (rule as RULE(pat,exp as MARKexp(_))) = 
	     let val vars = (patvars (fn v => VARexp(ref v)) pat)
		 val bsites' = (evf rule)::bsites
		 val bvars' = vars@bvars
	     in if force then
                  let val evn = makeevent(bsites',bev)
		      val bevvar = makevar ("BIND" ^ makestring evn, intTy)
		      val (_,exp') = 
			       instrexp((VARexp(ref bevvar),evn,nil,nil),exp)
		  in RULE(pat, LETexp(VALdec[VB{pat=VARpat bevvar,
						exp=EVENTexp(evn,btexp,bvars'),
						tyvars=nil}],
				      exp'))
		  end
                else 
  		  let val (stat,exp') = 
		               instrexp ((btexp,bev,bsites',bvars'),exp)
		  in if (need_backstop stat) then
		       let val evn = makeevent(bsites', bev)
		       in RULE (pat,LETexp(VALdec[VB{pat=WILDpat,
						     exp=EVENTexp(evn,btexp,bvars'),
						     tyvars=nil}],
					   exp'))
		       end
		     else RULE(pat,exp')
		  end
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


and instrdec (b as (btexp:exp, bev:int, bsites:event list,bvars: exp list),
	     dec:dec) : 
	((exp * int * (event list) * (exp list)) * dec) =
  let
    fun instr (dec as MARKdec(VALdec vbl,_,_)) =
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
      | instr (dec as MARKdec(VALRECdec rvbl,_,_)) =
            (* N.B. Bodies cannot discharge vars ! *)
            let val ((btexp',bev',bsites',bvars'),evndecopt) = 
		     (* if hasfnsite bsites then
		        let val (b',evndec) = simplebind(b,nil,nil)
		        in (b',SOME evndec)
		        end
    		      else  *)
			(b,NONE)
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
			    str=NULLstr}
	            val timestrb = STRB{strvar=timestrvar,
				        def=timestr, thin=NONE,constraint=NONE}
		in (n-1,
		    bev' :: bevl,
	            strb' :: (timestrb :: strbl))
		end
	   val (_,bevl,strbl') = fold dostrb strbl (length strbl - 1,[],[])
  	   fun tovar (STRB{strvar=STRvar{name=[n],access=PATH [lv],...},...}) =
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
