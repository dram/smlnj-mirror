(* NO TIMES VERSION *)

(* DebugInstrum

   Instrument user cde.
   Instrumented code needs to gain access to a variety of structures at 
     run-time, some of them in a dirty fashion.  This is done via
     a special system ref set up in DebugInterface.

*)

signature DEBUG_NINSTRUM = sig
   val instrumDec: {absyn:Absyn.dec,
		    firstEvn:DebugNStatic.evn,
		    lastBindEvn:DebugNStatic.evn} ->
         {absyn:Absyn.dec,
	  evCount:int,
          events:DebugNStatic.event list array,
	  elb:DebugNStatic.evn array}
   val instrumLevel : int ref
end

structure DebugNInstrum : DEBUG_NINSTRUM =
struct

open Array List DebugUtil DebugNStatic Access Absyn Basics BasicTypes ErrorMsg
infix 9 sub
   
val instrumLevel = ref 3  (* normal *) 

(* Possible values:
magnitude:
   1 - invalid
   2 - newfangled idea of worst case: 
         - no sequentialization analysis; 
	 - events at top of every function
   3 - another newfangled idea
   4 - and its variant.
addends: +10 = update current br at every event, for better reporting.
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
     in STRvar{name=[name],access=PATH[lvar],binding=NULLstr}
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
(* N.B. now BOXED !! *)
val updateop = VALvar{name=[Symbol.varSymbol "update"],
		      access=INLINE(P.update),
	 	      typ = ref(tupleTy([CONty(arrayTycon,[alpha]),intTy,alpha]) --> unitTy)} (* *?? *)
(* val ineqop = VALvar{name=[Symbol.varSymbol "ineq"],
		    access=INLINE(P.ineq),
		    typ = ref(tupleTy([intTy,intTy]) --> boolTy)} *)
val ieqlop = VALvar{name=[Symbol.varSymbol "ieql"],
		    access=INLINE(P.ieql),
		    typ = ref(tupleTy([intTy,intTy]) --> boolTy)}
(*val delayop = VALvar{name= [Symbol.varSymbol "delay"],
		    access=INLINE(P.delay),
 		    typ= ref UNDEFty} *)
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

fun instrumDec {absyn:dec,firstEvn:evn,lastBindEvn:evn} =

let 
val nextEvn = ref firstEvn (* next event number to use *)
val evList = ref ([]: event list list)
val elbList = ref ([]: int list)

val storeAlways = (abs(!instrumLevel) div 10) = 1
val useInstrumLevel = abs(!instrumLevel) mod 10

fun makeevent (ev,elb) =
  (evList := ev::(!evList);
   elbList := elb::(!elbList);
   !nextEvn before
   inc nextEvn)

fun saveNextEvent () = (!evList,!elbList,!nextEvn)
fun restoreNextEvent (evList',elbList',nextEvn') = 
    (evList := evList'; elbList := elbList'; nextEvn := nextEvn')

val eventBreaks = makevar("eventBreaks", UNDEFty)
val brvar = makevar ("bindrecord", objectarraytype)
(* Instrumented code must arrange to set this global:
   - just prior to an APP (or similar)
   - just prior to to FCTAPP
   - at the end of every structure body
   - at the beginning and end of the outer decl body 
   - whereever else it feels like it, to increase accuracy of exn reporting! *)
val breakentry = makevar ("breakentry", objectarraytype --> unitTy)
val arrayvar = makevar ("array", UNDEFty)

val bracount = ref 0	(* for cosmetics only *)
fun makebrarr len =
  let val brarr = makevar("BRARR"^makestring(!bracount),UNDEFty)
  in inc bracount;
     (VALdec[VB{pat=VARpat(brarr),
		exp=APPexp(VARexp(ref arrayvar),
			   TUPLEexp[INTexp len,
				    INTexp 0]),
		tyvars=nil}],
      VARexp (ref brarr))
  end

val GETBRexp = 
  if !instrumLevel > 0 then
    APPexp(VARexp(ref derefop),
	   VARexp(ref brvar))
  else APPexp(VARexp(ref defop),unitExp)

fun SETBRexp nbrexp = APPexp(VARexp(ref assop),
			     TUPLEexp[VARexp(ref brvar),
				      nbrexp])
fun makenbr (evn,args) =
     let val nbr = makevar("Brec" ^ (makestring evn),UNDEFty)
     in (VALdec[VB{pat=VARpat nbr,
		   exp = TUPLEexp(INTexp evn::args),
		   tyvars=nil}],
	 VARexp(ref nbr))
     end

fun BREAKexp(evn,nbrexp,otherwiseExp) =
      CASEexp(APPexp(VARexp(ref subop),
		     TUPLEexp[VARexp(ref eventBreaks),
			      INTexp(evn-firstEvn)]),
	      [RULE(truePat, SEQexp[SETBRexp nbrexp,
				    APPexp(VARexp (ref breakentry),
					   unitExp)]),
	       RULE(falsePat, otherwiseExp),
	       RULE(WILDpat, INTexp 0)])


fun EVENTexp (evn,lbr,args,store) = 
  if !instrumLevel > 0 then
     let val (nbrdec,nbrexp) = makenbr(evn,lbr::args)
     in LETexp (nbrdec,
		SEQexp[BREAKexp(evn,nbrexp,
				if store orelse storeAlways then
				  SETBRexp nbrexp
				else unitExp),
		       nbrexp])
     end
  else 
    SEQexp ((map (fn arg => APPexp(VARexp(ref useop),arg)) (lbr::args))
	    @ [APPexp(VARexp(ref defop),unitExp)])

fun FEVENTexp (evn,lbr,args) =
  if !instrumLevel > 0 then
     let val (nbrdec,nbrexp) = makenbr(evn,lbr::args)
     in LETexp (nbrdec,
		SEQexp [SETBRexp nbrexp,
			APPexp(VARexp(ref breakentry),
			       unitExp),
		        nbrexp])
     end
  else 
    SEQexp ((map (fn arg => APPexp(VARexp(ref useop),arg)) (lbr::args))
	    @ [APPexp(VARexp(ref defop),unitExp)])

fun simplebind ((btexp,bev,bsites,bvars),ev,vars,store) =
       let val evn = makeevent(ev@bsites,bev)
	   val bevvar = makevar("BIND" ^ makestring evn,intTy)
	   val evndec = VALdec[VB{pat=VARpat(bevvar),
		                    exp=EVENTexp(evn,btexp,vars@bvars,store),
				    tyvars=nil}]
       in ((VARexp(ref bevvar),evn,nil,nil),evndec)
       end

fun hasfnsite (FNev _::_) = true
  | hasfnsite (HANDLEev _::_) = true
  | hasfnsite (_::rest) = hasfnsite rest
  | hasfnsite (nil) = false


and instrexp2(b as (brexp:exp,bev:evn,bsites:event list,bvars:exp list),
	      exp:exp) : 
     exp (* instrumented expression *) =
  let 
    fun instr (RECORDexp l) =
	  let fun f(lab,exp) = (lab,instr exp)
	  in RECORDexp (map f l)
	  end
      | instr (SEQexp expl) = SEQexp(map instr expl)
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
			   SEQexp [EVENTexp(evn,brexp,bvars,true), 
				   APPexp(VARexp(ref oprtmp), 
					  VARexp(ref argtmp))])
		 end
	  in case (strip opr) of
	       (opr as VARexp(ref(VALvar{access=INLINE prim,...}))) =>
		 let val arg' = instr arg
		 in APPexp(opr,arg')
                 end
	     | CONexp _ => APPexp(opr, instr arg)
             | FNexp body =>  (* really a CASE or equivalent *)
		 let val arg' = instr arg
		     val body' = instrrules(b,fn r => CASEev(arg,r),false) body
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
		     SEQexp [EVENTexp(evn,brexp,bvars,true),
			     RAISEexp(VARexp(ref argtmp))])
	  end
      | instr (exp as MARKexp(LETexp (ldec,lexp),_,_)) = 
		(* note: must be marked *)
	  let val (_,ldec') = instrdec (b,ldec)
	      val lexp' = instr lexp
  	      val evn = makeevent(LETev exp::bsites, bev)
	  in LETexp (SEQdec [ldec',
			     VALdec[VB{pat=WILDpat,
				       exp=EVENTexp(evn,brexp,bvars,false),
				       tyvars=nil}]],
		     lexp')
          end
      | instr (CASEexp(exp,rl)) = 
	  let val exp' = instr exp
	      val rl' = instrrules (b,fn r => CASEev(exp,r),false) rl
	  in CASEexp(exp',rl')
	  end
      | instr (HANDLEexp (e, HANDLER(FNexp body))) =
     	    let val e' = instr e
     	        val body' = instrrules(b,HANDLEev,true) body
     	    in HANDLEexp(e',HANDLER(FNexp body'))
            end
      | instr (FNexp body) = 
	    let val body' = instrrules (b,FNev,true) body
	    in FNexp body'
	    end
      | instr (MARKexp (exp,s,e)) =
	    let val exp' = instr exp
	    in MARKexp(exp',s,e)
	    end
      | instr exp = exp
    and instrrules (b as (brexp,bev,bsites,bvars),evf,retrieve) =
      let 
        fun f (rule as RULE(pat,exp as MARKexp(_))) = 
	     let val vars = (patvars (fn v => VARexp(ref v)) pat)
		 val bsites' = (evf rule)::bsites
		 val bvars' = vars @ bvars
		 val bvars' = if retrieve then GETBRexp::bvars' else bvars'
		 val evn = makeevent(bsites',bev)
		 val bevvar = makevar ("BIND" ^ makestring evn, intTy)
		 val exp' = instrexp2((VARexp(ref bevvar),evn,nil,nil),exp)
	     in RULE(pat, LETexp(VALdec[VB{pat=VARpat bevvar,
					   exp=EVENTexp(evn,brexp,bvars',false),
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
   If d is true, the instrumented expression returns a pair (lbr,value);
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
and instrexp3(b as (brexp:exp,bsites:event list,bvars:exp list),
	      exp:exp) : bool * exp =
(* bool: d = true iff instrumented expression discharges (any and all) 
             events (even if none to discharge 
   exp: exp' = instrumented expression. Consists of a tuple (lbr,value)
                iff d = true, else just a value.
*)
  let
    fun discharge (allsites:bool,
		   f:(exp * event list * exp list) -> exp) =
	(* Discharge any function events (any events at all if allsites true)
	   before executing expression constructed by f. *)
	  let val (bropt,b as (brexp,bsites,bvars)) =
	         if (allsites andalso (useInstrumLevel = 3) 
		     andalso (not (null bsites))) 
		   orelse (hasfnsite bsites) then
		   let val brvar = makenvar "brvar"
		       val evn = makeevent(bsites,0)
		       val brdec = 
			   VALdec[VB{pat=VARpat brvar,
				     exp=EVENTexp(evn,brexp,bvars,false),
				     tyvars=nil}]
		   in (SOME brdec,(VARexp(ref brvar),nil,nil))
		   end
		 else (NONE,b)
	      val exp' = f b
          in case bropt of
	       SOME brdec =>
		 (true,LETexp(brdec,TUPLEexp[brexp,exp']))   (* !! *)
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
	      fun g (b as (brexp,bsites,bvars),(fieldtmp,exp)::rest) = 
		    let val (d,exp') = instrexp3(b,exp)
		    in if d then
			 let val brvar = makenvar "brvar"
		             val dec' = VALdec[VB{pat=TUPLEpat[VARpat brvar,
							       VARpat fieldtmp],
						  exp=exp',
						  tyvars=nil}]
			     val (_,rest') = g((VARexp(ref brvar),nil,nil),rest)
			 in (SOME(VARexp(ref brvar)),dec'::rest')
                         end
		       else
			 let val dec' = VALdec[VB{pat=VARpat fieldtmp,
						  exp=exp',
						  tyvars=nil}]
			     val (bropt,rest') = g(b,rest)
			 in (bropt,dec'::rest')
			 end
		    end
                | g (_,nil) = (NONE,nil)
	      val (bropt,ld') = g(b,ld)
	      (* avoid decl's when possible, for cosmetic reasons *)
	      fun h ((lab,_)::rl,(VALdec[VB{exp,...}])::re) = 
		         (lab,exp)::(h(rl,re))
		| h (nil,nil) = nil
		| h _ = impossible "DebugNInstrum.instrexp RECexp"
	  in case bropt of
	       SOME (brexp') => (true,
				 LETexp(SEQdec ld',
					TUPLEexp[brexp',
						 RECORDexp lv]))  (* !! *)
	     | NONE => (false, RECORDexp(h(lv,ld')))
	  end 
      | instr (SEQexp expl) =
	  let 
	    fun g(usebr,b as (brexp,bsites,bvars),exp::rest) =
		  let val (d,exp') = instrexp3(b,exp)
		  in if d then
		       let val brvar = makenvar "brvar"
			   val valvar = makenvar "valvar"
			   val (_,rest') = 
			       g(false,(VARexp(ref brvar),nil,nil),rest)
			   val useexp = 
			      if usebr then
				TUPLEexp[VARexp(ref brvar),
					 SEQexp(VARexp(ref valvar)::
						rest')]
			      else SEQexp(VARexp(ref valvar)::rest')
		       in (true,
			   [LETexp(VALdec[VB{pat=TUPLEpat[VARpat brvar,
							  VARpat valvar],
					    exp=exp',
					    tyvars=nil}],
				   useexp)])
		       end
		     else 
		       let val (d',rest') = g(usebr,b,rest)
		       in (d',exp'::rest')
		       end
		  end
	      | g(_,_,nil) = (false,nil)
	    val (d,expl') = g(true,b,expl)
          in (d,SEQexp expl')                          (* !! *)
          end
      | instr (exp as APPexp(opr,arg)) =
	  let fun strip (MARKexp(exp,_,_)) = strip exp
		| strip (CONSTRAINTexp(exp,_)) = strip exp
		| strip (SEQexp[exp]) = strip exp
		| strip exp = exp
	      fun normal () =
		 let val oprtmp = makenvar "oprvar"
		     val argtmp = makenvar "argvar"
		     val brvar = makenvar "brvar"
		     val (dopr,opr') = instr opr
		     val oprpat =
			if dopr then
			  TUPLEpat[VARpat brvar,VARpat oprtmp]
			else VARpat oprtmp
		     val (darg,arg') =
		       if dopr then
			 instrexp3((VARexp(ref brvar),nil,nil),arg)
		       else instr arg
		     val argpat = 
		       if darg then
                         if dopr then
			   TUPLEpat[WILDpat, VARpat argtmp]
			 else TUPLEpat[VARpat brvar,VARpat argtmp]
		       else VARpat argtmp
		     val appexp = APPexp(VARexp(ref oprtmp),
					 VARexp(ref argtmp))
		     val fullexp =
		        if dopr orelse darg then 
			  let val evn = makeevent([APPev exp],0)
			  in TUPLEexp[VARexp(ref brvar),
				      SEQexp[EVENTexp(evn,VARexp(ref brvar),nil,true),
					     appexp]]
			  end
			else 
			  let val evn = makeevent (APPev exp::bsites,0)
			  in TUPLEexp[EVENTexp(evn,brexp,bvars,true),
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
	       (opr as VARexp(ref(VALvar{access=INLINE prim,...}))) =>
		 let val (darg,arg') = instr arg
		     val argtmp = makenvar "argvar"
		 in if darg then
		      let val brvar = makenvar "brvar"
		      in (true,LETexp(VALdec[VB{pat=TUPLEpat[VARpat brvar,
							     VARpat argtmp],
						exp=arg',
						tyvars=nil}],
				      TUPLEexp[VARexp(ref brvar),
					       APPexp(opr,
						      VARexp(ref argtmp))]))
		      end
		    else if (hasfnsite bsites) 
			   andalso (Prim.mayRaise prim) then
		      let val evn = makeevent(APPev exp::bsites,0)
		      in (true,LETexp(VALdec[VB{pat=VARpat argtmp,
						exp=arg',
						tyvars=nil}],
				      TUPLEexp[EVENTexp(evn,brexp,bvars,true),
					       APPexp(opr,
						      VARexp(ref argtmp))]))
		      end
  		    else (false,APPexp(opr,arg'))
		 end
	     | CONexp _ => 
		 let val (darg,arg') = instr arg
		 in if darg then
		      let val brvar = makenvar "brvar"
			  val argtmp = makenvar "argtmp"
		      in (true,LETexp(VALdec[VB{pat=TUPLEpat[VARpat brvar,
							     VARpat argtmp],
						exp=arg',
						tyvars=nil}],
				      TUPLEexp[VARexp(ref brvar),
					       APPexp(opr,
						      VARexp(ref argtmp))]))
		      end
		    else (false,APPexp(opr,arg'))
		 end
             | FNexp body =>  (* really a CASE or equivalent *)
		 let val (darg,arg') = instr arg
		 in if darg then
		      let val valvar = makenvar "valtmp"
			  val brvar = makenvar "brvar"
			  val body' = instrrules((VARexp(ref brvar),nil,nil),
						fn r => CASEev(arg,r),false) body
		      in (true,
			  LETexp(VALdec[VB{pat=TUPLEpat[VARpat brvar,
							VARpat valvar],
					   exp=arg',
					   tyvars=nil}],
				 TUPLEexp[VARexp(ref brvar),
					  APPexp(FNexp body',
						 VARexp(ref valvar))]))
		      end
		    else 
		      discharge (true,fn b =>
		        let val body' = 
				 instrrules(b,fn r => CASEev(arg,r),false) body
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
		   val brvar = makenvar "brvar"
	       in (true,
		   LETexp(VALdec[VB{pat=TUPLEpat[VARpat brvar,
						 VARpat argtmp],
				    exp=arg',
				    tyvars=nil}],
			  TUPLEexp[VARexp(ref brvar),
				   SEQexp[EVENTexp(evn,VARexp(ref brvar),nil,true),
					  RAISEexp(VARexp(ref argtmp))]]))
               end
             else 
	       let val evn = makeevent(RAISEev exp::bsites,0)
               in (true,
		   LETexp(VALdec[VB{pat=VARpat argtmp,
			 	    exp=arg',
				    tyvars=nil}],
			  TUPLEexp[EVENTexp(evn,brexp,bvars,true),
				   RAISEexp (VARexp(ref argtmp))]))
	       end
	   end
      | instr (exp as MARKexp(LETexp (ldec,lexp),_,_)) = 
		(* note: must be marked *)
	  (* We discharge any events before executing LET. *)
	  discharge (true,fn (b as (brexp,bsites,bvars)) =>
	      (* assume instrdec returns a brexp' valid in context of ldec' *)
	    let val ((brexp',_,bsites',bvars'),ldec') =
		           instrdec ((brexp,0,bsites,bvars),ldec)
		val (d',lexp') = instrexp3((brexp',bsites',bvars'),lexp)
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
					      exp=EVENTexp(evn,brexp',bvars',false),
					      tyvars=nil}]],
			     lexp')
		   end
	    end)
      | instr (CASEexp(exp,rl)) = 
	  let val (d,exp') = instr exp
	  in if d then
	       let val valvar = makenvar "valtmp"
		   val brvar = makenvar "brvar"
		   val rl' = instrrules((VARexp(ref brvar),nil,nil),
				       fn r => CASEev(exp,r),false) rl
	       in (true,
		   LETexp(VALdec[VB{pat=TUPLEpat[VARpat brvar,VARpat valvar],
				    exp=exp',
				    tyvars=nil}],
			  TUPLEexp[VARexp(ref brvar),
				   CASEexp(VARexp(ref valvar),
					   rl')]))
	       end
	     else 
               discharge (true,fn b =>		 
		 let val rl' = instrrules(b,fn r => CASEev(exp,r),false) rl
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
		val body' = instrrules(b,HANDLEev,true) body
	    in HANDLEexp(e',HANDLER(FNexp body'))
	    end)
      | instr (FNexp body) =
	  (* We discharge any function events before executing FNexp *)
	  discharge (false,fn b =>
	    let val body' = instrrules(b,FNev,true) body
	    in FNexp body'
	    end)
      | instr (MARKexp (exp,s,e)) =
          let val (d,exp') = instr exp
          in (d, MARKexp(exp',s,e))
          end
      | instr exp = (false,exp)
    and instrrules (b as (brexp,bsites,bvars),evf,retrieve) =
	(* always return just a value *)
      let 
        fun f (rule as RULE(pat,exp as MARKexp(_))) = 
	     let val vars = (patvars (fn v => VARexp(ref v)) pat)
		 val bsites' = (evf rule)::bsites
		 val bvars' = vars @ bvars
		 val bvars' = if retrieve then GETBRexp :: bvars' else bvars'
		 val (d,exp') = instrexp3((brexp,bsites',bvars'),exp)
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
			  SEQexp[EVENTexp(evn,brexp,bvars',false),
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



and instrdec (b as (brexp:exp, bev:int, bsites:event list,bvars: exp list),
	     dec:dec) : 
	((exp * int * (event list) * (exp list)) * dec) =
  let
    fun instr (dec as MARKdec(VALdec vbl,_,_)) =
          if useInstrumLevel = 2 then
	    let val vars = vblextract (fn v => VARexp(ref v)) vbl
		fun f (VB{pat,exp,tyvars}) =
		    VB{pat=pat,exp=instrexp2(b,exp),tyvars=tyvars}
	    in ((brexp,bev,VALev dec::bsites,vars@bvars),VALdec(map f vbl))
	    end
          else (* useInstrumLevel = 3 or 4 *)
	    let val vars = vblextract (fn v => VARexp(ref v)) vbl
		fun g(VB{pat,exp,tyvars}::rest,brexp,bsites,bvars) =
		     let val (d,exp') = instrexp3((brexp,bsites,bvars),exp)
		     in if d then
			  let val brvar = makenvar "brvar"
			      val (_,rest') = g(rest,VARexp(ref brvar),nil,nil)
			      val dec' =
			         VALdec[VB{pat=TUPLEpat[VARpat brvar,
							pat],
					   exp=exp',
					   tyvars=nil}]
			  in (SOME(VARexp(ref brvar)),dec'::rest')
			  end
			else
			  let val dec' = VALdec[VB{pat=pat,
						   exp=exp',
						   tyvars=nil}]
			      val (bropt,rest') = g(rest,brexp,bsites,bvars)
			  in (bropt,dec'::rest')
			  end
                     end
		  | g (nil,_,_,_) = (NONE,nil)
	       val (bropt,decl') = g(vbl,brexp,bsites,bvars)
	       val (brexp',bsites',bvars') =
		  case bropt of
		    SOME brexp' => (brexp',nil,nil)
		  | NONE => (brexp,bsites,bvars)
	    in ((brexp',0,VALev dec::bsites',vars@bvars'),
		SEQdec decl')
	    end
      | instr (dec as MARKdec(VALRECdec rvbl,_,_)) =
            (* N.B. Bodies cannot discharge vars ! *)
	if useInstrumLevel = 2 then
	    let val vars = map (fn RVB{var,...} => VARexp(ref var)) rvbl
		val b' = (brexp,bev,VALRECev dec::bsites,vars @ bvars)
		val rvbl' = map (fn (RVB{var,exp,resultty,tyvars}) =>
				     let val exp' = instrexp2(b',exp)
				     in RVB {var=var,
					     exp=exp',
					     resultty=resultty,
					     tyvars=tyvars}
				     end) rvbl
	    in (b',VALRECdec rvbl')
	    end
	  else (* useInstrumLevel = 3 or 4 *)
	    let val vars = map (fn RVB{var,...} => VARexp(ref var)) rvbl
		val bsites' = VALRECev dec::bsites
		val bvars' = vars@bvars
		fun g (RVB{var,exp,resultty,tyvars}) =
		     let val (d,exp') = instrexp3((brexp,bsites',bvars'),exp)
		     in if d then
			 debugPanic "instrexp3 VALRECdec"
			else RVB{var=var,
				 exp=exp',
				 resultty=resultty,
				 tyvars=tyvars}
		     end
                val rvbl' = map g rvbl
	    in ((brexp,bev,bsites',bvars'),VALRECdec rvbl')
	    end
      | instr (SEQdec decl) =
	    let val (b', decl') = instrlist (b, decl)
	    in (b', SEQdec decl')
	    end
      | instr (dec as MARKdec(LOCALdec(localdec, visibledec),_,_)) = 
	    let val (b' as (brexp',bev',bsites',bvars'), localdec') = 
		   instrdec ((brexp,bev,LOCALev dec::bsites,bvars),localdec)
		val (b'' as (brexp'',bev'',bsites'',bvars''), visibledec') =
		   instrdec ((brexp',bev',LOCINev dec::bsites',bvars'),visibledec)
	    in if exists (fn LOCINev _ => true | _ => false) bsites'' then
		 let val (b',indec) = simplebind(b',[LOCINev dec],nil,false)
		     val (b'' as (brexp'',bev'',bsites'',bvars''),visibledec')=
				 instrdec (b',visibledec)
		 in ((brexp'',bev'',LOCENDev dec::bsites'',bvars''),
   	  	     LOCALdec(localdec',SEQdec[indec,visibledec']))
		 end
	       else 
		 ((brexp'',bev'',LOCENDev dec::bsites'',bvars''),
	          LOCALdec(localdec',visibledec'))
	    end
      | instr (dec as MARKdec(ABSTYPEdec {abstycs,withtycs,body},_,_)) =
	    let val ((brexp',bev',bsites',bvars'), body') = instr body
	    in ((brexp',bev',TYPEev dec::bsites',bvars'),
		ABSTYPEdec {abstycs=abstycs,withtycs=withtycs,body=body'})
	    end
      | instr (dec as MARKdec(STRdec strbl,_,_)) =
            let val (brarrdec,brarrexp) = makebrarr (length strbl)
	        val (bevl,strbl',strvl) = instrstrbl brarrexp strbl
		val dec' = SEQdec [brarrdec, STRdec strbl']
	    in ((brexp,
		 bev,
                 STRev (dec,bevl)::bsites,
		 brarrexp::(strvl @ bvars)), 
                dec')
	    end
      | instr (dec as MARKdec(ABSdec strbl,_,_)) =
            let val (brarrdec,brarrexp) = makebrarr (length strbl)
	        val (bevl,strbl',strvl) = instrstrbl brarrexp strbl
		val dec' = SEQdec [brarrdec, ABSdec strbl']
	    in ((brexp,
		 bev,
                 ABSev (dec,bevl)::bsites,
		 brarrexp::(strvl @ bvars)), 
                dec')
	    end
      | instr (dec as MARKdec(FCTdec fctbl,_,_)) =
	    let 
              fun instrfctb(fctb as FCTB{fctvar,param,def,thin,constraint}) =
                let val (b',entdec) = simplebind(b,[FCTENTev fctb],[GETBRexp],false)
                    val (bev,def') = instrstrexp(b',def)
                in (bev, FCTB{fctvar=fctvar,param=param,
		                 def=LETstr(entdec,def'),
		                 thin=thin,constraint=constraint})
                end
	      fun tovar (FCTB{fctvar=FCTvar{name,access=PATH[lv],...},...}) =
		    fakeuse(name,lv)
	      val fctvl = map tovar fctbl
	      val (bevl,fctbl') = unpairlist (map instrfctb fctbl)
	    in ((brexp,bev,FCTev (dec,bevl)::bsites,fctvl @ bvars),
		FCTdec fctbl')
            end
      | instr (dec as MARKdec(SIGdec _,_,_)) =
	    ((brexp,bev,SIGev dec::bsites,bvars),dec)
      | instr (dec as MARKdec(TYPEdec _,_,_)) =
            ((brexp,bev,TYPEev dec::bsites,bvars),dec)
      | instr (dec as MARKdec(FIXdec _,_,_)) =
	    ((brexp,bev,FIXev dec::bsites,bvars),dec)
      | instr (dec as MARKdec (OVLDdec _,_,_)) =
	    ((brexp,bev,OVLDev dec::bsites,bvars),dec)
      | instr (dec as MARKdec(DATATYPEdec _,_,_)) =
            ((brexp,bev,TYPEev dec::bsites,bvars),dec)
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
               ((brexp,bev, EXCEPTIONev dec::bsites,ebreps @ bvars),dec)
	    end
      | instr (dec as MARKdec(OPENdec _,_,_)) =
	    ((brexp,bev,OPENev dec::bsites,bvars),dec)
      | instr (MARKdec (dec,s,e)) = 
	    let val (b',dec') = instr dec
	    in (b', MARKdec (dec',s,e))
	    end
      | instr dec = (b, dec)

   and instrstrbl brarrexp strbl = 
         let 
           fun dostrb (STRB{strvar,def,thin,constraint},(n,bevl,strbl))= 
	        let val (bev',def') = instrstrexp(b,def)
		    val strb' = STRB{strvar=strvar,def=def',thin=thin,
				       constraint=constraint}
		    val brstrvar = anonstrvar()
		    val brstr = STRUCTstr
		           {body=[VALdec[VB{pat=WILDpat,
				            exp=APPexp(VARexp(ref updateop),
					  	       TUPLEexp[brarrexp,
						   	        INTexp n,
						   		GETBRexp]),
					    tyvars=nil}]],
			    locations=nil,
			    str=NULLstr}
	            val brstrb = STRB{strvar=brstrvar,
				        def=brstr, thin=NONE,constraint=NONE}
		in (n-1,
		    bev' :: bevl,
	            strb' :: (brstrb :: strbl))
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
       	       simplebind(b',[STRENDev mstrexp],nil,true)
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
       		simplebind(b,[FCTAPPev mstrexp],[GETBRexp,paramv],true)
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
       		simplebind(b,[STRVARev mstrexp],nil,true)
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
      val STARTexp = FEVENTexp(startevn,GETBRexp,nil)
      val STARTdec = VALdec[VB{pat=VARpat(bevvar),exp=STARTexp,tyvars=nil}]
      val ((brexp,bev,bsites,bvars),absyn') = 
 	     instrdec((VARexp (ref bevvar),startevn,nil,nil),absyn)
      val endevn = makeevent(ENDev absyn::bsites,bev)
      val ENDexp = EVENTexp(endevn,brexp,bvars,true)
      val ENDdec = VALdec[VB{pat=WILDpat,exp=ENDexp,tyvars=nil}]
  in SEQdec [STARTdec,
	     absyn',
	     ENDdec]
  end


in 
  {absyn=LOCALdec(VALdec [VB {pat = TUPLEpat [VARpat brvar,
					      VARpat eventBreaks,
					      VARpat breakentry,
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
