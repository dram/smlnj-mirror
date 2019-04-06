signature DEBUGINSTRUM = sig
   val instrumDec: DbgStat.filename * Absyn.dec -> Absyn.dec
   val commit: unit -> unit
   val reset:unit -> unit
   val hideFile: DbgStat.filename -> unit
   val eventsAfter: DbgStat.filename * DbgStat.charno -> 
		(*	(DbgStat.evnum * DbgStat.evindex) list list *)
			DbgStat.evnum list
   val eventsBefore:DbgStat.filename * DbgStat.charno -> 
		(*	(DbgStat.evnum * DbgStat.evindex) list list *)
			DbgStat.evnum list
   val locOfEvent: DbgStat.event -> DbgStat.charno
   val argCnt: DbgStat.event -> int
end

structure DbgInstr : DEBUGINSTRUM =
struct

open DbgUtil DbgStat Access Absyn Basics BasicTyp ErrorMsg

val currentCud = ref initialCud
val nextCud = ref initialCud

(* this version computed spots...
fun locOfEvent (cp2lnf:int->string*int*int): (event->spot) =
  let fun f (p,s,e) = 
        let val (filename,pline,ppos) = cp2lnf p
            val (_,sline,spos) = cp2lnf s
	    val (_,eline,epos) = cp2lnf e
        in (filename,(pline,ppos),(sline,spos),(eline,epos))
        end
  in 
   fn VALev(MARKdec(_,s,e)) => f (s,s,e)
    | VALRECev(MARKdec(_,s,e)) => f (s,s,e)
    | FNev(RULE(_,MARKexp(_,s,e))) => f (s,s,e)
    | FNev(_) => ("dummy",(0,0),(0,0),(0,0))
    | CASEev(_,RULE(_,MARKexp(_,s,e))) => f (s,s,e)
    | CASEev(_) => ("dummy",(0,0),(0,0),(0,0))
    | APPev(exp) =>
	let fun g (MARKexp(_,s,e)) = f (s,s,e)
	      | g (CONSTRAINTexp(e,_)) = g e
	      | g (SEQexp[e]) = g e
	      | g (APPexp(opr,RECORDexp[(_,arg1),(_,arg2)])) =
		     let val (filename,oprp,_,_) = g opr
			 val (_,_,arg1s,_) = g arg1
		         val (_,_,_,arg2e) = g arg2
		     in (filename,oprp,arg1s,arg2e)
		     end
	      | g (APPexp(opr,arg)) = 
		     let val (filename,_,oprs,opre) = g opr
			 val (_,_,_,arge) = g arg
	             in (filename,opre,oprs,arge)
		     end
	      | g _ = debugPanic "bad APPev marking in instrum.locOfEvent"
        in g exp
        end
    | RAISEev(MARKexp(_,s,e)) => f (s,s,e)
    | HANDLEev(RULE(_,MARKexp(_,s,e))) => f (s,s,e)
    | HANDLEev(_) => ("dummy",(0,0),(0,0),(0,0))
    | STRev(MARKdec(_,s,e),_) => f (s,s,e)
    | ABSev(MARKdec(_,s,e),_) => f (s,s,e)
    | FCTev(MARKdec(_,s,e),_) => f (s,s,e)
    | FCTENTev(FCTB{def=MARKstr(_,s,e),...}) => f (s,s,e)
    | FCTAPPev(MARKstr(_,s,e)) => f (s,s,e)
    | STRENDev(MARKstr(_,s,e)) => f (s,s,e)
    | STRVARev(MARKstr(_,s,e)) => f (s,s,e)
    | OPENev(MARKdec(_,s,e)) => f (s,s,e)
    | LETev(MARKexp(_,s,e)) => f (s,s,e)
    | LOCALev(MARKdec(_,s,e)) => f (s,s,e)
    | LOCINev(MARKdec(_,s,e)) => f (s,s,e)
    | LOCENDev(MARKdec(_,s,e)) => f (s,s,e)
    | IOev => ("",(0,0),(0,0),(0,0))
    | NULLev => ("",(0,0),(0,0),(0,0))
    | _ => debugPanic "bad event type in instrum.locOfEvent"
  end
*)

val locOfEvent' =
(* Return a character position corresponding to a given event.
 * The event marker actually falls between characters (or between tokens,
 * since white space is insignificant); its position is immediately BEFORE
 * the returned character position.
 *
 * For each match below, an event of the matching type will appear at each
 * position marked <*> in the following comment. *)

   fn VALev(MARKdec(_,s,e)) => e
	    (* val a = 7 <*> *)

    | VALRECev(MARKdec(_,s,e)) => e
	    (* val rec a = 7 <*> *)

   | FNev(RULE(_,MARKexp(_,s,e))) => s
	    (* (fn a => <*> a + 1) *)              (* explicit fn *)
	    (* fun f a = <*> a + 1 *)              (* implicit fn *)
	    (* fun f a <*> b <*> c <*> = a + b + c *)
				(* nested implicit fn's -- N.B. doesn't work*)
    | FNev(_) => debugPanic "bad FNev marking in instrum.locOfEvent"

    | CASEev(_,RULE(_,MARKexp(_,s,e))) => s
	    (* case a of 1 => <*> b | 2 => <*> c *)
    | CASEev(_) => debugPanic "bad CASEev marking in instrum.locOfEvent"

    | APPev(APPexp(opr,_)) => 
        let fun g (MARKexp (_,s,e)) = e 
              | g (CONSTRAINTexp(e,_)) = g e
 	      | g (SEQexp[e]) = g e
	      | g (RECORDexp[(_,arg1),(_,arg2)]) = g arg2
	      | g (APPexp(opr,arg)) = g arg
              | g _ = debugPanic "bad APPev marking in instrum.locOfEvent"
	in g opr
        end
	    (* f <*> b *)	                 (* non-infixed application *)
	    (* infix add                         (* infixed application *)
	     * 3 add <*> 4 *)

    | RAISEev(MARKexp(_,s,e)) => s
	    (* raise <*> Match *)

    | HANDLEev(RULE(_,MARKexp(_,s,e))) => s
	    (* handle Match => <*> raise Overflow *)
    | HANDLEev(_) => debugPanic "bad HANDLEev marking in instrum.locOfEvent"

    | STRev(MARKdec(_,s,e),_) => e 
 	    (* structure a = struct val d = 7 end <*> *)

    | ABSev(MARKdec(_,s,e),_) => e 
	    (* abstraction a: ABSA = struct val d = 7 end <*> *)

    | FCTev(MARKdec(_,s,e),_) => e 
	    (* functor afunct (b:C) = struct end <*> *)

    | FCTENTev(FCTB{def=MARKstr(_,s,e),...}) => s
	    (* functor afunct (b:C) = <*> struct end *)

    | FCTAPPev(MARKstr(_,s,e)) => e
	    (* structure d = afunct<*>(b) *)

    | STRENDev(MARKstr(_,s,e)) => e
	    (* structure a = struct val b = 7 <*> end *)

    | STRVARev(MARKstr(_,s,e)) => e
	    (* structure a = b <*> *)

    | OPENev(MARKdec(_,s,e)) => e 
	    (* open System.Control.Runtime <*> *)

    | LETev(MARKexp(_,s,e)) => e 
	    (* let val a = 5 val b = 7 in <*> c end *)

    | LOCALev(MARKdec(_,s,e)) => e
	    (* local <*> val b = 7 in val c = b end *)

    | LOCINev(MARKdec(_,s,e)) => e
	    (* local val b = 7 in <*> val c = b end *)

    | LOCENDev(MARKdec(_,s,e)) => e
	    (* local val b = 7 in val c = b <*> end *)

    | IOev => 0
    | STARTev (MARKdec(_,s,e)) => s
    | STARTev (_) => 0
    | ENDev (MARKdec(_,s,e)) => e 
    | ENDev (_) => 0
    | NULLev => 0
    | _ => debugPanic "bad event type in instrum.locOfEvent"

fun locOfEvent x = locOfEvent' x - 1  (* account for parser weirdness *)


fun dumpEvents (firstev:int,events:event list array,elb:int array) =
  let fun pr_one evt = 
	let val charno = locOfEvent evt
	in print ("\t" ^ (eventText evt) ^ "\t"); print charno; print "\n"
        end
      fun pr n = let val evt = events sub n 
		 in print (firstev+n); 
                    app pr_one evt;
		    print "\t==> "; print (elb sub n); print "\n";
		    pr (n+1)
		 end  handle Subscript => ()
  in print "Events:\n";    
     pr 0
  end		 


(*  Fancy version dealing with fine events ....
(* LocIndex handling *)

structure CharnoSet = SortSet (
struct
  type t = charno * ((event * (evnum * evindex) list) list)
  type k = charno
  fun key (charno,wl) = charno
  val lt = Integer.<
end)

type locindex = (filename * CharnoSet.s * (cud list)) list

val emptyLocIndex = nil:locindex
val currentLocIndex = ref emptyLocIndex
val nextLocIndex = ref emptyLocIndex

fun augmentLocIndex (oldIndex, cud as {file,firstev,events,...}:cud) =
  let fun putin (cs,evt,charno,w) =
        let open CharnoSet
	in if charno > 0 then
             let val (_,ewll) = find(cs,charno)
		 fun q ((evt',wl)::rest) =
		       if evt = evt' then
			 (evt',w::wl)::rest
		       else (evt',wl)::(q rest)
		   | q nil = [(evt,[w])]
	     in update (cs, (charno,q ewll))
	     end handle NotFound => insert (cs, (charno,[w]))
           else cs
        end
      fun augcs cs =
	let fun d (evn,cs) = 
              let fun g (cs,n,evt::rest) = 
			  g(putin(cs,evt,locOfEvent evt,(evn+firstev,n)),n+1,rest)
		    | g (cs,_,nil) = cs
              in d(evn+1, g (cs,0,events sub evn))
              end handle Subscript => cs
        in d(0,cs)
	end
      fun augf ((f as (file',cs,cuds))::rest) =
	    if file = file' then 
	      (file',augcs cs,cud::cuds)::rest
            else f::(augf rest)
        | augf nil = [(file,augcs (CharnoSet.new()),[cud])]
  in augf oldIndex
  end

fun eventsAfter (file:filename, charno:charno) : (evnum * evindex) list list =
  let fun find ((file',cs,_)::rest) = if file = file' then cs else find rest
	| find nil = raise CharnoSet.NotFound
      val cs = find (!nextLocIndex)
      val (_,ewll) = CharnoSet.finds (cs,charno)
  in map (fn (e,wl) => wl) ewll
  end handle CharnoSet.NotFound => nil

fun eventsBefore (file:filename, charno:charno) :(evnum * evindex) list list =
  let fun find ((file',cs,_)::rest) = if file = file' then cs else find rest
	| find nil = raise CharnoSet.NotFound
      val cs = find (!nextLocIndex)
      val (_,ewll) = CharnoSet.findp (cs,charno)
  in map (fn (e,wl) => wl) ewll
  end handle CharnoSet.NotFound => nil

....end of fancy version *)

(* LocIndex handling *)
(* this version handles only coarse events *)

structure CharnoSet = SortSet (
struct
  type t = charno * (evnum list)
  type k = charno
  fun key (charno,wl) = charno
  val lt = Integer.<
end)

type locindex = (filename * CharnoSet.s * (cud list)) list

val emptyLocIndex = nil:locindex
val currentLocIndex = ref emptyLocIndex
val nextLocIndex = ref emptyLocIndex

fun augmentLocIndex (oldIndex, cud as {file,firstev,events,...}:cud) =
  let fun putin (cs,charno,evnum) =
        let open CharnoSet
	in if charno > 0 then
             let val (_,evnuml) = find(cs,charno)
	     in update (cs, (charno,evnum::evnuml))
	     end handle NotFound => insert (cs, (charno,[evnum]))
           else cs
        end
      fun augcs cs =
	let fun d (evn,cs) = 
	      d(evn+1, putin(cs,locOfEvent(hd (events sub evn)),evn+firstev))
              	 handle Subscript => cs
        in d(0,cs)
	end
      fun augf ((f as (file',cs,cuds))::rest) =
	    if file = file' then 
	      (file',augcs cs,cud::cuds)::rest
            else f::(augf rest)
        | augf nil = [(file,augcs (CharnoSet.new()),[cud])]
  in augf oldIndex
  end

fun eventsAfter (file:filename, charno:charno) : evnum list =
  let fun find ((file',cs,_)::rest) = if file = file' then cs else find rest
	| find nil = raise CharnoSet.NotFound
      val cs = find (!nextLocIndex)
      val (_,evnuml) = CharnoSet.finds (cs,charno)
  in evnuml
  end handle CharnoSet.NotFound => nil

fun eventsBefore (file:filename, charno:charno) : evnum list =
  let fun find ((file',cs,_)::rest) = if file = file' then cs else find rest
	| find nil = raise CharnoSet.NotFound
      val cs = find (!nextLocIndex)
      val (_,evnuml) = CharnoSet.findp (cs,charno)
  in evnuml
  end handle CharnoSet.NotFound => nil

fun hideFile (file:filename) =
  let fun zap ((f as (file',_,cuds))::rest) = 
	    if file = file' then
	      (app hideCud cuds;
	       rest)
	    else f::(zap rest)
        | zap nil = nil
  in currentLocIndex := zap (!currentLocIndex) 
  end 

fun argCnt evt =
  case evt of 
    VALev(MARKdec(VALdec vbl,_,_)) => length (vblextract (fn x=>x) vbl)
  | VALRECev(MARKdec(VALRECdec rvbl,_,_)) => length rvbl
  | FNev(RULE(pat,_)) => length (patvars (fn x => x) pat)
  | HANDLEev(RULE(pat,_)) => length (patvars (fn x => x) pat)
  | CASEev(_,RULE(pat,_)) => length (patvars (fn x => x) pat)
  | STRev(_) => 1
  | ABSev(_) => 1
  | _ => 0
   
val toaccess = fn (VALvar{access=LVAR v,name,typ}) =>
		    VARexp(ref (VALvar{access=PATH[v],name=name,typ=typ}))

fun makevar(str,t) = 
     let val name = Symbol.symbol str
	 val lvar = namedLvar name
	 val reft = ref t
     in (VALvar{name=[name],access=LVAR(lvar),typ=reft},
	 VALvar{name=[name],access=PATH[lvar],typ=reft})
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


fun instrumDec(file',absyn) =
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

fun FEVENTexp (evn,lbt,args) =
     let val (ntimedec,ntimeexp) = makent "newtime"
     in LETexp (ntimedec,
	        SEQexp [SETTIMEexp ntimeexp,
		        APPexp(VARexp (ref breakentrya),
			       TUPLEexp(INTexp evn::lbt::args)),
	                NOTETIMEexp(ntimeexp,evn),
			ntimeexp])
     end


(* Because ref is a constructor that cannot be replaced at top level, 
    we replace it explicitly here: *)

val (hcreaterb, hcreatera) = makevar ("hcreater", UNDEFty)
val HREFexp = VARexp(ref hcreatera)

(* For efficiency, we implement

fun hass (objr,value) =
        (updatedRList := (weak objr) :: (!updatedList);
	 objr := value)

in-line. Note we maintain type information in opr used by the 
garbage collector.
*)


val (weakb,weaka) = makevar ("weak",UNDEFty)
val (udlb,udla) = makevar ("udl",UNDEFty)
fun HASSexp opr = 
     let val (objb,obja) = makevar ("obj",UNDEFty)
	 val (valb,vala) = makevar ("val",UNDEFty)
	 val objexp = VARexp(ref obja)
         val valexp = VARexp(ref vala)
      (* val newobj = APPexp(VARexp(ref weaka),objexp)  *)
         val newobj = APPexp(VARexp(ref delayop), 
			     TUPLEexp[INTexp 22,objexp]) 
	 val oldlist = APPexp(VARexp(ref derefop),VARexp(ref udla))
     in FNexp [RULE(TUPLEpat[VARpat objb,VARpat valb],
	            SEQexp[APPexp(VARexp(ref assop),
	                          TUPLEexp[VARexp(ref udla),
			                   APPexp(CONexp(consDcon),
				           TUPLEexp[newobj,oldlist])]),
	                   APPexp(VARexp(ref opr), 
		                  TUPLEexp[objexp,valexp])]),
	       RULE(WILDpat,INTexp 0)]
     end

fun simplebind ((btexp,bev,bsites,bvars),ev,vars) =
       let val evn = makeevent(ev@bsites,bev)
	   val (bevb,beva) = makevar("BIND" ^ makestring evn,intTy)
	   val evndec = VALdec[VB{pat=VARpat(bevb),
		                    exp=EVENTexp(evn,btexp,vars@bvars),
				    tyvars=nil}]
       in ((VARexp(ref beva),evn,nil,nil),evndec)
       end

fun hasfnsite (FNev _::_) = true
  | hasfnsite (HANDLEev _::_) = true
  | hasfnsite (_::rest) = hasfnsite rest
  | hasfnsite (nil) = false


(* provocative returns true on those primitives which might raise an exception *)
fun provocative (P.*) = true
  | provocative (P.+) = true
  | provocative (P.-) = true
  | provocative (P.div) = true
  | provocative (P.fadd) = true
  | provocative (P.fdiv) = true
  | provocative (P.feql) = true
  | provocative (P.fmul) = true
  | provocative (P.fsub) = true
  | provocative _ = false

fun instrexp(ntexp:exp,
	     b as (btexp:exp,bev:int,bsites:event list,bvars:exp list),
	     exp:exp) : bool * (exp * int * event list * exp list) * exp =
  let 
    fun instr (RECORDexp l) = 
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
      | instr (CONexp(DATACON{rep=REF,...})) = (false,b,HREFexp)
      | instr (VARexp(ref(assopr as VALvar{access=INLINE(P.:=),...}))) =
		(false,b,HASSexp assopr)
      | instr (exp as APPexp(opr,arg)) =
	  let fun strip (MARKexp(exp,_,_)) = strip exp
		| strip (CONSTRAINTexp(exp,_)) = strip exp
		| strip (SEQexp[exp]) = strip exp
		| strip exp = exp
	      fun normal () =
		 let val (_,b',opr') = instr opr
		     val (_,b' as (btexp',bev',bsites',bvars'),arg') = 
			       instrexp (ntexp,b',arg)
	             fun newb (b as (_,_,nil,_),_) = b
      	               | newb (_,evn) = (ntexp,evn,nil,nil)
		     val (oprtmpb, oprtmpa) = makevar ("oprvar",UNDEFty)
		     val (argtmpb, argtmpa) = makevar ("argvar",UNDEFty)
		     val evn = makeevent(APPev exp::bsites',bev')
		 in (true,
		     newb (b',evn),
		     LETexp (VALdec [VB{pat = VARpat(oprtmpb), 
					exp = opr', 
					tyvars = nil},
				     VB{pat = VARpat(argtmpb), 
					exp = arg', 
					tyvars = nil}],
			    SEQexp [EVENTexp(evn, btexp',bvars'), 
				    APPexp(VARexp(ref oprtmpa), 
					   VARexp(ref argtmpa))]))
		 end
	  in case (strip opr) of
	       VARexp(ref(VALvar{access=INLINE(P.:=),...})) => normal()
	     | (opr as VARexp(ref(VALvar{access=INLINE prim,...}))) =>
		 let val (d',b' as (btexp',bev',bsites',bvars'),arg') = instr arg
		 in if (hasfnsite bsites') andalso (provocative prim) then
		      let val (argtmpb, argtmpa) = makevar ("argvar", UNDEFty)
		          val evn = makeevent(APPev exp::bsites',bev')
		      in (true, 
			  (ntexp,evn,nil,nil),
			  LETexp (VALdec[VB{pat=VARpat(argtmpb),
					    exp=arg',
					    tyvars=nil}],
				  SEQexp[EVENTexp(evn,btexp',bvars'),
					 APPexp(opr,
					        VARexp(ref argtmpa))]))
		      end
		    else 
		      (d',b',APPexp(opr,arg'))
                 end
	     | CONexp(DATACON{rep=REF,...}) => normal()
	     | CONexp _ =>
		 let val (d',b',arg') = instr arg
	 	 in  (d',b',APPexp(opr, arg'))
	  	 end
             | FNexp body =>  (* really a CASE or equivalent *)
		 let val (_,b',arg') = instr arg
		     val body' = instrrules (b',fn r => CASEev(arg,r)) body
		 in (true,b',APPexp(FNexp body',arg'))
		 end
	     | _ => normal()
	  end
      | instr (CONSTRAINTexp (e,c)) =
	  let val (d',b',e') = instr e
	  in (d',b',CONSTRAINTexp (e',c))
	  end
      | instr (exp as MARKexp(RAISEexp arg,_,_)) = 
		(* N.B. Must be marked *)
	  let val (_,b' as (btexp',bev',bsites',bvars'),arg') = instr arg
	      val (argtmpb,argtmpa) = makevar ("argvar",UNDEFty)
	      val evn = makeevent(RAISEev exp::bsites',bev')
              fun newb (b as (_,_,nil,_),_) = b
                | newb (_,evn) = (ntexp,evn,nil,nil)
	  in (true,
	      newb(b',evn),
	      LETexp (VALdec [VB{pat = VARpat(argtmpb),
				 exp = arg',
				 tyvars = nil}],
		      SEQexp [EVENTexp(evn,btexp',bvars'),
			      RAISEexp(VARexp(ref argtmpa))]))
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
	      val rl' = instrrules (b',fn r => CASEev(exp,r)) rl
	  in (true,b',CASEexp(exp', rl'))
	  end
      | instr (HANDLEexp (e, HANDLER(FNexp body))) =
	  if hasfnsite bsites then
	    let val evn = makeevent(bsites,bev)
     	        val b' = (ntexp,evn,nil,nil)
     	        val (_,_,e') = instrexp(INTexp 0,b',e)
     	        val body' = instrrules(b',HANDLEev) body
     	    in (true,b',SEQexp[EVENTexp(evn,btexp,bvars),
     		    	       HANDLEexp(e',HANDLER(FNexp body'))])
     	    end
          else 
     	    let val (d',b',e') = instr e
     	        val body' = instrrules(b,HANDLEev) body
     	    in (d',b', HANDLEexp(e',HANDLER(FNexp body')))
            end
      | instr (FNexp body) =
          if hasfnsite bsites then   (* N.B. This must never occur as RHS of a val rec binding!! *)
	    let val evn = makeevent(bsites,bev)
		val b' = (ntexp,evn,nil,nil)
	        val body' = instrrules(b',FNev) body
	    in (true,b',SEQexp[EVENTexp(evn,btexp,bvars),
			       FNexp body'])
	    end
	  else
	    let val body' = instrrules (b,FNev) body
	    in (false,b,FNexp body')
	    end
      | instr (MARKexp (exp,s,e)) =
	  let val (d',b',exp') = instr exp
	  in (d',b', MARKexp(exp',s,e))
          end
      | instr exp = (false,b,exp)
    and instrrules (b as (btexp,bev,bsites,bvars),evf) =
      let 
        fun f (rule as RULE(pat,exp as MARKexp(_))) = 
	     let val vars = (patvars toaccess pat)
		 val (ntdec,ntexp) = makent "NEXTTIME"
  		 val (d',b' as (_,_,bsites',bvars'),exp') = 
			 instrexp (ntexp,
			           (btexp,bev,(evf rule)::bsites,vars@bvars),
				    exp)
	     in if d' then
		  RULE (pat,LETexp (ntdec,exp'))
		else (* insert backstop event for rule body *)
		  let val evn = makeevent(bsites', bev)
		  in RULE (pat,LETexp(VALdec[VB{pat=WILDpat,
						exp=EVENTexp(evn,btexp,bvars'),
						tyvars=nil}],
				      LETexp (ntdec,
					      exp')))
		  end
	     end
          | f (RULE(pat,exp)) =
	     let val (_,_,exp') = instrexp(ntexp,b,exp)
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
            let val ((btexp',bev',bsites',bvars'),evndecopt) = 
		      if hasfnsite bsites then
		        let val (b',evndec) = simplebind(b,nil,nil)
		        in (b',SOME evndec)
		        end
    		      else (b,NONE)
	        val vars =(map (fn RVB{var=VALvar{access=LVAR(var),name,typ},...} => 
				VARexp(ref (VALvar{access=PATH[var],name=name,typ=typ}))) rvbl)
		val b' = (btexp',bev',VALRECev dec::bsites',vars @ bvars')
		val rvbl' = map (fn (RVB{var,exp,resultty,tyvars}) =>
				     let val (_,_,exp') = instrexp(INTexp 0,b',exp)
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
                let val (b',entdec) = simplebind(b,[FCTENTev fctb],nil)
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
       	       simplebind(b',[STRENDev mstrexp],nil)
          in (bev',STRUCTstr{body=body'@[enddec],locations=locations,
       			str=str})
          end
      | APPstr{oper,argexp,argthin,str} => 
          let val (_,argexp') = instrstrexp (b,argexp)
              val ((_,bev',_,_),appdec) = 
       		simplebind(b,[FCTAPPev mstrexp],nil)
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

val (lastbindev',absyn') =
  let val startevn = makeevent([STARTev absyn],lastbindev)
      val (bevb,beva) = makevar ("BIND" ^ makestring startevn,intTy)
      val STARTexp = EVENTexp(startevn,INTexp(lastTime lastbindev),nil)
      val STARTdec = VALdec[VB{pat=VARpat(bevb),exp=STARTexp,tyvars=nil}]
      val ((btexp,bev,bsites,bvars),absyn') = 
 	     instrdec((VARexp (ref beva),startevn,nil,nil),absyn)
      val endevn = makeevent(ENDev absyn::bsites,bev)
      val ENDexp = FEVENTexp(endevn,btexp,bvars)
      val ENDdec = VALdec[VB{pat=WILDpat,exp=ENDexp,tyvars=nil}]
  in (endevn,
      SEQdec [STARTdec,
	      absyn',
	      ENDdec])
  end


val evcount' = !evNum - firstev'
val events' = arrayoflist (rev (!evList))
val elb' = arrayoflist (rev (!elbList))

in
   nextCud := 
     {file = file',
      visible = ref true,
      firstev = firstev',
      evcount = evcount',
      lastbindev = lastbindev',
      events = events',
      elb = elb',
      eventtimes = array(evcount',0)};
   nextLocIndex := augmentLocIndex (!currentLocIndex,!nextCud);
   addCud (!nextCud);
   if (!debugdebug) then
     (print "Entering cud ";print firstev'; print " "; 
      print evcount'; print "\n";
      dumpEvents (firstev',events',elb'))
   else ();
   LOCALdec(VALdec [VB {pat = TUPLEpat [VARpat timesvarb,
					VARpat eventtimesb,
					VARpat breakentryb,
					VARpat hcreaterb,
					VARpat weakb,
					VARpat udlb,
					VARpat arrayb],
                        exp = APPexp(VARexp(CoreInfo.getDebugVar),
			             INTexp (firstev')),
			tyvars = nil}],
            absyn')
end (* end fun instrumDec *)

fun commit () = (currentCud := !nextCud;
		 currentLocIndex := !nextLocIndex)

fun reset () = (currentCud := initialCud;
	        nextCud := initialCud;
		currentLocIndex := emptyLocIndex;
                nextLocIndex := emptyLocIndex)

end (* struct DbgInstr *)

