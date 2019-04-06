structure DbgCom: 
sig val Xcomplete:unit->unit val Xabort:unit->unit end =
struct

open DbgUtil DbgKern DbgStat DbgRun DbgInstr 
	Access Absyn Basics PrtAbsyn PrtBasic PrtUtil CalcType

local 
type when = int 
type where = int * int
type wherewhen = where * when
datatype break = TIME of when |
	         EVENT of where
type value = System.Unsafe.object
type valtype = ty
datatype valinfo = UNBOUND |
     	           BOUND of value * valtype * wherewhen

datatype 'a outcome = COMPLETED of 'a | INTERRUPTED of 'a | NOTRUNNING

fun checkint x = if !interruptRaised then 
		   INTERRUPTED x
	         else COMPLETED x

fun protect f =
  let val originalTime = currentTime()
  in setinthand();
     f() before (restoreTime (originalTime,0); (* ignoring interrupts! *)
		 resetinthand())
  end


fun evdataAt (time:int) = restoreTime (time,0)

val eventsAt = eventsFor o #evn o evdataAt

fun currentWhereWhen () = 
      let val t = currentTime()
	  val {evn,...} = restoreTime (t,0)
      in ((evn,0),t)
      end

val breakList = ref (nil: (int*break) list)
val nextBreak = ref 1

val pdepth = 100	(* print depth *)

val initDelta = ref 1000	(* default amount to progress initially *)

(* type chasing *)
fun chaseType (t,c) (exp:Absyn.exp) = 
  let val _ = if (!debugdebug) 
	      then (print "fetching type for "; 
			printExp (exp, 0, 1000); print "\n")
	      else ();
      fun encfunct t () = 
	    let fun ef 0 = NONE
		  | ef t =
		      let val {evn,lbt,...} = evdataAt t
		      in case eventsFor evn of
		           (FCTENTev(_)::_) =>
				(case eventsAt (t-1) of
				   ((FCTAPPev(MARKstr(s as APPstr(_),_,_)))::_)
				     => SOME s
				 | _ => debugPanic "missing FCTAPPev in encfunct")
			 | _ => ef lbt
		      end
	    in ef t
	    end
      fun enc (t,c) () =
	    let fun ef (0,_)  = NOTENCLOSED
		  | ef (t,c) =
		      let val {evn,lbt,...} = evdataAt t
		          fun f (nil) = ef (lbt,0)
			    | f (FNev(RULE(pat,_))::_) = 
				      ENCLOSED (pat,enc (lbt,0),argf (t-1))
			    | f (HANDLEev(RULE(pat,_))::_) =
				      ENCLOSED (pat,enc (lbt,0),argh (t-1))
			    | f (_::rest) = f rest
		      in f (tln(eventsFor evn,c))
		      end
	    in ef (t,c)
	    end
      and argf t () = 
	    case hd(eventsAt t) of
	      APPev(APPexp(_,exp)) => ARGS(exp,enc (t,0),encfunct t)
            | _ => NOTAVAIL   (* caller not compiled debug? *)
      and argh t () =
	    case hd (eventsAt t) of
	      RAISEev(MARKexp(RAISEexp exp,_,_)) => 
					ARGS(exp,enc (t,0),encfunct t)
	    | _ => NOTAVAIL (* implicit exception? *)
      val typ = getType(exp, enc (t,c), encfunct t)
   in if (!debugdebug) 
      then (print "type is "; PrtType.printType typ; print "\n")
      else ();
      typ
  end


fun nametype (t,c) = fn (v as VALvar{name=[nm],...}) =>
			(Symbol.name nm, 
			 fn () => chaseType (t,c) (VARexp(ref v)))
		  | _ => debugPanic "bad var in debugcommands.nametype"

in

(* events *)
fun XeventsAfterLocation(loc:location) : where list = 
	map (fn evnum => (evnum,0)) (eventsAfter loc)

fun XeventsBeforeLocation(loc:location) : where list = 
	map (fn evnum => (evnum,0)) (eventsBefore loc)

(* breakpoints and motion *)

fun XinsertBreak (b:break): int  =
  (breakList := ((!nextBreak,b)::(!breakList));
   !nextBreak before (inc nextBreak))

fun XdeleteBreak (bn:int) : bool  = 
  let 
    exception NoSuchBreak
    fun del ((n,b)::r) = if n = bn then r else ((n,b)::(del r))
      | del (nil) = raise NoSuchBreak
  in
    (breakList := del(!breakList);
     true) 
     handle NoSuchBreak => false
  end

fun XclearBreaks () : unit =
  breakList := nil

fun XgetBreaks () : (int*break) list = !breakList

fun Xcurrent () : wherewhen outcome =
  if !running then
    COMPLETED (currentWhereWhen())
  else NOTRUNNING


fun Xforwards () : wherewhen outcome =
  if !running then
    let val minbtime = fold (fn ((_,TIME t),m) => if t > (currentTime())
					      then min(t,m)
					      else m
			      | (_,m) => m) 
			     (!breakList) (!finalTime)
	fun getetimes () = fold (fn ((_,EVENT (evnum,_)),etl) => 
					lastTime evnum :: etl
				  | (_,etl) => etl) (!breakList) []
	val etimes = getetimes()
	val inittime = currentTime()
	fun minchanged () = 
	  fold (fn ((old,new),m) =>
		     if (new > old) andalso (new > inittime) andalso 
			(new < m) then new else m)
	       (pairlist etimes (getetimes())) DbgUtil.infinity
	fun narrow (ftime,ltime) =
	 (if (!debugdebug) then (print "narrowing "; print ftime; print " ";
				 print ltime; print "\n") else ();
	  if (ftime < ltime) then
	     (let val target = (ftime + ltime) div 2
	      in restoreTime (target, (target-ftime) div 10);
		 if !interruptRaised then
		   ftime
		 else
		   let val minc = minchanged () 
		   in if minc < DbgUtil.infinity then
		        narrow (ftime,minc)
		      else narrow (target+1,ltime)
		   end
	      end)
	  else ltime)
  
	fun breached(evn) = 
	  let fun h((_,EVENT (evn',_))::bl) = evn' = evn orelse h(bl)
		| h(_::bl) = h(bl)
		| h(nil) = false
	  in h (!breakList)
	  end
	fun f(defDelta) = 
	  let val startTime  = currentTime()
	      val defTarget = currentTime() + defDelta
	      val defError = defDelta div 10
	      val (target,error) = 
		    if (length etimes > 0) andalso 
			  (defTarget + defError < minbtime) then
		      (defTarget, defError)
		    else (minbtime,0)
	      fun iobreak () = minchanged() < DbgUtil.infinity
	      val {evn,...} = advanceTime(target,error,true,iobreak)
	      val minc = minchanged()
	  in if !interruptRaised then
		INTERRUPTED (currentWhereWhen())
	     else if (minc < DbgUtil.infinity) then
		let val {evn,...} = restoreTime(narrow(startTime+1,minc),0)
		in checkint (currentWhereWhen())
		end
	     else (if (!debugdebug) then 
	  	     (print "forwards: "; print (currentTime()); print " ";
				       print (breached evn); print " ";
				       print (!finalTime); print " ";
				       print minbtime; print "\n")
	           else ();
		   if (not (breached(evn)))
		   andalso (not (currentTime()=minbtime))
		   andalso (currentTime() < !finalTime) then
			  f((currentTime() - startTime) * 2 + 1)
		   else
			  checkint (currentWhereWhen()))
	  end
    in
      setinthand();
      f(!initDelta) before (resetinthand())
    end
  else NOTRUNNING

  
fun Xcomplete() : unit = 
	(* run remainder of compilation unit with no breakpoints 
		nor attention to io, exceptions, interrupts, end event *)
  if !running then
    (setinthand();
     advanceTime (DbgUtil.infinity, 0, false, fn () => false);())
  else ()

    
fun Xabort() : unit = 
  if !running then
       (setinthand();
 	abort())
  else ()

fun Xbackwards() : wherewhen outcome =
  if !running then
    let val target = 
         fold (fn ((_,TIME t), m) => 
			if (t < currentTime()) then max(t,m) else m
		| ((_,EVENT (evn,_)), m) => max (lastTime evn,m))
	      (!breakList) (!initialTime)
        val _ = setinthand()
        val {evn,...} = restoreTime (target,0)
    in
      checkint (currentWhereWhen()) before (resetinthand())
    end
  else NOTRUNNING

fun Xexception () : exn option =
  if currentTime() = !finalTime then !lastException else NONE

(* call tracing *)

fun XcallTrace (maxdepth:int) : (((wherewhen*wherewhen*(((string*valtype)*value) list)) list) outcome) = 
  protect (fn () =>
    if !running then
      let fun dotype (name,typef) = (name,typef())
	  fun lastfunc(0,{evn,...}) = (0,evn,0,nil)
	    | lastfunc(t,{evn,lbt,args}) =
		 let fun f (FNev (RULE(pat,_))::_,n,args) = 
			    (t,evn,n,pairlist (patvars (dotype o (nametype (t,n))) pat) args)
		       | f (HANDLEev (RULE(pat,_))::_,n,args) =
			    (t,evn,n,pairlist (patvars (dotype o (nametype (t,n))) pat) args)
		       | f (evt::rest,n,args) = f(rest,n+1,tln (args,argCnt evt))

		       | f (nil,_,_) = lastfunc(lbt,evdataAt lbt)
		 in f (eventsFor evn,0,args)
		 end
	  fun up 0 _ = nil
	    | up _ 0 = nil
	    | up t d =
	       if !interruptRaised then 
		 nil
	       else
	         let val evdata as {evn,...} = evdataAt (t-1)
		     val (t',evn',n,varlist) = lastfunc(t-1,evdata)
	         in
		   (((evn,0),t-1),((evn',n),t'),varlist) :: (up t' (d-1))
	         end
	  val cww = currentWhereWhen()
	  val (t',evn',n,varlist) = lastfunc (currentTime(),evdataAt(currentTime()))
	  val calls = (cww, ((evn',n),t'),varlist) :: (up t' maxdepth)
      in
	checkint calls
      end
    else NOTRUNNING)

(* value fetching printing *)

fun XisFn (vt:valtype) : bool = BasicTyp.isArrowType vt

fun XprintVal(boundval:value,vtype:valtype) : unit =
	 PrintVal.printVal(boundval,vtype,pdepth)
	

fun XeventDesc ((evn:evnum,c:evindex),t:int) : 
				(string * location * visible) option =
  protect (fn () =>
    let val evt = nth (eventsFor evn,c) 
        val desc = eventText evt 
	val ((filename,visible),charno) =
	  case evt of
	    IOev => let val evn = (#evn o evdataAt) (t-1)
		    in (filenameFor evn, locOfEvent (hd(eventsFor evn)))
		    end
	  | _ => (filenameFor evn, locOfEvent evt)
    in case evt of 
	 NULLev => NONE
       | _ => SOME (desc,(filename,charno),visible)
    end handle Nth => NONE 
	     | Evnum _ => NONE)

(*fun XprintAbsyn((evn:int,c:int),indent:int) : unit = 
  (case nth(eventFor evn,c) of
     VALev(MARKdec(dec,_,_)) => printDec(dec,indent,pdepth)
   | VALRECev(MARKdec(dec,_,_)) => printDec(dec,indent,pdepth)
   | FNev(RULE(pat,_)) => (print "fn "; printPat(pat,pdepth); print " =>")
   | HANDLEev(RULE(pat,_)) => (print "handle "; printPat(pat,pdepth); 
				print " =>")
   | CASEev(exp,RULE(pat,_)) => (print "case "; printExp(exp, indent, pdepth);
			print " of "; printPat(pat,pdepth); print (" =>"))
   | APPev(exp) => printExp(exp,indent,pdepth)
   | RAISEev(exp) => printExp(exp,indent,pdepth)
   | STRev(MARKdec(dec,_,_),_) => printDec(dec,indent,pdepth)
   | ABSev(MARKdec(dec,_,_),_) => printDec(dec,indent,pdepth)
   | FCTev(MARKdec(dec,_,_),_) => printDec(dec,indent,pdepth)
   | FCTENTev(FCTB{fctvar=FCTvar{access,name=fname,...},
	param=STRvar{name=pname,...},def,...}) =>
	 (print "functor "; printSym fname; printAccess access; print " ("; 
		print(formatQid pname); print ") = "; nlindent(indent+4);
		printStrexp(def,indent+4,pdepth))
   | FCTAPPev(MARKstr(strexp,_,_)) => printStrexp(strexp,indent,pdepth)
   | STRENDev(MARKstr(strexp,_,_)) => printStrexp(strexp,indent,pdepth)
   | STRVARev(MARKstr(strexp,_,_)) => printStrexp(strexp,indent,pdepth)
   | OPENev(MARKdec(dec,_,_)) => printDec(dec,indent,pdepth)
   | LETev(MARKexp(exp,_,_)) => printExp (exp,indent,pdepth)
   | LOCALev(MARKdec(dec,_,_)) => printDec (dec,indent,pdepth)
   | LOCINev(MARKdec(dec,_,_)) => printDec (dec,indent,pdepth)
   | LOCENDev(MARKdec(dec,_,_)) => printDec (dec,indent,pdepth)
   | IOev => print "IO event"
   | NULLev _  => print "Null event"
   | _ => debugPanic "bad event type in debugcommands.XprintAbsyn"
   ) handle NoSuchEvent => print "No Such Event"
*)

fun XprintBind(((evn:int,c:int),t:int),indent:int) : unit =
  protect (fn () =>
   case nth(eventsFor evn,c) of
     VALev(MARKdec(dec as VALdec(_),_,_)) => printDec(dec,indent,pdepth)
   | VALRECev(MARKdec(dec as VALRECdec(_),_,_)) => printDec(dec,indent,pdepth)
   | FNev(RULE(pat,_)) => 
	(printPat(pat,pdepth); print " <=== ";
	 case hd(eventsAt (t-1)) of
	   APPev(APPexp(_,exp)) => printExp(exp,indent+8,pdepth)
	 | _ => print "unknown call site")
   | HANDLEev(RULE(pat,_)) =>
	(printPat(pat,pdepth); print " <=== ";
	 case hd(eventsAt (t-1)) of
           RAISEev(MARKexp(RAISEexp exp,_,_)) => printExp(exp,indent+8,pdepth)
         | _ => print "implicit exception")
   | CASEev(exp,RULE(pat,_)) => 
	 ( printPat(pat,pdepth); print " <=== "; printExp(exp,indent+8,pdepth))
   | _ => debugPanic "bad event type in debugcommands.xprintBind")


local
exception BindNotFound

fun nthArgs (evn,n,args) =
  let fun f (_,0,args) = args
        | f (evt::rest,n,args) = f(rest,n-1,tln (args,argCnt evt))
  in f (eventsFor evn,n,args)
  end

fun checkInStruct lookf (n:string, s:Basics.Structure) =
     let val res = 
       case s of 	
         STRstr {table,...} => ((((lookf (table,Basics.Symbol.symbol n); true)
				   handle Unbound => false)
				   handle Unboundrec => false)
				   handle UnboundTable => false)
       | _ => debugPanic "bad Structure in debugcommands.checkInStruct"
     in if (!debugdebug) 
	then (print "checkinstruct "; print n; 
	      print " "; print (makestring res); print "\n")
	else ();
	res
     end

val checkValInStruct = checkInStruct EnvAcc.lookVARinTable
val checkStructInStruct = checkInStruct EnvAcc.lookSTRinTable

fun getstructnbd (t,c) : ((Symbol.symbol * Basics.Structure * Absyn.strexp) list) =
     case nth(eventsAt t,c) of
       STRev (MARKdec(STRdec strbl,_,_),_) => 
		map (fn STRB{strvar=STRvar{name=[nm],binding,...},def,...} => 
				(nm,binding,def)) strbl
     | ABSev (MARKdec(ABSdec strbl,_,_),_) =>
		map (fn STRB{strvar=STRvar{name=[nm],binding,...},def,...} => 
				(nm,binding,def)) strbl
     | FCTev (MARKdec(FCTdec fctbl,_,_),_) => 
		map (fn FCTB{fctvar=FCTvar{name,binding=FUNCTOR{body,...},...},def,...} =>
				(name,body,def)) fctbl
     | _ => []

fun getopennb (t,c): ((string list * Basics.Structure) list) =
     case nth(eventsAt t,c) of
       OPENev (MARKdec(OPENdec svl,_,_)) =>
		map (fn STRvar{name,binding,...} => 
			(rev (map Symbol.name name),binding)) (rev svl)
     | _ => []

fun getfctarg (t,c) =
      case nth(eventsAt t,c) of
        FCTENTev (FCTB{param=STRvar{name=[nm],binding,...},...}) => 
	    SOME (nm,binding)
      | _ => NONE

fun fetchStructure (t,c,i) : Basics.Structure = 
     let val (_,binding,_) = nth (getstructnbd (t,c),i)
     in binding
     end handle Nth => let val SOME (_,binding) = getfctarg(t,c)
		       in binding
		       end

fun findStructAtBind (n,t,c) : int =
  (index (fn (name,_,_) => Symbol.name name = n) (getstructnbd (t,c)))
	handle Index => case getfctarg(t,c) of
			  SOME(name,_) => if Symbol.name name = n then 0
					  else raise BindNotFound
			| _ => raise BindNotFound

fun getVALvars (t,c) = 
      case nth(eventsAt t,c) of
	VALev(MARKdec(VALdec(vbl),_,_)) => vblextract (fn x => x) vbl
      | VALRECev(MARKdec(VALRECdec(rvbl),_,_)) =>
				map (fn RVB{var,...} => var) rvbl
      | FNev(RULE(pat,_)) => patvars (fn x => x) pat
      | HANDLEev(RULE(pat,_)) => patvars (fn x => x) pat
      | CASEev(_,RULE(pat,_)) => patvars (fn x => x) pat
      | _ => []

fun fetchvar (t,c,i) : Basics.var = nth (getVALvars (t,c),i)

fun findValAtBind (n,t,c) : int =
  (index (fn VALvar{name=[nm],...} => Symbol.name nm = n) (getVALvars (t,c)))
		handle Index => raise BindNotFound

fun checkvis(t,c,vc as(ec,ic)) =
  case nth(eventsAt t,c) of
    LOCALev(_) => if ic > 0 then
		    (ec,ic-1)
		  else if ec > 0 then
		    (ec-1,ic)
                  else vc
  | LOCINev(_) => if ic = 0 andalso ec > 0 then
		    (ec-1,ic+1)
		  else vc
  | LOCENDev(_) => if ic > 0 then
		     (ec,ic+1)
		   else (ec+1,ic)
  | _ => vc

fun findBind findAtBind	checkInStruct =
   let fun lastbind t = (#lbt o evdataAt) t
       fun count t = (length o eventsAt) t
       fun find ([n],t,c) =
             let fun loop (0,_,_) = raise BindNotFound
                   | loop (t,c,vc as (_,ic)) = 
				 if c < count t then
				   if ic = 0 then
                                     (t, c, findAtBind (n,t,c))
		  		       handle BindNotFound => 
				        (findInOpen (n,t,c) 
				         handle BindNotFound =>
				          (findInAnonFctarg (n,t,c) 
					   handle BindNotFound =>
				            loop (t,c+1,checkvis(t,c,vc))))
				    else loop (t,c+1,checkvis(t,c,vc))
		                  else loop(lastbind t,0,vc)
	     in loop (t,c,(0,0))
	     end
         | find (n::r,t,c) =
	     let val stx = findStructBind (r,t,c)
	     in if checkInStruct (n, fetchStructure stx)
	        then find ([n], cleanStruct stx,0)
	        else raise BindNotFound
	     end
	 | find ([],_,_) = raise BindNotFound

       and findInOpen (n,t,c) =
	     let fun f ((sn,st)::r) = if checkInStruct (n,st)
				      then let val stx = findStructBind(sn,t,c)
				           in find ([n], cleanStruct stx,0)
				           end
				      else f r
	           | f nil = raise BindNotFound
 	     in f (getopennb (t,c))
	     end

       and findInAnonFctarg (n,t,c) = 
	     case (getfctarg (t,c)) of 
	       SOME (name,st) => 
		 if name = Misc.anonParamName andalso checkInStruct (n, st) 
  	         then find ([n], t-1,0)
	         else raise BindNotFound
	     | _ => raise BindNotFound
   in
     find
   end


and findValBind (nl,t,c) = findBind findValAtBind checkValInStruct (nl,t,c)
and findStructBind (nl,t,c) = findBind findStructAtBind checkStructInStruct (nl,t,c)

and cleanStruct (t,c,i) : int =
  let
    val _ = if (!debugdebug) then
                    (print "cleaning "; print (t:int); print " "; 
		     print (c:int); print " "; print (i:int); print "\n")
            else ()
    val t' = case getstructnbd(t,c) of
               nil => t-2  (* FCTENTev: jump into parameter *)
             | _ => let val {evn,args,...} = evdataAt t
  		        val timearr:int array = 
				 System.Unsafe.cast (hd (nthArgs(evn,c,args)))
		    in (case nth(eventsFor evn,c) of
			  STRev _ => timearr sub i
			| ABSev _ => timearr sub i
			| FCTev _ => 0 (* can't enter a functor! *)
			| _ => debugPanic "bad ev in cleanStruct")
			  handle Nth => debugPanic "bad evn c in cleanStruct"
		    end
    val t' = case hd (eventsAt t') of
       	       STRVARev (MARKstr(VARstr(STRvar{name,...}),_,_)) =>
	     	 cleanStruct (findStructBind (rev (map Symbol.name name),t',0))
	     | STRVARev _ => debugPanic "bad STRVARev in cleanStruct"
	     | STRENDev _ => t'
             | _ => debugPanic "bad event in cleanStruct"
  in if (!debugdebug) then
	   (print "cleaned to "; print (t':int); print "\n")
     else ();
     t'     
  end

fun split s =
  let fun sp s =
    let val pos = index (fn c => c = ".") (explode s)
    in substring (s,0,pos) :: 
		sp (substring(s,pos+1, String.length s - (pos+1)))
    end handle Index => [s]
  in rev (sp s)
  end

in
fun XgetVal (n:string) : valinfo  =
  protect (fn () =>
      let val (t,c,i) = findValBind (split n, currentTime(),0) 
	  val v = fetchvar (t,c,i)	
	  val {evn,args,...} = evdataAt t
	  val bv = nth (nthArgs(evn,c,args),i)
	  val ty = chaseType (t,c) (VARexp(ref v))
	  val ww = ((evn,c),t)
      in BOUND (bv,ty,ww)
      end handle BindNotFound => UNBOUND)
end

fun Xprovoke (exnopt:exn option) : unit =
  addAction (fn () => provocation :=exnopt)

fun Xreset():unit = 
  (HistIO.reset();
   HistStor.reset();
   DbgStat.reset();   
   DbgRun.reset();
   DbgInstr.reset();
   breakList := nil;
   nextBreak := 1;
   debugdebug := false)

fun Xindebug() : bool = !running

val _ = System.Control.Debug.getDebugf := 
	 (System.Unsafe.cast)  (fn firstevn =>
		(times,eventtimeArray firstevn,!break,
			HistStor.hcreater,Weak.weak,
			HistStor.updatedRList,Array.array))

val _ =
  let val old_interface = !System.Control.Debug.interface
  in System.Control.Debug.interface := 
   (fn 
     (* 1 and 2 reserved for use -- see interact.sml *)
       3 => System.Unsafe.cast XclearBreaks
     | 4 => System.Unsafe.cast XinsertBreak
     | 5 => System.Unsafe.cast XdeleteBreak
     | 6 => System.Unsafe.cast XgetBreaks
     | 7 => System.Unsafe.cast Xcurrent
     | 8 => System.Unsafe.cast Xforwards
     | 9 => System.Unsafe.cast Xbackwards
     | 10 => System.Unsafe.cast XcallTrace
     | 11 => System.Unsafe.cast XgetVal
     | 12 => System.Unsafe.cast XprintVal
     | 13 => System.Unsafe.cast XisFn
     | 14 => System.Unsafe.cast XprintBind 
     | 15 => System.Unsafe.cast (DbgUtil.debugdebug,DbgUtil.sizing)
     | 16 => System.Unsafe.cast (HistStor.updatedAList,HistStor.updatedRList,HistStor.createdList,HistStor.hcreatea,HistStor.hcreater)
     | 17 => System.Unsafe.cast XeventsAfterLocation
     | 18 => System.Unsafe.cast XeventsBeforeLocation
     | 19 => System.Unsafe.cast Xreset
     | 20 => System.Unsafe.cast Xcomplete
     | 21 => System.Unsafe.cast Xabort
     | 22 => System.Unsafe.cast Xindebug
     | 23 => System.Unsafe.cast Xexception
     | 24 => System.Unsafe.cast HistIO.logit
     | 25 => System.Unsafe.cast XeventDesc
     | 26 => System.Unsafe.cast (DbgRun.minTimeDelta,DbgRun.maxTimeDelta,
					DbgRun.maxstates)
     | 27 => System.Unsafe.cast times
     | 28 => System.Unsafe.cast (HistStor.activate,HistIO.activate)
     | 29 => System.Unsafe.cast Xprovoke
     | q => old_interface q)
  end (* let ... *)

end (* local *)
end (* DbgCom *)  

