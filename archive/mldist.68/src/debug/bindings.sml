signature DEBUGBINDINGS =
sig
  val blookup: int -> System.Unsafe.object option
  val bclear: unit -> unit
  val looker: Symbol.symbol -> Basics.binding
  val getVal: string -> System.Unsafe.object * Basics.ty * ((int*int)*int)
end

structure DebugBindings : DEBUGBINDINGS =
struct
  exception RunDebugBind
  val btable = (ref (Intmap.new(32,RunDebugBind))):System.Unsafe.object Intmap.intmap ref

  fun bclear () = btable := Intmap.new(32,RunDebugBind)
  val bind = Intmap.add (!btable)
  val lookup = Intmap.map (!btable)
  fun blookup lv = (SOME (lookup lv)) handle RunDebugBind => NONE

open DebugUtil DebugKernel DebugStatic DebugRun DebugInstrum 
         Access Absyn Basics PrintAbsyn PrintBasics PrintUtil CalcType

fun dbgprint (s:string) = if (!debugdebug) then print s else ()
    
fun protect f =
  let val originalTime = currentTime()
      fun reset () = (restoreTime (originalTime,0); (* ignoring interrupts !*)
		      resetinthand())
  in setinthand();
     (f() before reset()) 
	handle Unbound => (reset(); raise Unbound)
  end

fun evdataAt (time:int) = restoreTime (time,0)

val eventsAt = eventsFor o #evn o evdataAt

(* type chasing *)
fun encfunct t () = 
  let fun ef 0 = NONE
        | ef t =
	      let val {evn,lbt,...} = evdataAt t
	      in case eventsFor evn of
	           (FCTENTev(_)::_) =>
			(case eventsAt (t-1) of
			   ((FCTAPPev(MARKstr(s as APPstr(_),_,_)))::_)
			     => SOME (s,t)
			 | _ => debugPanic "DebugBindings.encfunct missing FCTAPPev")
		 | _ => ef lbt
	      end
  in ef t
  end

fun chaseType (t,c) (exp:exp) = 
  let val _ = if (!debugdebug) 
	      then (print "fetching type for "; 
			printExp (!debugEnv) (exp, 0, 1000); print "\n")
	      else ();
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
      then (print "type is "; PrintType.printType std_out (!debugEnv) typ; print "\n")
      else ();
      typ
  end

fun chaseTycon t = deabstyc (encfunct t)

fun chaseStr t = deabsstr (encfunct t)

exception Unbound = Env.Unbound

(* find a binding within a structure *)
fun findInStruct symbol (n:string, STRstr{table,...}:Structure) =
                                               Env.look table (symbol n)
  | findInStruct _ _ = debugPanic "DebugBindings.checkInStruct bad Structure"

val findVARCONInStruct = findInStruct Symbol.varSymbol
val findSTRInStruct = findInStruct Symbol.strSymbol
val findTYCInStruct = findInStruct Symbol.tycSymbol

(* getting all bindings of a given sort at a particular event *)
fun getSTRs ev: (structureVar list) =
     case ev of
       STRev (MARKdec(STRdec strbl,_,_),_) => 
	        map (fn STRB{strvar,...} => strvar) strbl
     | ABSev (MARKdec(ABSdec strbl,_,_),_) => 
	        map (fn STRB{strvar,...} => strvar) strbl
     | _ => []

fun getFCTs ev : (functorVar list) =
     case ev of
       FCTev (MARKdec(FCTdec fctbl,_,_),_) => 
		map (fn FCTB{fctvar,...} => fctvar) fctbl
     | _ => []

fun getSIGs ev : (signatureVar list) =
     case ev of
       SIGev (MARKdec(SIGdec svl,_,_)) => svl
     | _ => []

fun getFIXs ev : (fixityVar list) = 
     case ev of
       FIXev (MARKdec(FIXdec {fixity,ops},_,_)) =>
	      map (fn name => FIXvar{name=name,binding=fixity}) ops
     | _ => []


fun getTYCs ev : tycon list =
      case ev of
	TYPEev(MARKdec(TYPEdec tbl,_,_)) => map (fn TB{tyc,...} => tyc) tbl
      | TYPEev(MARKdec(DATATYPEdec {datatycs=tycl,withtycs=tbl},_,_)) => 
				tycl @ (map (fn TB{tyc,...} => tyc) tbl)
      | TYPEev(MARKdec(ABSTYPEdec {abstycs=tycl,withtycs=tbl,...},_,_)) =>
				tycl @ (map (fn TB{tyc,...} => tyc) tbl)
      | _ => []

fun getVARs ev : var list = 
      case ev of
	VALev(MARKdec(VALdec(vbl),_,_)) => vblextract (fn x => x) vbl
      | VALRECev(MARKdec(VALRECdec(rvbl),_,_)) =>
				map (fn RVB{var,...} => var) rvbl
      | FNev(RULE(pat,_)) => patvars (fn x => x) pat
      | HANDLEev(RULE(pat,_)) => patvars (fn x => x) pat
      | CASEev(_,RULE(pat,_)) => patvars (fn x => x) pat
      | OVLDev(MARKdec(OVLDdec ovldvar,_,_)) => [ovldvar]
      | _ => []

fun getCONs ev : datacon list =
     let fun extract (GENtyc{kind=ref (DATAtyc dcl),...},acc) = dcl @ acc
	   | extract (GENtyc{kind=ref (ABStyc tyc),...},acc) = extract (tyc,acc)
           | extract (_,acc) = acc
	 fun geteb (EBgen {exn,...},acc) = exn::acc
	   | geteb (EBdef {exn,...},acc) = exn::acc
     in
      case ev of
	TYPEev(MARKdec(DATATYPEdec{datatycs=tycl,...},_,_)) => 
				fold extract tycl nil
      | TYPEev(MARKdec(ABSTYPEdec{abstycs=tycl,...},_,_)) => 
				fold extract tycl nil
      | EXCEPTIONev(MARKdec(EXCEPTIONdec ebl,_,_)) => fold geteb ebl nil
      | _ => []
     end

(* finding a named binding of a given sort at a particular (time,sub-event) *)
fun findAtBind getter namer binder (n,t,c) : (int * binding) =
  let fun find(i,v::r) = if Symbol.name (namer v) = n then (i,binder v)
                         else find(i+1,r)
        | find(_,nil) = raise Unbound
  in find (0,getter(nth(eventsAt t,c)))
  end

val findFIXAtBind = findAtBind getFIXs (fn FIXvar{name,...} => name) FIXbind
val findSIGAtBind = findAtBind getSIGs (fn SIGvar{name,...} => name) SIGbind
val findFCTAtBind = findAtBind getFCTs (fn FCTvar{name,...} => name) FCTbind
val findTYCAtBind = findAtBind getTYCs TypesUtil.tycName TYCbind

fun findVARCONAtBind ntc =
       findAtBind getVARs 
                   (fn VALvar{name=[nm],...} => nm | OVLDvar{name,...} => name)
		   VARbind ntc
	 handle Unbound =>
            findAtBind getCONs (fn DATACON{name,...} => name) CONbind ntc

fun getfctarg (t,c) : ((Symbol.symbol * Structure) option) =
      case nth(eventsAt t,c) of
        FCTENTev (FCTB{param=STRvar{name=[nm],binding=formal,...},...}) => 
	  let val binding =
	         case eventsAt (t-1) of
		   ((FCTAPPev(MARKstr(s as APPstr{str,...},_,_)))::_) =>
		       (case str of
			  STRstr{env as REL{s,...},...} => s sub 1
			| _ => formal)
   	         | _ => debugPanic "DebugBindings.getfctarg"
	  in SOME (nm,binding)
          end
      | _ => NONE

fun findSTRAtBind (n,t,c) =
       findAtBind getSTRs (fn STRvar{name=[nm],...} => nm) STRbind (n,t,c)
         handle Unbound => 
	      case getfctarg(t,c) of 
	        SOME(name,s) => 
		   if Symbol.name name = n then 
	             (* dummy up a STRvar *)
		     (~1,STRbind(STRvar{name=[name],binding=s,access=PATH[0]}))
  	           else raise Unbound
              | _ => raise Unbound

(* utility for keeping track of local hiding *)
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


(* utility for extracting from run-time arguments *)
fun nthArgs (evn,n,args) =
  let fun f (_,0,args) = args
        | f (evt::rest,n,args) = f(rest,n-1,tln (args,argCnt evt))
  in f (eventsFor evn,n,args)
  end

(* extract structure names/bindings from OPEN dec *)
fun getopennb ev: ((string list * Structure) list) = 
     case ev of
       OPENev (MARKdec(OPENdec svl,_,_)) =>
		map (fn STRvar{name,binding,...} => 
			(rev (map Symbol.name name),binding)) (rev svl)
     | _ => []

(* full scale finder: takes any qualified id; 
                      returns binding time as well as binding *)
fun findBind findAtBind	findInStruct =
   let fun lastbind t = (#lbt o evdataAt) t
       fun count t = (length o eventsAt) t
       fun checkInStruct x = (findInStruct x; true) handle Unbound => false
       fun find ([n],t,c) =
             let fun loop (0,_,_) = raise Unbound
                   | loop (t,c,vc as (_,ic)) = 
				 if c < count t then
				   if ic = 0 then
                                     (t,c,findAtBind(n,t,c))
		  		       handle Unbound => 
				        (findInOpen (n,t,c) 
				         handle Unbound =>
				          (findInAnonFctarg (n,t,c) 
					   handle Unbound =>
				            loop (t,c+1,checkvis(t,c,vc))))
				    else loop (t,c+1,checkvis(t,c,vc))
		                  else loop(lastbind t,0,vc)
	     in loop (t,c,(0,0))
	     end
         | find (n::r,t,c) =
	     let val stx as (_,_,(_,STRbind(STRvar{binding=s,...}))) = 
		    findSTRBind (r,t,c)
	     in if checkInStruct (n,s)
	        then find ([n], enterStruct stx,0)
	        else raise Unbound
	     end
	 | find ([],_,_) = raise Unbound

       and findInOpen (n,t,c) =
	     let fun f ((sn,st)::r) = if checkInStruct (n,st)
				      then let val stx = findSTRBind(sn,t,c)
				           in find ([n], enterStruct stx,0)
				           end
				      else f r
	           | f nil = raise Unbound
 	     in f (getopennb (nth(eventsAt t,c)))
	     end

       and findInAnonFctarg (n,t,c) = 
	     case getfctarg (t,c) of 
	       SOME (name,st) => 
		 if name = Misc.anonParamName andalso checkInStruct (n, st) 
  	         then find ([n], t-2,0)  (* ??? *)
	         else raise Unbound
	     | _ => raise Unbound
   in
     find
   end


and findSTRBind (nl,t,c) = findBind findSTRAtBind findSTRInStruct (nl,t,c)

and enterStruct (t,c,(i,_)) : int =
  let
    val _ = if (!debugdebug) then
                    (print "entering "; print (t:int); print " "; 
		     print (c:int); print " "; print (i:int); print "\n")
            else ()
    val t' = if i < 0 then t-2 (* FCTENTev: jump into parameter *)
	     else let val {evn,args,...} = evdataAt t
  		      val timearr:int array = 
				 System.Unsafe.cast (hd (nthArgs(evn,c,args)))
		  in timearr sub i
			  handle Nth => debugPanic "DebugBindings.enterStruct bad evn c"
		  end
    val t' = case hd (eventsAt t') of
       	       STRVARev (MARKstr(VARstr(STRvar{name,...}),_,_)) =>
	     	 enterStruct (findSTRBind (rev (map Symbol.name name),t',0))
	     | STRVARev _ => debugPanic "DebugBindings.enterStruct bad STRVARev"
	     | STRENDev _ => t'
             | _ => debugPanic "DebugBindings.enterStruct bad event"
  in if (!debugdebug) then
	   (print "entered at "; print (t':int); print "\n")
     else ();
     t'     
  end


(* Fast version: takes only simple id's, returns binding only.
   Main difference: don't track down binding sites within structures.
   It would take some considerable reorganization to make this idea useful,
    since we would need to fetch run-time object, revise type, etc.
    within the per-namespace code called herein. *)
fun findBind' findAtBind findInStruct =
   let fun lastbind t = (#lbt o evdataAt) t
       fun count t = (length o eventsAt) t
       fun find (n,t,c) =
             let fun loop (0,_,_) = raise Unbound
                   | loop (t,c,vc as (_,ic)) = 
				 if c < count t then
				   if ic = 0 then
                                     findAtBind(n,t,c)
		  		       handle Unbound => 
				        (findInOpen (n,t,c) 
				         handle Unbound =>
				          (findInAnonFctarg (n,t,c) 
					   handle Unbound =>
				            loop (t,c+1,checkvis(t,c,vc))))
				    else loop (t,c+1,checkvis(t,c,vc))
		                  else loop(lastbind t,0,vc)
	     in loop (t,c,(0,0))
	     end
       and findInOpen (n,t,c) =
	     let fun f ((_,st)::r) = (findInStruct(n,st)
		                       handle Unbound => f r)
		   | f nil = raise Unbound
 	     in f (getopennb (nth(eventsAt t,c)))
	     end
       and findInAnonFctarg (n,t,c) = 
	     case getfctarg (t,c) of 
	       SOME (name,st) => 
		 if name = Misc.anonParamName then findInStruct(n, st) 
	         else raise Unbound
	     | _ => raise Unbound
   in
     find
   end

val findVARCONBind = findBind findVARCONAtBind findVARCONInStruct
val findFCTBind = findBind findFCTAtBind (fn _ => raise Unbound)
val findSIGBind = findBind findSIGAtBind (fn _ => raise Unbound)
val findTYCBind = findBind findTYCAtBind findTYCInStruct
val findFIXBind = findBind findFIXAtBind (fn _ => raise Unbound) 

(* look up symbol in a given namespace, and return new binding *)
fun lookVARCON (symbol:Symbol.symbol) : binding  =
      let val n = Symbol.name symbol
	  val _ = dbgprint ("lookVARCON " ^ n ^ "\n")
	  val (t,c,(i,binding)) = findVARCONBind ([n], currentTime(),0) 
      in case binding of
	   VARbind(v as VALvar _) => 
	     let val {evn,args,...} = evdataAt t
		 val bv = nth (nthArgs(evn,c,args),i)
		 val ty = chaseType (t,c) (VARexp(ref v))
		 val lv = namedLvar symbol
		 val _ = dbgprint "gotVALVAR\n"
	     in bind(lv,bv);
		VARbind(VALvar{access=PATH[lv],
			       name=[symbol],
			       typ=ref ty})
             end
	 | VARbind(OVLDvar _) =>
	     (dbgprint "gotOVLDVAR\n";
	      binding)
	 | CONbind(dc as (DATACON{const,rep,sign,...}))  =>
	     let val typ = chaseType(t,c) (CONexp dc)
		 val rep = case rep of
	  	  	     VARIABLE _ => 
				let val {evn,args,...} = evdataAt t
				    val ev = nth(nthArgs(evn,c,args),i)
				    val lv = namedLvar symbol
				in bind (lv,ev);
				   VARIABLE (PATH[lv])
				end
	  	  	   | VARIABLEc _ => 
				let val {evn,args,...} = evdataAt t
				    val ev = nth(nthArgs(evn,c,args),i)
				    val lv = namedLvar symbol
				in bind (lv,ev);
				   VARIABLEc (PATH[lv])
				end
	   	   	   | _ => rep
		 val _ = dbgprint "gotCON\n"
	     in CONbind(DATACON{name=symbol,const=const,
				typ=typ,rep=rep,sign=sign})
	     end
      end

fun lookSTR (symbol:Symbol.symbol) : binding =
	let val n = Symbol.name symbol
	    val _ = dbgprint ("lookSTR " ^ n ^ "\n")
	    val (t,c,(i,STRbind(STRvar{binding=s,...}))) = 
		           findSTRBind ([n], currentTime(),0)
            val s = chaseStr t s
	    val sobj =
		if i >= 0 then 
	          let val {evn,args,...} = evdataAt t
		  in nth(nthArgs(evn,c,args),i+1)
		  end
	        else let val {args,...} = evdataAt (t-1)
		     in hd args
		     end
	    val lv = namedLvar symbol
	    val _ = dbgprint "gotSTR\n"
        in bind (lv,sobj);
	   STRbind(STRvar{name=[symbol],access=PATH[lv],binding=s})
	end

fun lookFCT (symbol:Symbol.symbol) : binding =
        let val n = Symbol.name symbol
	    val _ = dbgprint ("lookFCT " ^ n ^ "\n")
            val (t,c,(i,FCTbind(FCTvar{binding=f,...}))) = 
		findFCTBind ([n], currentTime(),0)
	    val {evn,args,...} = evdataAt t
	    val fobj = nth(nthArgs(evn,c,args),i)
	    val lv = namedLvar symbol
	    val _ = dbgprint "gotFCT\n"
        in bind (lv,fobj);
	   FCTbind(FCTvar{name=symbol,access=PATH[lv],binding=f})
	end

fun lookSIG (symbol:Symbol.symbol) : binding =
        let val n = Symbol.name symbol
	    val _ = dbgprint ("lookSIG " ^ n ^ "\n")
	    val (t,c,(i,SIGbind sv)) = findSIGBind ([n],currentTime(),0)
	    val _ = dbgprint "gotSIG\n"
	in SIGbind(sv)
        end

fun lookTYC (symbol:Symbol.symbol) : binding = 
	let val n = Symbol.name symbol
	    val _ = dbgprint ("lookTYC " ^ n ^ "\n")
	    val (t,c,(i,TYCbind tycon)) = findTYCBind ([n],currentTime(),0)
	    val tycon = chaseTycon t tycon
	    val _ = dbgprint "gotTYC\n"
        in TYCbind(tycon)
	end

fun lookFIX (symbol:Symbol.symbol) : binding =
        let val n = Symbol.name symbol
	    val _ = dbgprint ("lookFIX " ^ n ^ "\n")
	    val (t,c,(i,FIXbind fv)) = findFIXBind ([n],currentTime(),0)
	    val _ = dbgprint "gotFIX\n"
	in FIXbind(fv)
        end

(* intended use: add SPECIAL (looker) to environment at an appropriate spot *)
fun looker (symbol:Symbol.symbol) :binding =
      let val space = Symbol.nameSpace symbol      
      in
      protect (fn () =>
	  case space of
	    Symbol.VARspace => lookVARCON symbol
	  | Symbol.TYCspace => lookTYC symbol
          | Symbol.SIGspace => lookSIG symbol 
          | Symbol.STRspace => lookSTR symbol
          | Symbol.FCTspace => lookFCT symbol
          | Symbol.FIXspace => lookFIX symbol
	  | _ => raise Unbound)
      end



(* Routine getVal supports looking up VARCON's using full path names;
     returns unbundled value, type and binding site information,
     or raises Unbound.
   Intended use: supporting pointing at source. *)
fun getVal (n:string) : System.Unsafe.object * ty * ((int*int)*int) =
    let fun split s =
          let fun sp s =
                let val pos = index (fn c => c = ".") (explode s)
                in substring (s,0,pos) :: 
		          sp (substring(s,pos+1, String.length s - (pos+1)))
                end handle Index => [s]
          in rev (sp s)
          end
    in protect (fn () =>
      let val (t,c,(i,binding)) = findVARCONBind (split n, currentTime(),0) 
      in case binding of
	   VARbind(v as VALvar _) =>
	     let val {evn,args,...} = evdataAt t
 	         val bv = nth (nthArgs(evn,c,args),i)
	         val ty = chaseType (t,c) (VARexp(ref v))
	         val ww = ((evn,c),t)
             in (bv,ty,ww)
             end
	 | VARbind (OVLDvar _) => raise Unbound
	 | CONbind(dc as (DATACON{const,rep,sign,...}))  =>
	     let val {evn,args,...} = evdataAt t
		 val ty = chaseType(t,c) (CONexp dc)
		 val bv = case rep of
	  	  	     VARIABLE _ =>  nth(nthArgs(evn,c,args),i)
	  	  	   | VARIABLEc _ =>  nth(nthArgs(evn,c,args),i)
	   	   	   | _ => System.Unsafe.cast 0 
		                     (* no run-time object exists *)
		 val ww = ((evn,c),t)
	     in (bv,ty,ww)
	     end
      end)
    end

end (* structure *)
