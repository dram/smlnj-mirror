(* NO TIME VERSION *)

(* DebugBindings

   Synthesize symbol table bindings.
   Used by DebugEnv and DebugQueries.  
   Uses DebugRun and Calctype facilities.
   
*)

signature DEBUG_NBINDINGS =
sig
  type br
  type evn
  type evindex
  (* Basic binding finders. 
     Argument: qualified identifier (as string list)
               * time and evindex from which to start searching back.
     Returns: time,evindex of event where binding is found
              * (which element in event * binding itself)
     Raises: Env.Unbound if not successful. 
  *)
  val findVARCONBind: (string list * br * evindex) -> 
                            (br * evindex * (int * Modules.binding))
  val findSTRBind: (string list * br * evindex) -> 
                            (br * evindex * (int * Modules.binding))
  val findFCTBind: (string list * br * evindex) -> 
                            (br * evindex * (int * Modules.binding))
  val findSIGBind: (string list * br * evindex) -> 
                            (br * evindex * (int * Modules.binding))
  val findTYCBind: (string list * br * evindex) -> 
                            (br * evindex * (int * Modules.binding))
  val findFIXBind: (string list * br * evindex) -> 
                            (br * evindex * (int * Modules.binding))
  (* Drivers for calctype functions. *)
  val chaseType: (br * evindex) -> BareAbsyn.exp -> Types.ty
  val chaseTycon: br -> Types.tycon -> Types.tycon
  val chaseStr: br -> Modules.Structure -> Modules.Structure
  val callerBr: br * evindex (* of entry event *) -> br
  val nthArgs: evn * evindex * System.Unsafe.object list ->
                  System.Unsafe.object list
	(* Return subset of args for given evindex of evn. *)
end

structure DebugNBindings : DEBUG_NBINDINGS =
struct
  (* Policy on interrupt: pass on QueryInterrupted. *)
  open Array List DebugUtil DebugNStatic DebugNKernel Modules Types
         Access Absyn PrintAbsyn PrintBasics PrintUtil CalcType Variables
  infix 9 sub
  structure U = System.Unsafe

  val eventsAt = eventsFor o evnAt
  
  
  (* utility for extracting from run-time arguments *)
  fun nthArgs (evn,n,args) =
    let fun f (_,0,args) = args
	  | f (evt::rest,n,args) = f(rest,n-1,tln (args,argCnt evt))
    in f (eventsFor evn,n,args)
    end

  fun callerBr (br,c) = 
    if br = nullBr then nullBr else U.cast hd(nthArgs(evnAt br,c,argsAt br))

  (* type chasing *)
  fun encfunct br () = 
    let fun ef br =
	      if br = nullBr then
		NONE
	      else
		let val (evn,lbr) = evnLbrAt br
		in case eventsFor evn of
		     (FCTENTev(_)::_) =>
			  (case eventsAt (callerBr (br,0)) of
			     ((FCTAPPev(MARKstr(s as APPstr(_),_,_)))::_)
			           => SOME (s,0 (* this is a bug! *))
			   | _ => debugPanic "DebugBindings.encfunct missing FCTAPPev")
		   | _ => ef lbr
		end
    in ef br
    end
  
  fun chaseType (br,c) (exp:exp) = 
    let val _ = if (!debugdebug) 
		then (print "fetching type for "; 
			  printExp (!debugEnv) (exp, 0, 1000); print "\n")
		else ();
	fun enc (br,c) () =
	      let fun ef (br,c) =
		    if br = nullBr then NOTENCLOSED
		    else
			let val (evn,lbr) = evnLbrAt br
			    fun f (nil,_) = ef (lbr,0)
			      | f (FNev(RULE(pat,_))::_,c) = 
					ENCLOSED (pat,enc (lbr,0),argf(callerBr(br,c)))
			      | f (HANDLEev(RULE(pat,_))::_,c) =
					ENCLOSED (pat,enc (lbr,0),argh(callerBr(br,c)))
			      | f (_::rest,c) = f (rest,c+1)
			in f (tln(eventsFor evn,c),c)
			end
	      in ef (br,c)
	      end
	and argf br () = 
	      case hd(eventsAt br) of
		APPev(APPexp(_,exp)) => ARGS(exp,enc (br,0),encfunct br)
	      | _ => NOTAVAIL   (* caller not compiled debug? *)
	and argh br () =
	      case hd (eventsAt br) of
		RAISEev(MARKexp(RAISEexp exp,_,_)) => 
					  ARGS(exp,enc (br,0),encfunct br)
	      | _ => NOTAVAIL (* implicit exception? *)
	val typ = getType(exp, enc (br,c), encfunct br)
     in if (!debugdebug) 
	then (print "type is "; PrintType.printType std_out (!debugEnv) typ; print "\n")
	else ();
	typ
    end
  
  fun chaseTycon br = deabstyc (encfunct br)
  
  fun chaseStr br = deabsstr (encfunct br)
  
  exception Unbound = Env.Unbound
  
  (* find a binding within a structure *)
  fun findInStruct symbol (n:string, SIMPLE{env,...}) =
          Env.look (env,symbol n)
    | findInStruct symbol (n: string, INSTANCE{sign=SIG{env,...},...}) =
          Env.look (env,symbol n)
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
  
	  
local
  (* this kludge is needed to pad out the argument positions in FNevs
     and HANDLEevs by one, to account for the caller br argument in pos. 0 *)
  val dummyvar = 
     let val name = Symbol.varSymbol "TOTALJUNK"
         val lvar = namedLvar name
     in VALvar{name=[name],access=PATH[lvar],typ=ref UNDEFty}
     end
in 
  fun getVARs ev : var list = 
	case ev of
	  VALev(MARKdec(VALdec(vbl),_,_)) => vblextract (fn x => x) vbl
	| VALRECev(MARKdec(VALRECdec(rvbl),_,_)) =>
				  map (fn RVB{var,...} => var) rvbl
	| FNev(RULE(pat,_)) => dummyvar :: (patvars (fn x => x) pat) 
	| HANDLEev(RULE(pat,_)) => dummyvar :: (patvars (fn x => x) pat)
	| CASEev(_,RULE(pat,_)) => patvars (fn x => x) pat
	| OVLDev(MARKdec(OVLDdec ovldvar,_,_)) => [ovldvar]
	| _ => []
end  

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
  
  (* finding a named binding of a given sort at a particular (br,sub-event) *)
  fun findAtBind getter namer binder (n,br,c) : (int * binding) =
    let fun find(i,v::r) = if Symbol.name (namer v) = n then (i,binder v)
			   else find(i+1,r)
	  | find(_,nil) = raise Unbound
    in find (0,getter(nth(eventsAt br,c)))
    end
  
  val findFIXAtBind = findAtBind getFIXs (fn FIXvar{name,...} => name) FIXbind
  val findSIGAtBind = findAtBind getSIGs (fn SIGvar{name,...} => name) SIGbind
  val findFCTAtBind = findAtBind getFCTs (fn FCTvar{name,...} => name) FCTbind
  val findTYCAtBind = findAtBind getTYCs TypesUtil.tycName TYCbind
  
  fun findVARCONAtBind nbrc =
	 findAtBind getVARs 
		     (fn VALvar{name=[nm],...} => nm | OVLDvar{name,...} => name)
		     VARbind nbrc
	   handle Unbound =>
	      findAtBind getCONs (fn DATACON{name,...} => name) CONbind nbrc

  exception NewModulesSystem3
  
  fun getfctarg (br,c) : ((Symbol.symbol * Structure) option) =
      raise NewModulesSystem3
(*******
	case nth(eventsAt br,c) of
	  FCTENTev (FCTB{param=STRvar{name=[nm],binding=formal,...},...}) => 
	    let val binding =
		   case eventsAt(callerBr(br,c)) of
		     ((FCTAPPev(MARKstr(s as APPstr{str,...},_,_)))::_) =>
			 (case str of
			    STRstr{env as REL{s,...},...} => s sub 1
			  | _ => formal)
		   | _ => debugPanic "DebugBindings.getfctarg"
	    in SOME (nm,binding)
	    end
	| _ => NONE
*********)
  
  fun findSTRAtBind (n,br,c) =
	 findAtBind getSTRs (fn STRvar{name=nm,...} => nm) STRbind (n,br,c)
	   handle Unbound => 
		case getfctarg(br,c) of 
		  SOME(name,s) => 
		     if Symbol.name name = n then 
		       (* dummy up a STRvar *)
		       (~1,STRbind(STRvar{name=name,binding=s,access=PATH[0]}))
		     else raise Unbound
		| _ => raise Unbound
  
  (* utility for keeping track of local hiding *)
  fun checkvis(br,c,vc as(ec,ic)) =
    case nth(eventsAt br,c) of
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
  
  
  (* extract structure names/bindings from OPEN dec *)
  fun getopennb ev: ((string list * Structure) list) = 
       case ev of
	 OPENev (MARKdec(OPENdec svl,_,_)) =>
		  map (fn STRvar{name,binding,...} => 
			  ([Symbol.name name],binding)) (rev svl)
       | _ => []
  
  (* full scale finder: takes any qualified id; 
			returns binding "time" (br)  as well as binding *)
  fun findBind findAtBind findInStruct =
     let fun count br = (length o eventsAt) br
	 fun checkInStruct x = (findInStruct x; true) handle Unbound => false
	 fun find ([n],br,c) =
	       let fun loop (br,c,vc as (_,ic)) = 
		                 if br = nullBr then
				   raise Unbound
				 else
				   if c < count br then
				     if ic = 0 then
				       (br,c,findAtBind(n,br,c))
					 handle Unbound => 
					  (findInOpen (n,br,c) 
					   handle Unbound =>
					    (findInAnonFctarg (n,br,c) 
					     handle Unbound =>
					      loop (br,c+1,checkvis(br,c,vc))))
				      else loop (br,c+1,checkvis(br,c,vc))
				    else loop(lbrAt br,0,vc)
	       in loop (br,c,(0,0))
	       end
	   | find (n::r,br,c) =
	       let val stx as (_,_,(_,STRbind(STRvar{binding=s,...}))) = 
		      findSTRBind (r,br,c)
	       in if checkInStruct (n,s)
		  then find ([n], enterStruct stx,0)
		  else raise Unbound
	       end
	   | find ([],_,_) = raise Unbound
  
	 and findInOpen (n,br,c) =
	       let fun f ((sn,st)::r) = if checkInStruct (n,st)
					then let val stx = findSTRBind(sn,br,c)
					     in find ([n], enterStruct stx,0)
					     end
					else f r
		     | f nil = raise Unbound
	       in f (getopennb (nth(eventsAt br,c)))
	       end
  
	 and findInAnonFctarg (n,br,c) = 
	       case getfctarg (br,c) of 
		 SOME (name,sbr) => 
		   if name = Misc.anonParamName andalso checkInStruct (n, sbr) 
		   then find ([n], callerBr(callerBr(br,c),0),0)
		   else raise Unbound
	       | _ => raise Unbound
     in
       find
     end
  
  
  and findSTRBind (nl,br,c) = findBind findSTRAtBind findSTRInStruct (nl,br,c)
  
  and enterStruct (br,c,(i,_)) : br =
    let
      val _ = if (!debugdebug) then
		      (print "entering struct ";
		       print (c:int); print " "; print (i:int); print "\n")
	      else ()
      val br' = if i < 0 then callerBr(callerBr(br,c),0) 
		                (* FCTENTev: jump into parameter *)
	       else let val (evn,args) = evnArgsAt br
			val brarr:br array = U.cast (hd (nthArgs(evn,c,args)))
		    in brarr sub i
			    handle Nth => debugPanic "DebugNBindings.enterStruct bad evn c"
		    end
      val br' = case hd (eventsAt br') of
		 STRVARev (MARKstr(VARstr(STRvar{name,...}),_,_)) =>
		   enterStruct (findSTRBind ([Symbol.name name],br',0))
	       | STRVARev _ => debugPanic "DebugNBindings.enterStruct bad STRVARev"
	       | STRENDev _ => br'
	       | _ => debugPanic "DebugNBindings.enterStruct bad event"
    in if (!debugdebug) then
	     (print "entered\n")
       else ();
       br'     
    end
  
  
  (* Fast version: takes only simple id's, returns binding only.
     Main difference: don't track down binding sites within structures.
     It would take some considerable reorganization to make this idea useful,
      since we would need to fetch run-time object, revise type, etc.
      within the per-namespace code called herein. 
  fun findBind' findAtBind findInStruct =
     let fun count t = (length o eventsAt) t
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
				    else loop(lbtAt t,0,vc)
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
*)

  
  val findVARCONBind = findBind findVARCONAtBind findVARCONInStruct
  val findFCTBind = findBind findFCTAtBind (fn _ => raise Unbound)
  val findSIGBind = findBind findSIGAtBind (fn _ => raise Unbound)
  val findTYCBind = findBind findTYCAtBind findTYCInStruct
  val findFIXBind = findBind findFIXAtBind (fn _ => raise Unbound) 
    
end (* structure *)
