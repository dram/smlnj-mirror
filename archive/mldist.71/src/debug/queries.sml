(* DebugQueries

   User-level queries.
   By convention, these are all relative to an explicitly given time.
   They should only be called when imbedded in some DebugMotion routine
   (e.g., withEstablishedTime or binSearch) that ensures interrupt handler
   is set, established time is reset, etc.
   On interrupt, these routines return something dumb, and expect an outer
   wrapper to do something sensible when they notice the flag is still set.
*)


signature DEBUG_QUERIES =
sig
  type time
  type place 
  type wherewhen (* = place * time *)
  val caller: time -> (wherewhen*wherewhen) 
      (* Return top of function,caller for given time. *)
  val callTrace: int -> time -> 
                 ((wherewhen*wherewhen*(((string*Basics.ty)*System.Unsafe.object) list)) list)
      (* Return specified number of call frames from given time.
         Each frame gives top of function, caller, and list of 
	   ((name*type)*value) for each function argument bound at the call. *)
  val eventDesc : place -> (string * bool * DebugStatic.location * DebugStatic.visible) option
      (* Return descriptive string, pseudo-ness, location, 
         and visibility attributes for given place. *)
  val atCall : time -> bool
      (* Return true iff time refers to a call point (APP, etc.) *)
  val getVal: string -> time -> 
                  (System.Unsafe.object * Basics.ty * wherewhen) option
      (* Return value*type*binding site for given val identifier looking
         backwards from given time. *)
  val printBind: (wherewhen * int) -> unit
      (* Print binding information for binding at given wherewhen. *)
end

structure DebugQueries : DEBUG_QUERIES =
struct
  (* These functions should be imbedded (directly or at some deeper level)
     within some X motion routine, to ensure the interrupt handler is set.
     Policy on interrupts: returns something foolish and relies on caller
     to re-detect pendingInterrupt. *)

  open DebugUtil DebugRun DebugBindings DebugStatic DebugExec 
       Access Absyn Basics PrintAbsyn PrintBasics PrintUtil

  type wherewhen = place * time

  val pdepth = 100
 
  (* Suitable for support functions that do time-travel and may be 
     called from within time-sensitive contexts. Callers must
     check of pendingInterrupt on their return. *)
  fun keepOriginalTime f =
    let val originalTime = currentTime()
    in f() before
       (if not (!DebugSignals.pendingInterrupt) then
 	  resetTo (originalTime)
        else ()) (* caller will need to deal with interrupt anyhow *)
    end

  val eventsAt = eventsFor o evnAt

  fun currentWhereWhen () = ((currentEvn(),0),currentTime())
    
  fun caller (time:time) : (wherewhen*wherewhen) =
      (* return top of function containing time, caller of this function *)
    keepOriginalTime (fn () =>
	let fun lastfunc(0,(evn,_)) = (0,evn,0)
	      | lastfunc(t,(evn,lbt)) =
		   let fun f (FNev (RULE(pat,_))::_,n) = (t,evn,n)
			 | f (HANDLEev (RULE(pat,_))::_,n) = (t,evn,n)
			 | f (evt::rest,n) = f(rest,n+1)
			 | f (nil,_) = lastfunc(lbt,evnLbtAt lbt)
		   in f (eventsFor evn,0)
		   end
	    val (t,evn,n) = lastfunc(time,evnLbtAt time)
	    val (t',evn') = if t > 0 then
			      (t-1,evnAt(t-1))
			    else (0,0)
	in (((evn,n),t),((evn',0),t-1))
	       handle QueryInterrupted => (((0,0),0),((0,0),0))
	end)
  

  fun eventDesc (evn:evn, c:evindex) : 
         (string * bool * location * visible) option =
    let val evt = nth(eventsFor evn,c)
	val (filename,visible) = filenameFor evn
	val charno = locOfEvent evt
    in SOME (eventText evt,isPseudo evt,(filename,charno),visible)
    end handle Nth => NONE
	     | Evn _ => NONE

(* TIME-SENSITIVE VERSION 
  fun eventDesc (evn:evn,c:evindex) (t:time) : 
                       (string * bool * location * visible) option =
    keepOriginalTime (fn () =>
      let val evt = nth(eventsFor evn,c)
      in case evt of
	   NULLev => NONE
         | _ => 
  	   let val desc = eventText evt
	       val (evn',evt') =  
		    if isPseudo evt then  (* use data from time t-1 instead *)
	              (evnAt (t-1), hd(eventsAt (t-1)))
		    else (evn,evt)
	       val (filename,visible) = filenameFor evn'
	       val charno = locOfEvent evt'
	   in SOME(desc,isPseudo evt,(filename,charno),visible)
	   end 
      end handle Nth => NONE
	       | Evn _ => NONE
               | QueryInterrupted => NONE)
*)  

  fun atCall (t:time) =
    case hd (eventsAt t) of
      (APPev _) => true
    | (RAISEev _) => true
    | _ => false

  (* Routine getVal supports looking up VARCON's using full path names;
     returns with unbundled value, type and binding site information option.
   Intended use: supporting pointing at source. 
   Sample packaging:  
    getVal' (n:string) = withEstablishedTime(getVal n)
   with the INTERRUPT x return assumed corrupt. *)

  local
     fun split s =
	 let fun sp s =
	     let val pos = index (fn c => c = ".") (explode s)
	     in substring (s,0,pos) :: 
		 sp (substring(s,pos+1, String.length s - (pos+1)))
	     end handle Index => [s]
	 in rev (sp s)
	 end
  in
  fun getVal (n:string) (time:time) : 
                     (System.Unsafe.object * ty * wherewhen) option  =
      keepOriginalTime (fn () =>
      let val (t,c,(i,binding)) = findVARCONBind (split n, time,0) 
      in case binding of
	   VARbind(v as VALvar _) =>
	     let val (evn,args) = evnArgsAt t
 	         val bv = nth (nthArgs(evn,c,args),i)
	         val ty = chaseType (t,c) (VARexp(ref v))
	         val ww = ((evn,c),t)
             in SOME(bv,ty,ww)
             end 
	 | VARbind (OVLDvar _) => NONE
	 | CONbind(dc as (DATACON{const,rep,sign,...}))  =>
	     let val (evn,args) = evnArgsAt t
		 val ty = chaseType(t,c) (CONexp dc)
		 val bv = case rep of
	  	  	     VARIABLE _ =>  nth(nthArgs(evn,c,args),i)
			   | VARIABLEc _ => nth(nthArgs(evn,c,args),i)
	   	   	   | _ => System.Unsafe.cast 0 
		                     (* no run-time object exists *)
		 val ww = ((evn,c),t)
	     in SOME(bv,ty,ww)
	     end 
      end handle Env.Unbound => NONE | QueryInterrupted => NONE)
  end  (* local *)




  local
    fun nametype (t,c) = fn (v as VALvar{name=[nm],...}) =>
			(Symbol.name nm, 
			 fn () => chaseType (t,c) (VARexp(ref v)))
		  | _ => debugPanic "bad var in queries.nametype"

  in
  fun callTrace (maxdepth:int) (time:time):
      ((wherewhen*wherewhen*(((string*ty)*System.Unsafe.object) list)) list) = 
    keepOriginalTime (fn () => 
	let fun dotype (name,typef) = (name,typef())
	    fun lastfunc(0,(evn,_,_)) = (0,evn,0,nil)
	      | lastfunc(t,(evn,lbt,args)) =
		   let fun f (FNev (RULE(pat,_))::_,n,args) = 
			      (t,evn,n,pairlist (patvars (dotype o (nametype (t,n))) pat) args)
			 | f (HANDLEev (RULE(pat,_))::_,n,args) =
			      (t,evn,n,pairlist (patvars (dotype o (nametype (t,n))) pat) args)
			 | f (evt::rest,n,args) = f(rest,n+1,tln (args,argCnt evt))
  
			 | f (nil,_,_) = lastfunc(lbt,evnLbtArgsAt lbt)
		   in f (eventsFor evn,0,args)
		   end
	    fun up 0 _ = nil
	      | up _ 0 = nil
	      | up t d =
		   let val evdata as (evn,_,_) = evnLbtArgsAt (t-1)
		       val (t',evn',n,varlist) = lastfunc(t-1,evdata)
		   in
		     (((evn,0),t-1),((evn',n),t'),varlist) :: (up t' (d-1))
		   end
	    val cww = currentWhereWhen()
	    val (t',evn',n,varlist) = lastfunc (time,evnLbtArgsAt time)
	in (cww, ((evn',n),t'),varlist) :: (up t' maxdepth) 
	      handle QueryInterrupted => nil  
	           (* could do better if we really wanted to *)
	end)
  end (* local *)

  fun printBind (((evn:evn,c:evindex),t:time),indent:int) : unit =
   keepOriginalTime (fn () => 
    (case nth(eventsFor evn,c) of
       VALev(MARKdec(dec as VALdec(_),_,_)) => 
				  printDec(!debugEnv) (dec,indent,pdepth)
     | VALRECev(MARKdec(dec as VALRECdec(_),_,_)) => 
				  printDec(!debugEnv) (dec,indent,pdepth)
     | FNev(RULE(pat,_)) => 
	  (printPat(!debugEnv) (pat,pdepth); print " <=== ";
	   case hd(eventsAt (t-1)) of
	     APPev(APPexp(_,exp)) => printExp(!debugEnv) (exp,indent+8,pdepth)
	   | _ => print "unknown call site")
     | HANDLEev(RULE(pat,_)) =>
	  (printPat(!debugEnv) (pat,pdepth); print " <=== ";
	   case hd(eventsAt (t-1)) of
	     RAISEev(MARKexp(RAISEexp exp,_,_)) => printExp(!debugEnv) (exp,indent+8,pdepth)
	   | _ => print "implicit exception")
     | CASEev(exp,RULE(pat,_)) => 
	   ( printPat(!debugEnv) (pat,pdepth); print " <=== "; printExp(!debugEnv) (exp,indent+8,pdepth))
       | _ => debugPanic "bad event type in queries.printBind")
               handle QueryInterrupted => ())

end



