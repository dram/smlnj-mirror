(* DebugStatic

   Handles (mostly) compile-time data about debuggable code.  
   Basic concepts are events, evns, compilation unit data, and location data.

   Events are defined as tagged pointers into abstract syntax. Tags (expressed
     as the constructors of datatype event) indicate the variety of event.
     There are operators to return standard text description of the event
     variety, the number of arguments associated with the event, and the
     location (character position) of the event within the source file
     from which it was compiled. 

   When the user program returns control to the debugger, it does so at
     a particular time and *event number* (evn).  Note that because 
     the instrumenter coalesces neighboring events (roughly when they 
     are in the same basic block), each evn is associated with 
     a *list* of events, in reverse order of their natural sequence.

   This module maintains a global description of all events indexed by evn.
     In addition to the events themselves, this description includes the
     arrays of lastEventTimes, which tell the last time each evn was 
     encountered during execution.  These arrays are maintained by special
     code added by the instrumenter, and are used by the breakpointing
     mechanism (see DebugMotions). (These arrays are actually part of the
     dynamic state, and thus constitute a history-maintaining subsystem.)
     The description also maintains each evn's (static) last binding evn; 
     this is a byproduct of instrumentation that is currently unused.

   There is also an index to all events keyed by location, i.e., filename
     plus character position. This is primarily intended as a means for
     setting breakpoints.  At present, only events that can directly trigger
     a break (i.e., those at the heads of the event lists associated with
     an evn) are indexed, since the simplest view of breakpoints only permits
     them at these points.  Multiple evns can have the same character
     position; in this case their relative order is undefined.

   Events are added to these databases on a per-compilation-unit basis,
     immediately after instrumentation. Since there is no point in
     keeping event data if the compilation unit eventually terminates
     abnormally, we add the data only tentatively, and later commit or
     rollback the addition.

   Compilation units (and the events within them) have a visibility 
     attribute: this is set false (by an appropriate call from the
     interactive system) if the filename associated with the unit
     is recompiled, so that clients who use location data won't get confused
     about which file is really meant.
     A more elaborate system might user version numbers or datestamps to
     distinguish versions of the same file name.

   There are also a few *pseudo-events*. These are events that occur as a
     explicit debugger code (e.g., in the debugger's IO library) rather
     than within user code, and so have no location.  They are given fixed
     evn's and various support functions.
*)

signature DEBUG_STATIC =
sig
  datatype event				(* objects passed at runtime *)
   = APPev of Absyn.exp
   | RAISEev of Absyn.exp
   | HANDLEev of Absyn.rule			(* bound value objs *)
   | FNev of Absyn.rule				(* bound value objs *)
   | CASEev of Absyn.exp * Absyn.rule		(* bound value objs *)
   | VALev of Absyn.dec				(* bound value objs *)
   | VALRECev of Absyn.dec			(* bound value objs *)
   | STRev of Absyn.dec * (int list)		(* time array, strvar objs *)
   | ABSev of Absyn.dec * (int list)		(* time array, strvar objs *)
   | FCTev of Absyn.dec * (int list)		(* fctvar objs *)
   | SIGev of Absyn.dec
   | TYPEev of Absyn.dec
   | FIXev of Absyn.dec
   | OVLDev of Absyn.dec
   | EXCEPTIONev of Absyn.dec			(* exception objs *)
   | FCTENTev of Absyn.fctb
   | FCTAPPev of Absyn.strexp                   (* param strvar obj *)
   | STRENDev of Absyn.strexp
   | STRVARev of Absyn.strexp
   | OPENev of Absyn.dec
   | LETev of Absyn.exp
   | LOCALev of Absyn.dec
   | LOCINev of Absyn.dec
   | LOCENDev of Absyn.dec
   | IOev     (* pseudo-event raised at IO, etc. *)
   | UNCAUGHTev  (* pseudo-event raised on uncaught exception *)
   | STARTev of Absyn.dec (* entire compilation unit *)
   | ENDev of Absyn.dec (* entire compilation unit *)
   | NULLev

  type time (* = int *)

  type filename (* = string *)
  type charno (* = int *)
  type location (* = filename * charno *)
  type visible (* = bool *)

  type evn (* = int *)
  type evindex (* = int *)
      (* Index into list of events for an evn *)
  type place (* = evn * evindex *)
      (* There is a 1-1 correspondence between events and places *)

  (* Displayable text corresponding to variety of event. *)
  val eventText: event -> string
  (* Number of arguments passed to break with this event. *)
  val argCnt: event -> int
  (* Location of event in source code; filename is factored out. *)
  val locOfEvent:event -> charno
  
  (* Utility functions for pseudo-events. *)
  (* Return pseudo-evn corresponding to event. *)
  val pseudoEvn: event -> evn
  (* Tell if event is pseudo. *)
  val isPseudo: event -> bool
 
  (* Compilation unit handling. *)
  val install: {file:filename,firstEvn:evn,evCount:int,
		events:event list array,elb:evn array} -> unit
      (* Add the described events (produced by instrumenting the file)
         to our databases, but only tentatively. *)
  val commit: unit -> unit  (* make tentative installations permanent *)
  val rollback: unit -> unit (* remove tentative installations *)
  val nextEvn: unit -> evn  (* next free evn *)

  (* This function should be called by the interactive system once
     per *file* (not comp unit) before installing any new events.
     It marks any previous comp units tagged with this file name as hidden. *)
  val hideFile:filename -> unit 

  (* Following operate on all units, including tentatively installed ones. *)
  exception Evn of evn  
     (* Raised if no such evn or other internal problem *)
  val eventTimesArray: evn -> time array
     (* Return array that begins with this evn.  Used only to pass the array
        to instrumented code at runtime. *)
  val lastTime: evn -> time  (* Returns eventTimesArray entry for evn *)
  val eventsFor: evn -> event list (* Returns events for evn *)
  val elbFor: evn -> evn  (* Returns static last binding evn for evn *)
  val filenameFor: evn -> (filename * visible)
      (* Returns filename and visibility attribute for evn *)
  val eventPlacesAfter: location -> place list
      (* Returns (evn,0) at or after location *)
  val eventPlacesBefore: location -> place list
      (* Returns (evn,0) at or before location *)
  val eventCount: int ref
      (* Holds number of events in last installed unit (tentative or perm) *)

  (* Support history mechanism for eventTimes arrays *)
  val rememberEventTimes: unit -> DebugKernel.doers
end

structure DebugStatic: DEBUG_STATIC =
struct
  open Array List DebugUtil DebugKernel Access Basics Absyn
  infix 9 sub

  datatype event
   = APPev of exp
   | RAISEev of exp
   | HANDLEev of rule
   | FNev of rule
   | CASEev of exp * rule
   | VALev of dec
   | VALRECev of dec
   | STRev of dec * (int list)
   | ABSev of dec * (int list)
   | FCTev of dec * (int list)
   | SIGev of dec
   | TYPEev of dec
   | FIXev of dec
   | OVLDev of dec
   | EXCEPTIONev of dec
   | FCTENTev of Absyn.fctb
   | FCTAPPev of Absyn.strexp
   | STRENDev of Absyn.strexp
   | STRVARev of Absyn.strexp
   | OPENev of dec
   | LETev of Absyn.exp
   | LOCALev of Absyn.dec
   | LOCINev of Absyn.dec
   | LOCENDev of Absyn.dec
   | IOev    (* pseudo-event raised at IO, etc. *)
   | UNCAUGHTev (* pseudo-event raised on uncaught exception *)
   | STARTev of Absyn.dec
   | ENDev of Absyn.dec
   | NULLev
  
  fun eventText (evt:event) : string =
       case evt of
	 VALev(_) => "VAL"
       | VALRECev(_) => "VALREC"
       | FNev(_) => "FN"
       | CASEev(_) => "CASE"
       | APPev(_) => "APP"
       | RAISEev(_) => "RAISE"
       | HANDLEev(_) => "HANDLE"
       | STRev(_) => "STRUCTURE"
       | ABSev(_) => "ABSTRACTION"
       | FCTev(_) => "FUNCTOR"
       | SIGev(_) => "SIGNATURE"
       | TYPEev(_) => "TYPE"
       | FIXev(_) => "FIXITY"
       | OVLDev(_) => "OVERLOAD"
       | EXCEPTIONev(_) => "EXCEPTION"
       | FCTENTev(_) => "FUNCTOR ENTRY"
       | FCTAPPev(_) => "FUNCTOR APP"
       | STRENDev(_) => "STRUCTURE END"
       | STRVARev(_) => "STRUCTURE VAR"
       | OPENev(_) => "OPEN"
       | LETev(_) => "LET"
       | LOCALev(_) => "LOCAL"
       | LOCINev(_) => "LOCAL IN"
       | LOCENDev(_) => "LOCAL END"
       | IOev => "IO"
       | UNCAUGHTev => "UNCAUGHT EXCEPTION"
       | STARTev(_) => "START"
       | ENDev(_) => "END"
       | NULLev => "NULL"

  type filename = string
  type charno = int  (* counting from 1 *)
  type location = filename * charno
  type visible = bool (* true if file has not been hidden by reusing *)
 
  type evindex = int
  type place = evn * evindex 

  type cud = {file:filename,visible:visible ref,
	      firstEvn:evn, evCount: int, 
	      events:event list array, elb:evn array, 
	      eventTimes: time array}
  

  (* initial cud contains backstop NULLev and other pseudo-events *)
  val pseudos = 3
  val initialCud = {file="",visible=ref false,
		    firstEvn=0,evCount=pseudos,
		    events=array(pseudos,[NULLev]),
		    elb=array(pseudos,0),eventTimes=array(pseudos,0)}
  val _ = (update(#events initialCud,1,[IOev]);
	   update(#events initialCud,2,[UNCAUGHTev]))
  fun pseudoEvn NULLev = 0
    | pseudoEvn IOev = 1
    | pseudoEvn UNCAUGHTev = 2
    | pseudoEvn _ = debugPanic "static.pseudoEvn"
  fun isPseudo NULLev = true
    | isPseudo IOev = true
    | isPseudo UNCAUGHTev = true
    | isPseudo _ = false

  (* These structures support use of cuds and locIndexes on a tentative
     basis, before we know whether the unit has been successfully executed
     or not. Value currentCud refers to a tentative unit being executed 
     currently; value lastCud refers  to the last committed unit. *)
  val lastCud = ref initialCud
  val currentCud = ref initialCud
  val eventCount = ref 0

  exception Evn of evn
  (* cud structure *)
  structure CudSet = SortedSet (
	   struct
	     type t = cud
	     type k = evn
	     fun key ({firstEvn,...}:cud) : evn = firstEvn
	     val lt = Integer.<
	   end)
  
  local 
    open CudSet
    val cuds = ref (insert(new(),initialCud)) 
  in
    fun resetCuds () = cuds := insert(new(),initialCud)
    
    fun addCud (cud as {firstEvn,...}:cud) =
	 ((cuds := delete (!cuds,firstEvn)) handle NotFound => ();
	  cuds := insert (!cuds,cud))	
    
    fun eventTimesArray evn = 
      #eventTimes (find (!cuds,evn))
	handle _ => raise (Evn evn)
    
    fun lastTime evn  =  (* fetches value from within array *)
      let val {firstEvn,eventTimes,...} = findp (!cuds, evn)
      in eventTimes sub (evn-firstEvn)
      end handle _ => raise (Evn evn)
    
    fun eventsFor evn =
      let val {firstEvn,events,...} = findp (!cuds,evn:int)
      in events sub (evn-firstEvn)
      end handle _ => raise (Evn evn)
    
    fun elbFor evn = 
      let val {firstEvn,elb,...} = findp (!cuds,evn)
      in elb sub (evn-firstEvn)
      end handle _ => raise (Evn evn)
    
    fun filenameFor evn =
      let val {file,visible,...} = findp (!cuds,evn)
      in (file,!visible)
      end handle _ => raise (Evn evn)

    fun hideCud cud = #visible(cud:cud) := false

    local 
      structure EventTimesSet = SortedSet (
	   struct
	     type t = evn * time array
	     type k = evn
	     fun key (t,_) : evn = t
	     val lt = Integer.<
	   end)

      fun saveEventTimes() : EventTimesSet.s =
        let fun f ({firstEvn,eventTimes,...}:cud,ets) =
	      EventTimesSet.insert(ets,(firstEvn,copyarray eventTimes))
	in fold (!cuds,f,EventTimesSet.new())
	end
				      
      fun restoreEventTimes (ets:EventTimesSet.s) =
        let fun f ({firstEvn,eventTimes,...}:cud) =
	      let val (_,eta) = EventTimesSet.find(ets,firstEvn)
              in resetarray eventTimes eta
	      end handle EventTimesSet.NotFound => 
		   (* zero times *)
		   let fun g n = (Array.update(eventTimes,n,0); g (n+1))
		   in g 0 handle Subscript => ()
		   end
        in iterate (!cuds,f)
	end
	  
    in
    fun rememberEventTimes () = 
      let val ets = saveEventTimes()
          fun reset _ = restoreEventTimes ets
      in {undo=reset,redo=reset}
      end

    end (* local *)

  end (* local open structure CudSet *)

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
  
      | SIGev(MARKdec(_,s,e)) => e
	      (* signature a = sig end <*> *)
  
      | TYPEev(MARKdec(_,s,e)) => e
	      (* datatype t = A | B of int <*> *)
  
      | FIXev(MARKdec(_,s,e)) => e
	      (* infix 6 + - <*> *)
  
      | OVLDev(MARKdec(_,s,e)) => e
	      (* overload ~:('a->'a) as Integer.~ and Real.~ <*> *)
  
      | EXCEPTIONev(MARKdec(_,s,e)) => e
	      (* exception E <*> *)
  
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
      | UNCAUGHTev => 0
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
  
  (* LocIndex handling *)
  (* this version handles only coarse events *)
  
  structure CharnoSet = SortedSet (
  struct
    type t = charno * (evn list)
    type k = charno
    fun key (charno,wl) = charno
    val lt = Integer.<
  end)
  
  type locindex = (filename * CharnoSet.s * (cud list)) list
  
  val emptyLocIndex = nil:locindex
  (* Following are similar to lastCud, currentCud, above. *)
  val lastLocIndex = ref emptyLocIndex
  val currentLocIndex = ref emptyLocIndex
  
  fun augmentLocIndex (oldIndex, cud as {file,firstEvn,events,...}:cud) =
    let fun putin (cs,charno,evn) =
	  let open CharnoSet
	  in if charno > 0 then
	       let val (_,evnl) = find(cs,charno)
	       in update (cs, (charno,evn::evnl))
	       end handle NotFound => insert (cs, (charno,[evn]))
	     else cs
	  end
	fun augcs cs =
	  let fun d (evn,cs) = 
		d(evn+1,putin(cs,locOfEvent(hd (events sub evn)),evn+firstEvn))
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
  
  fun eventPlacesAfter (file:filename, charno:charno) : place list =
    let fun find ((file',cs,_)::rest) = if file = file' then cs else find rest
	  | find nil = raise CharnoSet.NotFound
	val cs = find (!currentLocIndex)
	val (_,evnl) = CharnoSet.finds (cs,charno)
    in map (fn evn => (evn,0)) evnl
    end handle CharnoSet.NotFound => nil
  
  fun eventPlacesBefore (file:filename, charno:charno) : place list =
    let fun find ((file',cs,_)::rest) = if file = file' then cs else find rest
	  | find nil = raise CharnoSet.NotFound
	val cs = find (!currentLocIndex)
	val (_,evnl) = CharnoSet.findp (cs,charno)
    in map (fn evn => (evn,0)) evnl
    end handle CharnoSet.NotFound => nil
  
  fun hideFile (file:filename) =
    let fun zap ((f as (file',_,cuds))::rest) = 
	      if file = file' then
		(app hideCud cuds;
		 rest)
	      else f::(zap rest)
	  | zap nil = nil
    in lastLocIndex := zap (!lastLocIndex) 
    end 
  
  fun argCnt evt =
    case evt of 
      VALev(MARKdec(VALdec vbl,_,_)) => length (vblextract (fn x => x) vbl)
    | VALRECev(MARKdec(VALRECdec rvbl,_,_)) => length rvbl
    | FNev(RULE(pat,_)) => length (patvars (fn x => x) pat)
    | HANDLEev(RULE(pat,_)) => length (patvars (fn x => x) pat)
    | CASEev(_,RULE(pat,_)) => length (patvars (fn x => x) pat)
    | STRev(MARKdec(STRdec strbl,_,_),_) => 1 + length strbl
    | ABSev(MARKdec(ABSdec strbl,_,_),_) => 1 + length strbl
    | FCTev(MARKdec(FCTdec fctbl,_,_),_) => length fctbl
    | EXCEPTIONev(MARKdec(EXCEPTIONdec ebl,_,_)) => length ebl
    | FCTAPPev _ => 1
    | _ => 0


  (* Create a new cud and locIndex and install them tentatively as "current".*)
  fun install {file:filename,firstEvn:evn,evCount:int,
	       events:event list array,elb:evn array} =
    let val cud = {file=file,
		   visible=ref true,
		   firstEvn=firstEvn,
		   evCount=evCount,
		   events=events,
		   elb=elb,
		   eventTimes=array(evCount,0)}
    in
      currentCud := cud;
      addCud cud;
      currentLocIndex := augmentLocIndex(!currentLocIndex,cud);
      eventCount := evCount;
      if (!debugdebug) then
        (print "Entering cud ";print firstEvn; print " "; 
         print evCount; print "\n";
        dumpEvents (firstEvn,events,elb))
      else ()
    end

  (* execute this when unit has been successfully executed. *)
  fun commit() = (lastCud := !currentCud;
	         lastLocIndex := !currentLocIndex)

  fun rollback() = (currentCud := !lastCud;
		    currentLocIndex := !lastLocIndex)

  (* query functions *)
  fun nextEvn() = #firstEvn (!currentCud) + #evCount (!currentCud)

end (* structure DebugStatic *)
