signature DEBUGRUN =
sig
  val finalTime: int ref
  val initialTime: int ref
  val running: bool ref
  val currentTime: unit -> int

  val maxstates: int ref

  datatype 'a result =
     NORMAL of 'a
   | EXCEPTION of exn
   | SUSPENDED
   | ABORTED
   | INTERRUPTED

  type eventdata

  val lastException: exn option ref
  val interruptRaised:bool ref
  val setinthand:unit -> unit
  val resetinthand:unit -> unit

  val minTimeDelta: int ref
  val maxTimeDelta: int ref

  val init: (unit -> 'a) -> 'a result
  val abort: unit -> 'a
  val restoreTime: (int*int) -> eventdata
  val advanceTime: (int*int*bool*(unit->bool)) -> eventdata
  val confirmTime: unit -> unit

  val provocation:exn option ref

  val addAction:(unit->unit) -> unit 

  val reset: unit -> unit
end

structure DebugRun: DEBUGRUN =
struct
  open DebugKernel DebugUtil DebugStatic

  val finalTime = ref 0
  val completedTime = ref 0
  val initialTime = ref 1
  val running = ref false

  fun currentTime () = times sub 0
  fun targetTime () = times sub 1
  
  datatype 'a result =
     NORMAL of 'a
   | EXCEPTION of exn
   | SUSPENDED
   | ABORTED
   | INTERRUPTED
  
  val provocation:exn option ref = ref NONE

  (* conts *)
  
  fun makeCont () = callcc(fn a=> let val x= callcc(fn b=> throw a b) 
				  in debugPanic "using empty cont"
				  end)
  
  val emptyUnitCont:unit cont = makeCont()
  val debugCont:unit cont ref = ref emptyUnitCont
  val currentCont:unit cont ref = ref emptyUnitCont

  (* executions *)
  
  structure ExecSet = SortedSet (
    struct 
      type t = int * unit cont
      type k = int
      fun key (k,_) = k
      val lt = Integer.<
    end)
  
  
  local 
    open ExecSet
    val execs = ref (insert(new(),(0,emptyUnitCont)))
  in
    fun resetExecs () = 
	  execs := insert(new(),(0,emptyUnitCont))
  
    fun addExec t c = 
      ((execs := delete(!execs, t)) handle NotFound => ();
       execs := insert (!execs,(t,c)))
  
    fun findExec t =
      #2 (find(!execs,t)) 
  end
  
  (* states *)
  
  type eventdata = {evn:int,lbt:int,args:(System.Unsafe.object list)}
  type state = {cont:unit cont,evtimes:eventtimescookie,
		sthandle:HistoryStore.storehandle,iohandle:HistoryIO.iohandle,
		evdata:eventdata}
  val nullEvdata = {evn=0,lbt=0,args=nil}
  val currentEvdata:(eventdata ref) = ref nullEvdata;
  val initStorehandle = ref (HistoryStore.remember())
  val initIOhandle = ref (HistoryIO.remember())
  
  structure StateSet = SortedSet(
    struct
      type t = int * state * int (* use count *)
      type k = int
      fun key (k,_,_) = k
      val lt = Integer.<
    end)
  
  local 
    open StateSet
    val states = ref (new())
  in 
  
  val maxstates = ref 10	(* maximum number of states to maintain *)
				(* special value 0: keep 1 state only, and
				   remove it before storing next state *)
  val usenum = ref 0	(* use counter for maintaining LRU *)
  
  fun resetStates() =
    (states := new();
     usenum := 0)
  
  fun findPrevState time = 
    let val (ptime,pstate,_) = findp(!states,time)
    in inc usenum;
       states := update (!states, (ptime,pstate,!usenum));
       (ptime,pstate)
    end handle NotFound => (0, {cont=findExec 0,
			        evtimes=zeroEventTimes(), 
			        sthandle= !initStorehandle,
			        iohandle= !initIOhandle, 
			        evdata=nullEvdata})
  fun findSuccState time =
    let val (stime,sstate,_) = finds(!states,time)
    in inc usenum;
       states := update(!states, (stime,sstate,!usenum));
       SOME (stime,sstate)
    end handle NotFound => NONE

  fun zapStates(time) = (* remove all states for times >= arg *)
    let fun zap () =
      let val (t,_,_) = finds(!states,time)
      in states := delete(!states,t);
	 zap()
      end handle NotFound => ()
    in zap()
    end
  
  fun saveState(0,_) = ()
    | saveState(time,state) =
    (if (size(!states) >= (!maxstates))
     then
  (* this computes LRU time -- don't use for now --
       let val (zaptime,_) = fold(!states, 
				  fn ((time,_,use),(mt,mu)) => 
				     (if use < mu 
				      then (time,use)
				      else (mt,mu)), 
				  (0,infinity)) 
  *)
       let val (_,zaptime,bd) = revfold(!states,
				  fn ((time,_,_),(pt,bt,bd)) =>
				     (if (time-pt) < bd
				      then (time,time,time-pt)
				      else (time,bt,bd)),
				  (0, 0, infinity))
       in if (!debugdebug) 
	  then (print "zapping time "; print zaptime; print " delta "; print bd;
	       print "\n")
	  else ();
	  (states := delete(!states,zaptime)) handle NotFound => ()
       end
     else ();
     inc usenum;
     (states := insert(!states,(time,state,!usenum))) handle DuplicateKey => ())
  
  end (* let open structure StateSet *)
  
  
  (* Time and state *)
  
  fun saveCurrentState() =
    (saveState (currentTime(), 
	        {cont= !currentCont,
		 evtimes=saveEventTimes(),
		 sthandle=HistoryStore.remember(),
		 iohandle=HistoryIO.remember(),
		 evdata= !currentEvdata});
     if (!debugdebug) then
       print ("storing time "^makestring(currentTime())^"\n")
     else ())

  fun restore (rtime,rstate:state) =
    (update (times,0,rtime);  (* currentTime *)
     currentEvdata := #evdata rstate;
     HistoryStore.restore (#sthandle rstate); 
     HistoryIO.restore (#iohandle rstate);
     restoreEventTimes (#evtimes rstate);
     currentCont := #cont rstate;
     provocation := NONE)

  fun zapFrom t =
    (HistoryIO.zap t;
     HistoryStore.zap t;
     zapStates t)

  fun zap () =
    (update(times,0,!completedTime);  (* currentTime *)
     finalTime := !completedTime;
     initialTime := !completedTime + 1;
     zapFrom (!completedTime + 1);
     running := false)
  
  val interruptRaised = ref false;
  val debughandsetcount = ref 0;

  val normalinthand = 
	ref (NONE:((int * unit cont) -> unit cont) option)

  fun debuginthand (count,cont) =
    (interruptRaised := true;
     update(times,1,currentTime()+1);	(* stuff targetTime to force break *)
     cont)

  fun setinthand () =
    (inc debughandsetcount;
     if !debughandsetcount = 1 then
      (interruptRaised := false;
       normalinthand := System.Signals.inqHandler(System.Signals.SIGINT);
       System.Signals.setHandler (System.Signals.SIGINT, SOME(debuginthand)))
     else ())

  fun resetinthand () =
     (case !debughandsetcount of
        0 => debugPanic "DebugRun.resetinthand"
      | 1 => (dec debughandsetcount;
  	      System.Signals.setHandler (System.Signals.SIGINT,!normalinthand))
      | n => dec debughandsetcount)
	    
  val lastException:exn option ref = ref NONE;
  val stopOnException = ref false;
  val stoppedByException = ref false;
  val returnCont:'a result cont ref = 
			  System.Unsafe.cast ref (makeCont());
  
  fun init f =
    let fun rununit ()=
	   let val result = f()
	   in if (currentTime() <= !completedTime) then
		    (throw (findExec (currentTime()))) ()
			       handle NotFound =>
				  debugPanic ("findExec call"
						 ^ (makestring (currentTime())))
	      else
		    (running := false;
		     completedTime := currentTime();
		     initialTime := !completedTime + 1;
		     NORMAL result)
	   end
	   handle exn =>
		 (if (!debugdebug) then print ("handling exception " ^
					  (System.exn_name exn) ^ 
					  " time " ^ (makestring (currentTime()))^
					  "\n") else ();
		  if (!stopOnException) then
                    (finalTime := currentTime();
                     lastException := SOME exn;
		     stoppedByException := true;
		     update(times,1,currentTime());  (* targetTime *)
		     restore (findPrevState (currentTime()));
  		     HistoryIO.silent := true;
		     if (targetTime() > currentTime()) then
		       throw (!currentCont) ()
		     else throw (!debugCont) ())
		  else 
		    (zap ();
		     EXCEPTION exn))
    in
      setinthand();
      let val p as (ptime,pstate) = findPrevState(!completedTime)
      in restore p;
	 if (ptime < !completedTime) then 
	   (update (times,1,!completedTime);   (* targetTime *)
	    HistoryIO.silent := true;
	    callcc (fn cont => (debugCont := cont;
	                        throw (!currentCont) ()));
	    saveCurrentState())
	 else ()
      end;
      finalTime := infinity;
      lastException := NONE;
      interruptRaised := false;
      running := true;
      stopOnException := false;
      callcc (fn a => (returnCont := a;
		       callcc (fn b => (addExec (!completedTime) b;
					sizereport ("exec " ^ 
						makestring(!completedTime));
					(* advance to initialTime,
					    setting currentCont *)
					update (times,1,!initialTime);
					callcc (fn c => (debugCont := c;
							 throw a (rununit())));
					saveCurrentState();
		     		        throw a SUSPENDED));
		       throw a (rununit())))
	before (resetinthand())

(*
      callcc (fn a => (returnCont := a;
		       callcc (fn b => (currentCont := b;
					addExec (!completedTime) b;
					sizereport ("exec " ^ 
						makestring(!completedTime));
					throw a SUSPENDED));
		       throw a (rununit())))
        before (resetinthand())
*)
    end
  
  fun abort() = (zap();
		 throw (!returnCont) ABORTED)

  fun bbreak(args:System.Unsafe.object array) = 
     (callcc(fn cont =>  (currentCont := cont; 
			  currentEvdata := {
				evn=System.Unsafe.cast (args sub 0),
				lbt=System.Unsafe.cast (args sub 1),
				args=let fun makelist n =
					  (Array.sub(args,n)::(makelist (n+1)))
						  handle Subscript => nil
				     in makelist 2 
			     	     end};
			  case (hd o eventsFor o #evn) (!currentEvdata) of
			    ENDev _ => if currentTime() > !completedTime then
					  finalTime := currentTime()
				        else ()
			  | _ => ();
			  throw (!debugCont) ()));
      case !provocation of
        NONE => ()
      | SOME exn => (provocation := NONE;
		     (* Note: eventtimes not updated! - is this right?? *)
		     raise exn))
  
  (* install break function in kernel *)
  val _ = break := bbreak;


  (* actions *)
  (* actions tagged with time t are considered to be done *before* we
     can stop at time t. *)

  structure ActionSet = SortedSet (
    struct 
      type t = int * ((unit->unit) list)
      type k = int
      fun key (k,_) = k
      val lt = Integer.<
    end)
  
  
  local 
    open ActionSet
    val actionSet = ref (new())
  in
    fun addAction (act:unit->unit) =  (* tag with current time *)
       (let val (_,existing) = find(!actionSet,currentTime())
	in actionSet := update(!actionSet, (currentTime(),existing @ [act]))
	end handle NotFound =>
		  actionSet := insert(!actionSet, (currentTime(), [act]));
	zapFrom (currentTime() + 1);
	finalTime := infinity;
	lastException := NONE;
	act ())
    fun nextActions time = (* report next actions at a time >= arg *)
	(SOME(finds(!actionSet,time))) handle NotFound => NONE
  end


  val minTimeDelta = ref 100
  val maxTimeDelta = ref 500000
   
  fun gotoTime(time:int,error:int,basetime:int,
		stopOnEnd:bool,stopOnInterrupt:bool) =
     (* if error = 0, goto a specified time >= 0, unless stopped
	  by unconditional breakpoint;
	otherwise, go to closest stored time within error, if any exists; if not,
	  go to exact time as above.
	updates currentTime, etc. *)
    let	fun forceStop () =
	  case (hd o eventsFor o #evn) (!currentEvdata) of
	    ENDev _ => (currentTime() > !completedTime) andalso stopOnEnd
    	  | IOev => System.Unsafe.cast (hd (#args (!currentEvdata)))
		      andalso ((!HistoryIO.usebreak)())
	  | _ => false
    in
      if (not (time = currentTime())) then
       (if (!debugdebug) then
	  (print "gotoTime "; print time; print " "; print error; print "\n") 
        else ();
        let val p as (ptime,_) = findPrevState time
	    val sopt = if (error > 0) then 
			 findSuccState time
		       else NONE
	    val b as (btime,_) =
	      case sopt of 
		SOME(s as (stime,_)) => if (stime-time) < error andalso 
					 (stime-time) < (time-ptime) then
		  			  s
		  		        else p
	      | NONE => p
	    val b as (btime,bstate) = if (btime <= basetime) then
		  		        b
		 		      else findPrevState basetime
	in if (!debugdebug) then
	     (print "using best time "; print btime; print "\n")
	   else ();
	   if (not (currentTime() = btime)) then
	     restore b
	   else ();
	   if (time - btime > error)
	   then (* move forwards *)
	     (stoppedByException := false;
	      while (time > currentTime() 
		     andalso (not (!stoppedByException))
		     andalso (not (!interruptRaised andalso stopOnInterrupt))
		     andalso (not (forceStop()))) do
		  let val newTarget = min(
			    if (error = 0) andalso
			       ((time - currentTime()) > (!minTimeDelta))
			    then currentTime() + 
					((time-currentTime() - 1) div 2) + 1
			    else time, 
			    currentTime() + !maxTimeDelta)
		  in update(times,1,newTarget);  (* targetTime *)
		     callcc(fn cont => (debugCont := cont;
				        throw (!currentCont) ()));
		     (* here when we've reached target time or forced break *)
		     if (!maxstates = 0) then
			zapStates(!completedTime + 1)
		     else ();
		     saveCurrentState();
		     sizereport ("store " ^ makestring (currentTime()))
		  end)
	   else () (* already there, or near enough *)
	end;
        if (!debugdebug) then
	    (print "time "; print (currentTime()); 
	     print " event "; print (#evn (!currentEvdata)); 
	     print " last bind "; print (#lbt (!currentEvdata));
	     print "\n")
        else ())
      else 
        if (!debugdebug) then print "."
	else ();
      !currentEvdata
    end

  
  fun actToTime (time,error,basetimefn,stopOnEnd,stopOnInterrupt) =
        let fun act (SOME(nextTime,nextActs)) =
	         if nextTime <= time andalso
			 currentTime() < time - error then
	          (gotoTime(nextTime,0,basetimefn(),stopOnEnd,stopOnInterrupt);
		   if nextTime <> currentTime() then
		     debugPanic("actToTime wanted " ^ makestring nextTime ^
					" got " ^ makestring (currentTime()))
	 	   else ();
	           app (fn f => f()) nextActs;
	           act (nextActions(currentTime()+1)))
	         else ()
	      | act NONE = ()
        in act(nextActions(currentTime()+1));
	   gotoTime (time,error,basetimefn(),stopOnEnd,stopOnInterrupt)
        end

  fun advanceTime (time:int, error:int, capture:bool, stopOnIO:unit->bool) =
    (HistoryIO.silent := false;
     HistoryIO.usebreak := stopOnIO;
     stopOnException := capture;
     let val evd = actToTime (time,error,HistoryIO.nexttime,capture,true) 
     in if !interruptRaised andalso (not capture) then
	   (zap();
	    throw (!returnCont) INTERRUPTED)
        else evd
     end)

  fun restoreTime (time:int, error:int) =
    (HistoryIO.silent := true;
     HistoryIO.usebreak := (fn () => false);
     stopOnException := true;
     actToTime (time,error,fn()=>infinity,true,false) before
       (if abs (currentTime() - time) > error then
	 debugPanic ("resetTime wanted " ^ makestring time ^ 
			" got " ^ makestring (currentTime()))
       else ()))

  fun confirmTime () =
      (* A function for the nervous: make sure that we're properly restored
         to the current time, including all actions. *)
       let val p as (ptime,pstate) = findPrevState(currentTime())
       in if ptime = currentTime() then
	    restore p
	  else debugPanic ("confirmTime wanted " ^ makestring (currentTime()) ^
		                 " got " ^ makestring(ptime));
          case nextActions(currentTime()) of
	    SOME(nextTime,nextActs) => 
	      if nextTime = currentTime() then
		 app (fn f => f()) nextActs
	      else ()
          | NONE => ()
       end

  fun reset () =  (* includes responsibility for kernel times *)
    (update(times,0,0);  (* currentTime *)
     update(times,1,0);  (* targetTime *)
     currentEvdata := nullEvdata;
     finalTime := 0;
     completedTime := 0;
     initialTime := 1;
     running := false;
     currentCont := emptyUnitCont;
     debugCont := emptyUnitCont;
     interruptRaised := false;
     resetExecs();
     resetStates();
     initStorehandle := HistoryStore.remember();
     initIOhandle := HistoryIO.remember())
    
end (* structure DebugRun *)


