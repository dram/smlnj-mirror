(* DebugMotions

   Support for breakpoints, motion forward/backward to nearest breakpoint, and
   more general motions.  Intended for use directly by user-mode interface or
   interactive system interface.

   These commands have a notion of established time/state; that is, the 
   time/state last reached by executing one of them.  Since the interactive
   system may allow instrumented user code to execute independently of 
   the debugger, so altering the current time/state, it is important to
   reset to the established time/state before doing anything else.  These
   commands all do that, and there is also a wrapping function suitable for
   use by other commands, e.g., those in DebugQueries. *)


signature DEBUG_MOTIONS = 
sig
  val runCompUnit: (unit -> 'a) -> 'a DebugExec.result
      (* Set thunk to run as next comp unit or interpolated unit. *)
  type time
  type place
  type wherewhen (* = place * time *)
  datatype 'a outcome = COMPLETED of 'a
                      | INTERRUPTED of 'a
		      | NOTRUNNING
  val jump: time -> wherewhen outcome (* to specified time *)
  val binSearch: (unit->time) * time  -> wherewhen outcome
      (* First argument evaluates a condition, returning
         the first time it is known to be true (infinity if it isn't so known).
	 Second argument is an absolute limit.  
	 Binary search forward to the first time when the condition
	 evaluates < infinity, or the limit is reached. *)
  val complete: unit -> unit outcome  (* returns only if NOTRUNNING *)
  val abort: unit -> unit outcome  (* returns only if NOTRUNNING *)
  val withEstablishedTime: (time->'a) -> 'a outcome
      (* Evaluate first arg, giving it established time as arg, and
         guaranteeing a return to established time when done. *)
  val keepEstablishedTime: (unit->'a) -> 'a outcome (* ?? *)
end

structure DebugMotions : DEBUG_MOTIONS =
struct
  open DebugExec DebugRun DebugUtil DebugStatic
  val establishedState: state ref = ref (zeroState())
                         (* ditto; the state we want it to appear to be *)

  fun establishState() = 
      (dbgprint ("*e " ^ makestring(currentTime()) ^ "\n");
       establishedState := currentState())

  (* All commands that assume current state/time
  should restoreState of establishedState before they start.
  All commands performing explicit time travel should keep 
  establishedState sensible as they complete.
  *)
  
  fun runCompUnit (f:unit -> 'a) : 'a result =
    if not(inCompUnit() andalso currentTime() = !finalTime) then
      let val result = (DebugSignals.setIntHand(); 
			setCompUnit f)
      in (case result of
	    SUSPENDED => ()
	  | _ => zapCache(currentTime()));
	 establishState();
	 DebugSignals.resetIntHand();  (* ignore possible interrupt *)
	 result
      end
    else INTERPOLATION  (* no-op *)
  
  (** All commands are responsible for polling interruptPending at sensible
  times, including but not limited to when they have just come back from
  a time-travel command, perhaps prematurely. 
  Moreover, commands are responsible for setting/clearing the 
  debugger's interrupt handler, so that it is set during polling periods. *)
  
  datatype 'a outcome = COMPLETED of 'a
                      | INTERRUPTED of 'a
		      | NOTRUNNING

  type wherewhen = place * time
    
  (* utility functions *)
  
  fun currentWhereWhen () = ((currentEvn(),0),currentTime())
  
  (* suitable for commands that may change established time. *)
  fun checkNotInterrupted f =
    if inCompUnit() then
      (DebugSignals.setIntHand();
       restoreState (!establishedState,QUIET);
       let val r = f()
       in DebugSignals.resetIntHand();
	  if !DebugSignals.pendingInterrupt then
	    INTERRUPTED r
	  else COMPLETED r
       end)
     else NOTRUNNING
  
  (* suitable for commands that do time-travel but don't want to change
     established time. *)
  fun keepEstablishedTime f =
    checkNotInterrupted (fn () => (f() before 
				   restoreState(!establishedState,QUIET)))

  (* suitable for invoking a function with the current established time. *)
  fun withEstablishedTime f =
    keepEstablishedTime (fn () => f(currentTime()))
  
  fun complete() : unit outcome =  (* returns only if NOTRUNNING *)
    checkNotInterrupted(completeCompUnit)
  
  fun abort() : unit outcome = (* returns only if NOTRUNNING *)
    checkNotInterrupted(abortCompUnit)
  
  fun jump (target:time) : wherewhen outcome =
    checkNotInterrupted(fn () =>
     let val target = min(max(target,!initialTime),!finalTime)
     in
       if target < currentTime() then
	 (resetTo target; 
	  (* if we were interrupted, pretend we went nowhere. *)
	  if target = currentTime() then
	    establishState()
          else restoreState(!establishedState,QUIET))
       else (advancePrecashTo (target,NOISY); (* possibly interrupted *)
	     establishState());
       currentWhereWhen()
     end)

  local val defInitDelta = 1000
  in
  fun binSearch(condition:unit->time,maxtime:time): wherewhen outcome =
       (* Evaluating condition on unit returns an upper bound on the time when
	  desired state has been reached -- infinity if no such bound yet known.
	  Execute forward until
	    - first time for which condition() < infinity, and/or
	    - maxtime is reached, and/or
	    - termination is reached, and/or
	    - interrupted. 
	  Will do quiet binary search for condition. *)
    checkNotInterrupted(fn () =>
      let val _ = dbgprint ("*bs " ^ (makestring maxtime) ^ "\n");
	  fun bcond () = condition() <= currentTime()
	  val inittime = currentTime()
	  val maxtime = min(maxtime,!finalTime)
	  fun narrow (ftime,ltime) =
	      (* N.B. There is improved theory on how to do this... *)
	   (dbgprint ("*nar " ^ (makestring ftime) ^ " " ^
		              (makestring ltime) ^ "\n");
	    if (ftime < ltime) then
	       (let val target = (ftime + ltime) div 2
		in resetTo target;
		   if !DebugSignals.pendingInterrupt then
		     () 
		   else
		     let val minc = condition()
		     in if minc < DebugUtil.infinity then
			  narrow (ftime,minc)
			else narrow (target+1,ltime)
		     end
		end)
	    else resetTo ltime)  (* interrupt may be raised *)
	  fun expand delta = 
	    let val startTime = currentTime()
		val target = min(currentTime() + delta,maxtime)
	    in dbgprint ("*exp " ^ (makestring startTime) ^ " " ^
			           (makestring target) ^ "\n");
	       advanceTo(target,BREAK bcond);
	       let val minc = condition()
		   val maxtime = min(maxtime,!finalTime)
	       in if !DebugSignals.pendingInterrupt then
		    ()
		  else if (minc < DebugUtil.infinity) then
		    narrow(startTime+1,minc)
		  else if (currentTime() < maxtime) then
		    expand((currentTime() - startTime) * 2 + 1)
		  else ()
	       end
	    end
	  val (ftime,ltime) = bracketCache bcond
      in
	dbgprint ("*bra " ^ (makestring ftime) ^ " " ^ 
		            (makestring ltime) ^ "\n");
	if maxtime < ftime then
	  advancePrecashTo (maxtime,NOISY)
	else 
	  (advanceTo (ftime,NOISY);
	   if ltime < infinity then 
	     expand(ltime-ftime)  (* stupid: covering possibility of BREAK *)
	   else
	     expand (defInitDelta));
	(* If an interrupt occured there may(?) be a small chance that we've
	   actually gone backwards! If so, pretend we went nowhere. *)
	if currentTime() >= inittime then
	  establishState()
	else
	  restoreState (!establishedState,QUIET);
	currentWhereWhen()
      end)
  
  end
  
  
end (* structure DebugMotions *)
