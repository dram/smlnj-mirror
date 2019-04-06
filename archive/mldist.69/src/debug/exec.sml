(* DebugExec

   This module supports basic user code execution. It coordinates
   and hides the functions of DebugKernel and the various history-maintaining
   subsystems (static,io,signals,exec). Provides for:
   - querying current state when stopped
   - initializing instrumented user code for execution under debugger control.
   - basic record and replay operations

   This module can be used directly by higher-level commands, or via
   DebugRun (q.v.).

*)

signature DEBUG_EXEC =
sig
  type time
  datatype onNoise = QUIET | NOISY | BREAK of (unit->bool)
      (* as in DebugKernel *)
  datatype 'a result =
      NORMAL of 'a
    | EXCEPTION of exn
    | ABORTED 
    | INTERRUPTED
    | SUSPENDED
    | INTERPOLATION
      (* possible results of executing a thunk of user code. *)
  val initialTime : time ref
      (* time of STARTev for current outermost comp unit. *)
  val finalTime: time ref
      (* time of termination of currently outermost comp unit, or
         infinity if unknown. *)
  val blockingExn: exn option ref
      (* exception blocking current outermost comp unit, if any. *)
  (* State manipulation functions *)
  type state (* ={time:time,
	          cont:userCont,
		  evData:evData,
		  transform:transform,
		  depth:int,
		  memories:doers list (* for each history *)} *)
  val currentState : unit -> state
  val restoreState: (state * onNoise) -> unit
  val zeroState: unit->state  (* the absolutely initial state *)

  (* Querying elements of current state.  These are defined for convenience. *)
  val currentTime: unit -> time
  val currentEvn: unit -> DebugStatic.evn
  val currentLbt: unit -> time
  val currentArgs: unit -> System.Unsafe.object list
  val knownTime: unit -> time

  (* Thunk initialization and premature termination. *)
  val setCompUnit: (unit -> 'a) -> 'a result
  val abortCompUnit: unit ->  'a
  val interruptCompUnit: unit -> 'a 

  (* Execution functions *)
  val recordTo: (time * onNoise) -> bool
  val recordRest: unit -> 'a 
  val replayTo: (time * onNoise) -> bool

  (* Query functions *)
  val inCompUnit: unit -> bool

end

structure DebugExec : DEBUG_EXEC =
struct
  open DebugUtil DebugStatic DebugKernel DebugSignals
  (* CompUnits include both normal and interpolated compilation units. *)
  structure Log = TimedLog(type entry = transform)
  val compUnitMark = Log.new()
  val zeroCompUnitMark = Log.copyMark compUnitMark
  val _ = (Log.append compUnitMark (fn cont=>cont)  (* at time 0 *) ;
	   Log.resetMark compUnitMark zeroCompUnitMark)
  fun nextCompUnitAction (time0:time) : action =
	(* we advance lazily here, since it's hard to know when our
	     result is used. *)
      let fun loop () = 
	    let val (time,entry) = Log.next compUnitMark
	    in if time < time0 then
	         (Log.advance compUnitMark;
		  loop())
	       else (time,entry)
	    end   
      in loop()
      end handle Log.Log => (infinity,fn cont =>cont)
  fun rememberCompUnits () =
      let val savedMark = Log.copyMark compUnitMark
	  fun reset _ = Log.resetMark compUnitMark savedMark
      in {redo=reset,undo=reset}
      end

  (* Master list of actions *)
  val nextActions = [DebugSignals.nextAction,
		     nextCompUnitAction]


  (* Master list of subsystem states *)
  val remembers =
      [DebugStatic.rememberEventTimes,
       DebugStore.remember,
       DebugIO.remember,
       DebugSignals.remember, 
       rememberCompUnits]

  val pendingTransform:transform option ref = ref NONE
  val initialTime : time ref = ref 0
  val blockingExn : exn option ref = ref NONE
  val finalTime: time ref = ref 0
  val compUnitDepth: int ref = ref 0
  fun inCompUnit () = (!compUnitDepth > 0)

  (* Current state.
     (The implementation at this level is very stateful, because some
     of the history mechanisms must deal with a large current state,
     and it would be quite inefficient to copy this in and out 
     all the time in order to maintain functional cleanliness. *)

  (* Clients may wish to separate out elements of, e.g., evData,
     for efficiency. *)
  type state ={time:time,
	       cont:userCont,
	       evData:evData,
	       transform:transform option,
	       depth:int,
	       memories:doers list (* for each history *)}

  fun currentState () : state =
      {time=currentTime(),
       cont=userCont(),
       evData=currentEvData(),
       transform= !pendingTransform,
       depth= !compUnitDepth,
       memories=map (fn remember => remember ()) remembers}

  val basicZeroState = currentState()
  fun zeroState () =
      let val {time,cont,evData,depth,memories,...} = basicZeroState
      in {time=time,cont=cont,evData=evData,depth=depth,memories=memories,
	  transform=let val (_,tr) = Log.next zeroCompUnitMark
	            in SOME tr
		    end}
      end

  (* N.B. This must work correctly even when applied to an illicit current
     state (later than the known time). *)
  (*  -- some further checks on legitimacy of the new state might be nice... *)
  fun restoreState ({time,cont,evData,memories,transform,depth},
		    onNoise:onNoise): unit =
    (dbgprint ("*r " ^ (makestring time) ^ "\n");
     let val oldTime = currentTime() 
     in
       if time <> oldTime then
	 (if oldTime < time then
	    app  (fn ({redo,...}:doers) => redo onNoise) memories
	  else app (fn ({undo,...}:doers) => undo onNoise) memories;
	  setCurrentTime time;
	  transformUserCont (fn _ => cont);
          setCurrentEvData (evData);
	  pendingTransform := transform;
	  compUnitDepth := depth)
       else ()
     end)

  (* Versions of kernel routines augmented to do signals. *)
  fun ignore () =
      (enableSignals := false;
       DebugKernel.ignore())

  fun continue (target:time, mode:execMode, signals:bool) =
      (case !pendingTransform of
	 SOME t => (dbgprint ("*tr " ^ (makestring (currentTime())) ^ "\n");
		    transformUserCont t;
		    (case mode of
		      RECORD _ => Log.append compUnitMark t
		     | _ => ()))
       | NONE => ();
       enableSignals := signals;
       DebugKernel.continue (target,mode);
       pendingTransform := NONE)

  (* Setting up thunks to run. *)
  datatype 'a result =
      NORMAL of 'a
    | EXCEPTION of exn
    | ABORTED 
    | INTERRUPTED
    | SUSPENDED
    | INTERPOLATION

  val currentTime = DebugKernel.currentTime
  fun currentEvn () = #evn(currentEvData())
  fun currentLbt () = #lbt(currentEvData())
  fun currentArgs () = #args(currentEvData())
  val knownTime = DebugKernel.knownTime

  val resultCont: 'a result cont ref = System.Unsafe.cast (ref (makeCont()))
  val lastState : state ref = ref (zeroState())

  fun reset() = 
      (compUnitDepth := 0;
       restoreState(!lastState,QUIET);
       resetKnownTime();
       finalTime := knownTime())

  (* Routine setCompUnit sets up instrumented code to run, by modifying the
     continuation in the current state. 
     The argument is a thunk of instrumented code.
     Action depends on whether there is no comp unit set to run (depth = 0),
     in which case this will be an outer-level comp unit, or there are
     one or more units set (depth > 0), in which case  this will be a
     interpolated comp unit.
     Outer-level units:
     For these units, routine returns twice:
     - after setting up the code to run, returns with result SUSPENDED.
       At this point, the other exec routines can be run to record,replay,etc.
     - after recordRest() or abortCompUnit() has been successfully called,
       setCompUnit returns again with result NORMAL,EXCEPTION,SUSPENDED,
       or ABORTED, as appropriate.  On abnormal returns, the current
       state and time will be reset to what they were before the call to
       setCompUnit. It is the caller's responsibility to destroy any 
       copies of the state that refer to an abnormally terminated unit.
     Interpolated units:
     For these units, setCompUnit returns only once, with result INTERPOLATION.
     The interpolated code is set to run starting at the current time. When
     this thunk finishes normally, the previous code will be resumed.
     Note that setting an interpolation removes all previous interpolations set
     at the current or any later time.  It is the caller's responsibility to
     destroy any copies of the state that refer to any later time.
     On abnormal results the current state and time will be reset to what 
     they were before the *outer-level call* to setCompUnit, and the
     *outer-level* abnormal return will be made immediately. *)
  fun setCompUnit (f:unit -> 'a) : 'a result =
     let fun transform oldCont =
	   callcc (fn rcont => 
		   (callcc (fn cont => throw rcont cont);
		    let val result = (inc compUnitDepth; f())
		    in dec compUnitDepth;
		       if inCompUnit() then
			 throw oldCont ()
		       else 
			 (advanceKnownTime();
			  finalTime := knownTime();
			  ignore();
			  throw (!resultCont) (NORMAL result))
		    end handle exn => 
			(pseudoEvent{evn=pseudoEvn UNCAUGHTev,
				     forced=true,
				     args=[System.Unsafe.cast exn]};
			 reset();
			 throw (!resultCont) (EXCEPTION exn))))
	val nested = inCompUnit()
     in dbgprint ("*init " ^ (makestring (!compUnitDepth)) ^ "\n");
	if nested then
	  (assert (currentTime() < !finalTime,"Exec.setCompUnit 1");
	   resetKnownTime())
	else 
	  lastState := currentState();
	pendingTransform := SOME transform;
	finalTime := infinity;
	blockingExn := NONE;
	(if nested then
	   INTERPOLATION
	 else
	   (callcc (fn cont => (resultCont := cont;
				initialTime := currentTime() + 1;
				continue(!initialTime,RECORD QUIET,false);
				assert (knownTime() = !initialTime,
					"Exec.setCompUnit 2");
				(* N.B. Must be normal result, ignoring
				 possible interrupt. *)
				SUSPENDED))))
	before ignore()
     end

  fun abortCompUnit() = 
      (reset();
       throw (!resultCont) ABORTED)

  fun interruptCompUnit() =
      (reset();
       throw (!resultCont) INTERRUPTED)

  (* Fundamental execution functions. *)
  (* Return conditions:
     When we return from recordTo or replayTo, at *least* one of 
     the following will be true:
      (a) We reached target, in which case currentTime = target.
      (b) We hit end of program, in which case atEndOfProgram()
            returns true. 
      (c) We hit an uncaught exception, in which case atException() 
            returns true.
      (d) We hit a noise event with onNoise=BREAK (true condition), 
            in which case atNoise() returns true.
      (e) CTRL/C was hit, in which case pendingInterrupt will be true. 
     Note that (b),(c), and (d) are mutually exclusive, but (a) and
     (e) can occur in combination with any of the others and/or each
     other. Note also that on replayTo, (b) and (c) can be true only
     if the target argument = knownTime(). 

     The distinctions among (b),(c),(d) are important for setting
     finalTime and blockingExn, but are not generally of interest to
     callers.  So for right now, we return just a boolean, which will
     be false iff one of (b),(c),(d) occured.  Callers must still
     check for (e) by polling pendingInterrupt.
    
     The following datatype is used internally, and may prove useful
     externally in future. *)

  datatype execResult = 
      EXEC_NORMAL | EXEC_END | EXEC_EXN of exn | EXEC_NOISE

  (* Analyse current state immediately after returning from execution.
     Not for export. *)
  fun execResult onNoise : execResult =
      case (hd o eventsFor) (currentEvn()) of
	ENDev _ => 
	  if (knownTime() = currentTime())   (* avoiding old ENDevs *)
	      andalso (!compUnitDepth = 1)   (* avoiding interpolated ENDevs*)
	  then EXEC_END
	  else EXEC_NORMAL
      | UNCAUGHTev => EXEC_EXN (System.Unsafe.cast (hd(currentArgs())))
      | IOev => (case onNoise of
	          BREAK condition => if condition() then 
		                       EXEC_NOISE
				     else EXEC_NORMAL
	 	 | _ => EXEC_NORMAL)
      | _ => EXEC_NORMAL

  fun recordTo (target:time,onNoise:onNoise) : bool =
      (* Record up to target time, unless something else intervenes.
         For possible conditions on return, see above.
         Include an initial check of signals.
	 Any pending signals are handled before we start execution.
	 If a signal is raised in the course of execution, we will stop
	 at the next clock tick, handle it, and continue -- unless
	 we stop at that clock tick for some other reason, in which 
	 case the signal remains pending. *)
      (dbgprint ("*rec " ^ makestring(target) ^ "\n");
       (* assert (inCompUnit(),"Exec.recordTo"); *)
       assert(currentTime() = knownTime(),"Exec.recordTo 2");
       assert(target > knownTime(),"Exec.recordTo 3");
       let fun go() = 
	 if currentTime() < !finalTime then
	   (transformUserCont handleSignals;
	    continue(target,RECORD onNoise,true); (* jump into user program *)
	    (* see why we stopped *)
            case execResult onNoise of
	      EXEC_END =>
		     (finalTime := knownTime(); false)
	    | EXEC_EXN exn => 
		     (finalTime := knownTime(); blockingExn := SOME exn; false)
	    | EXEC_NOISE => false
	    | EXEC_NORMAL =>
		     if target = knownTime() orelse !pendingInterrupt then
		       true
		     else  (* presumably false noise break or
			      pending signals... *)
		       go())
	 else false
       in go() before
	  ignore()
       end)

  fun recordRest ()  =
      (* Record remainder of compilation unit, jumping back to caller
         of the unit when done.
         This function does not return. 
         Deals with signals internally. *)
      (dbgprint ("*recr\n");
       (* assert (inCompUnit(),"Exec.recordRest 1"); *)
       assert(currentTime() = knownTime(),"Exec.recordRest 2");
       (* assume associated state appropriately set too *)
       let fun go() =
         (transformUserCont handleSignals;
	  continue(infinity,RECORD NOISY,true); (* jump into user program *)
          if !pendingInterrupt then
	    interruptCompUnit() (* does not return *)
	  else go())
       in go() (* does not return *)
       end)
    
  fun replayTo (target:time,onNoise:onNoise) : bool =
    (* For possible conditions on return, see above.
       If none of these conditions are met and we cannot reach 
       target time for any reason, raise an exception.
       We handle actions internally. *)
      (dbgprint ("*repl " ^ makestring(target) ^ "\n");
       (* assert (inCompUnit(),"Exec.replayTo 1"); *)
       assert (target <= knownTime(),"Exec.replayTo 2");
       assert (target > currentTime(),"Exec.replayTo 3");
       (* assume state for currentTime appropriately set too *)
       let 
	 fun go t =
	     if currentTime() < !finalTime then
	       continue(t,REPLAY onNoise,false) (* jump into user program *)
	     else ()
	 fun minAction nil = (infinity,fn c => c)
	   | minAction ((time,transform)::rest) =
	        let val (time',transform') = minAction rest
		in if time < time' then (time,transform)
		   else (time',transform')
		end
	 fun nextAction time0 =
	     minAction (map (fn a => a time0) nextActions)
         fun loop () =
	     let val (actTime,transform) = nextAction (currentTime() + 1)
	     in dbgprint ("*act " ^ (makestring (currentTime())) ^ " " ^
			             (makestring actTime) ^ "\n");
		(if actTime < target then
		   let fun go' () = 
		     (go actTime;
		      case execResult onNoise of
                        EXEC_NORMAL => 
			  if !pendingInterrupt then
			    true
			  else if currentTime() < actTime then
			    go'()
			  else
			    (dbgprint ("*trans " ^ (makestring (currentTime())) ^ "\n");
			     assert(currentTime() = actTime, 
			            "Exec.replayTo.loop " ^ 
				     (makestring (currentTime())));
			     pendingTransform := SOME transform;
			     loop ())
   		      | _ => false)
		   in go' ()
		   end
		 else
		   let fun go' () =
		     (go target;
		      case execResult onNoise of
		        EXEC_NORMAL => 
			   if not(!pendingInterrupt) andalso 
			       currentTime() < target then
			     go'() (* failed BREAK *)
			   else true
		      | _ => false)
		   in go'()
		   end)
	        before (if actTime = currentTime() then 
			  pendingTransform := SOME transform
			else())
	     end
       in loop() before
          ignore
       end)
end
