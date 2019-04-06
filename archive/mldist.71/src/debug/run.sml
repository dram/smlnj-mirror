(* DebugRun

   Manages state cache(s) and provides more convenient versions of some
   DebugExec functions; meant to be used together with DebugExec.

   At present, there is a single state cache.  In future, we may wish
   to separate out evn/lbt and possibly even arg info into a separate
   cache, since these data are cheaper to keep than the continuation data.
*)
   
signature DEBUG_RUN =
sig
  type time
  (* Motion functions.  These functions combine replay and record as 
     appropriate, so they can be used from any established state. *)
  val completeCompUnit: unit -> 'a    (* Does not return *)
  val advanceTo: time * DebugExec.onNoise -> unit          
      (* Advance to specified time, known or unknown. 
         Returns when target or terminating/noisy event reached, 
	 or on interrupt.
	 Caches state reached before returning. *)
  val advancePrecashTo : time * DebugExec.onNoise -> unit
      (* As advance (with option on onNoise mode), but also caches 
         intermediate states at exponentially decreasing intervals as
	 target time is approached. *)
  val minTimeDelta : int ref
  val maxTimeDelta : int ref
      (* Govern min and max intervals between caching for advancePrecashTo *)
  val resetTo: time -> unit

      (* Jump to known time, in past or future.
         Assumes onNoise mode is QUIET.
	 Caches state reached before returning. *)
  val resetPrecashTo: time -> unit
      (* As resetTo, but also caches intermediate states as advancePrecashTo *)
  val bracketCache: (unit->bool) -> (time * time)
      (* Searching forward from current time, return times of last cache entry
         for which condition is false, and first entry for which it is true
	 or infinity if no such. *)
  val zapRun: time -> unit
      (* Remove all cache and memo entries >= time. *)
  val maxStates: int ref (* in cache *)
  (* Event query functions.  On interrupt, they raise QueryInterrupted, but
     leave pendingInterrupt flag set. 
     These are here to allow alternative caching mechanisms to support them. *)
  exception QueryInterrupted
  type evn
  val evnAt: time -> evn
  val lbtAt: time -> time
  val evnLbtAt: time -> evn * time
  val argsAt : time -> System.Unsafe.object list
  val evnArgsAt: time -> evn * System.Unsafe.object list
  val evnLbtArgsAt: time -> evn * time * System.Unsafe.object list
  val memoLevel: int ref
  val rankBase : real ref
  val dumpCache: unit -> unit (* for debugging *)
end

structure DebugRun:DEBUG_RUN =
struct
  open Array List DebugUtil DebugStatic DebugExec
  infix 9 sub
  structure U = System.Unsafe
  val pendingInterrupt = DebugSignals.pendingInterrupt

  (* The memo table *)
  val memoLevel = ref 0  (* meaning: 0 = no memoization
			             1 = memoize evn,lbt
				     2 = memoize evn,lbt,args *)

  type entry = {evn:evn,lbt:time,args:U.object array option}
  exception NoMemo
  val memoMap:entry Intmap.intmap = Intmap.new(32,NoMemo)

  fun memoize (state:state) =
    if !memoLevel > 0 then
      let val time = #time state
	  val {evn,lbt,args} = #evData state
          val entry = 
	     if !memoLevel > 1 then
	       {evn=evn,lbt=lbt,args=SOME(arrayoflist args)}
	     else {evn=evn,lbt=lbt,args=NONE}
      in Intmap.add memoMap (time,entry)
      end
    else ()

  fun recall time = Intmap.map memoMap time

  fun zapMemo time = (* remove all memos for times >= arg *)
     let val z:time list ref = ref nil
	 fun addif (t,_) = if t >= time then z := t ::(!z) else ()
     in Intmap.app addif memoMap;
	app (Intmap.rmv memoMap) (!z)
     end

  (* The state cache *)
  structure StateSet = SortedSet(
    struct
      type t = state * int (* use number *)
      type k = int
      fun key ((s,_):t) = #time s
      val lt = Integer.<
    end)
  
  local 
    open StateSet
    val states = ref (new())
  in 
  
  val maxStates = ref infinity	(* maximum number of states to maintain *)
  val usenum = ref 0	(* use counter for maintaining LRU *)

  fun findPrevState time = 
    let val (pstate,_) = findp(!states,time)
    in inc usenum;
       states := update (!states, (pstate,!usenum));
       pstate
    end handle NotFound => zeroState()

  fun findSuccState time =
    let val (sstate,_) = finds(!states,time)
    in inc usenum;
       states := update(!states, (sstate,!usenum));
       SOME sstate
    end handle NotFound => NONE

  fun zapCache time  = (* remove all states for times >= arg *)
    let fun zap () =
      let val (s,_) = finds(!states,time)
      in states := delete(!states,#time s);
	 zap()
      end handle NotFound => ()
    in zap()
    end
  
  fun noMoreRoom () = (* decide whether there's room to cache another state *)
    let open System.Control.Runtime
    in ((!lastratio) < (!ratio * 100)) orelse (size(!states) >= !maxStates)
    end

  val rankBase = ref 1.5
      (* Should be >= 1.0.  The higher the number, the more important the
       LRU component.  At 1.0, the LRU component is ignored. *)

  fun eval (newtime:time) = (* return list of (time,worth) for current cache *)
    let val (timeuserepl,_) = revfold (!states,
				   (fn ((state,use),(acc,prev)) =>
				    let val t = #time state
					val repl = if t > newtime then
					    min(t-prev,t-newtime)
						   else t-prev
				    in ((t,use,t-prev)::acc,t)
				    end),
				   (nil,0))
	val timeuserepl = Sort.sort (fn ((_,u1,_),(_,u2,_)) => u1 > u2)
	                    timeuserepl
	(* list now in increasing order by usenum *)
	val maxn = real(List.length timeuserepl - 1)
	val lnrb = ln (!rankBase)
	fun worth(n,r) = exp((n-maxn) * lnrb) * (real r)
	fun evalit(nil,_) = nil
	  | evalit((t,u,r)::rest,n) = (t,worth(n,r))::(evalit(rest,n+1.0))
    in evalit(timeuserepl,0.0)
    end

  fun saveState (state:state) = 
    let val time = #time state
	fun doInsert () = 
	    (inc usenum;
	     (states := insert(!states,(state,!usenum))) 
	                handle DuplicateKey => ())
    in dbgprint ("*s " ^ (makestring time) ^ "\n");
       memoize state;
       if time = 0 then 
	 ()
       else
	 (if noMoreRoom() then
	    let val evaled =  eval time
		val (zaptime,zapworth) = List.fold (fn ((t,w),(mint,minw)) => 
					    if w < minw then 
					      (t,w) 
					    else (mint,minw)) evaled (~1,1E50)
	    in dbgprint ("*z " ^ (makestring zaptime) ^ 
			 " w " ^ (makestring zapworth) ^ "\n");
	       (states := delete(!states,zaptime)) handle NotFound => ();
	       if noMoreRoom() then 
		 ()
               else doInsert()
	    end
	  else doInsert());
       (!sizereport) ("*s" ^ (makestring time))
    end
	  
  fun dumpCache () =
    let val evaled = eval infinity
    in print "Cache contents:\nTime\tWorth\n";
       app (fn (t,w) => (print t; print "\t"; print w; print "\n")) evaled
    end

  end (* let open structure StateSet *)
  
  fun zapRun t = 
      (zapCache t;
       zapMemo t)

  fun saveCurrentState state : unit = saveState (currentState())
      (* N.B. doesn't displace existing state. *)

  fun restoreBestPrev (time,onNoise) =
      let val best = findPrevState time
      in if #time best > currentTime() then
	   restoreState(best,onNoise)
         else ()
      end

  (* Time-travel functions.  These operate on arbitary times, and
      hide current state, histories, and caches.
      They optionally implement pre-caching strategies, etc.
      All such policy decisions are at this level. *)

  fun completeCompUnit () =  (* doesn't return *)
      (dbgprint ("*com\n");
       restoreBestPrev (infinity,NOISY);
       if currentTime() < knownTime() then
	 (replayTo (knownTime(),NOISY);
	  if (!pendingInterrupt) then
	    interruptCompUnit()  (* does not return *)
	  else())
       else ();
       recordRest () (* does not return *))

  fun advanceTo (target:time,onNoise:onNoise) =
      (* Simplest possible version.  
         Returns when target or terminating event reached, or on interrupt. *)
      (dbgprint ("*adv " ^ (makestring target) ^ "\n");
       assert (target >= currentTime(),"Run.advanceTo");
       if currentTime() < target then
         ((case onNoise of
	     BREAK _ => ()  (* must re-execute here *)
	   | _ => restoreBestPrev (target,onNoise));
	  if currentTime() < target then
	    let val r = if currentTime() < knownTime() then
		          replayTo (min(knownTime(),target),onNoise)
			else true
	    in if r andalso not(!pendingInterrupt) 
		    andalso (currentTime() < target) then
		 (recordTo (target,onNoise); ())
	       else ();
	       saveCurrentState()
	    end
	  else ())
       else ())

  val minTimeDelta = ref 100
  val maxTimeDelta = ref 500000 (* must be > 0 *)

  fun advancePrecashTo (target:time,onNoise:onNoise) =
    (* This version does precashing, using halving intervals approach,
       with min/max delta constraints.
       Also takes onNoise options. 
       Intended use: moving forward to a specific time, from where we expect
          to perform undos.
       Returns when target or terminating event reached, or on interrupt. *)
    (dbgprint ("*advp " ^ (makestring target) ^ "\n");
     assert (currentTime() <= target,"Run.advancePrecashTo");
     let fun go () =
          let val newTarget = 
	        if (target-currentTime()) >= (!minTimeDelta) then
		  currentTime() + ((target-currentTime()-1) div 2 + 1)
		else 
		  target
	      val newTarget = min (newTarget,currentTime() + !maxTimeDelta)
              (* know currentTime() < newTarget *)
	      val r = if currentTime() < knownTime() then
		        replayTo (min(knownTime(),newTarget),onNoise)
      		      else true
	      val r' = if r andalso not(!pendingInterrupt) then
		         if currentTime() < newTarget then
			   recordTo(newTarget,NOISY)
			 else true
		       else false 
	  in saveCurrentState();
	     if r' andalso not(!pendingInterrupt) 
		   andalso currentTime() < target then
	       go()
	     else ()
	  end
     in
       if currentTime() < target then
           ((case onNoise of
	       BREAK _ => ()   (* must re-execute laboriously here *)
             | _ => restoreBestPrev (target,onNoise));
	    if currentTime () < target then
	      go ()
	    else ())
       else ()
     end)


  fun resetPrecashTo (target:time) =
      (* Reaches target or sets interruptPending.
         Does precashing, as in advancePrecashTo. *)
      (dbgprint ("*resetp " ^ (makestring target) ^ "\n");
       assert (target <= knownTime(),"Run.resetTo");
       if target <> currentTime() then
	 let val best = findPrevState target
	     fun go() =
		 let val newTarget = 
		       if (target-currentTime()) >= (!minTimeDelta) then
		         currentTime() + ((target-currentTime()-1) div 2 + 1)
		       else 
		         target
		     val newTarget = min (newTarget,
					  currentTime() + !maxTimeDelta)
		     (* know currentTime() < newTarget *)
		     val r = replayTo(newTarget,QUIET)
		 in saveCurrentState();
		    if r andalso not (!pendingInterrupt)
			 andalso currentTime() < target then
		      go()
                    else ()
                 end
	 in restoreState (best,QUIET);
	    if currentTime () < target then
	      go()
	    else ()
	 end
       else ())
  

  fun resetTo (target:time) =  (* simplest possible version *) 
      (* reaches target or sets interruptPending *)
      (dbgprint ("*reset " ^ (makestring target) ^ "\n");
       assert (target <= knownTime(),"Run.resetTo");
       if target <> currentTime() then
	 let val best = findPrevState target
	 in restoreState (best,QUIET);
	    if currentTime () < target then
	     (replayTo(target,QUIET); (* N.B. Ignore result. *)
	      saveCurrentState())
	    else()
	 end
       else ())
  
(*
  fun resetApproxTo (target:time, error:int)  (* just an idea *)
      (assert (target-error <= knownTime(),"Run.resetApproxTo");
       if abs(target-currentTime()) > error then
	 let val prev = findPrevState target
	     val prev_err = target - (#time prev)
	     val succ = findSuccState target
	     val succ_err = #time succ - target
	 in if succ_err < error andalso succ_err < prev_err then
	      restoreState (succ,QUIET)
	    else 
	      (restoreState (prev,QUIET);
	       if prev_err > error then
		 (replayTo(target-error,QUIET);
		  saveCurrentState())
	       else ())
	 end
       else ())

*)

  fun bracketCache (condition:unit -> bool) : time * time =
    (* Searching forward from current time, return times of last cache entry
       for which condition is false, and first entry for which it is true
       or infinity if no such.  This would be much cheaper as a special-purpose
       operator on eventTimes! *)
     (assert (not (condition()), "Run.bracketCache");
      (* a very primitive implemention: binary search would be better! *)
      let val oldState = currentState()
          fun loop t =
	   (case findSuccState(t+1) of 
	      NONE => (t,infinity)
	    | SOME ns => (restoreState(ns,QUIET);
			  let val nt = #time ns 
			  in
			    if condition() then 
			      (t,nt) 
			    else loop nt
			  end))
      in 
        (* saveCurrentState();  -- think this is unnecessary *)
        loop (currentTime())
	before
	restoreState (oldState,QUIET)
      end)


  (* checkResetTo is suitable for support functions that do time-travel
     without restoring, but are vulnerable to interrupts. *)
  exception QueryInterrupted
  fun enforce time: unit =
    if currentTime() <> time then
      (if (!pendingInterrupt) then
	 raise QueryInterrupted
       else debugPanic ("Run.enforce " ^ 
			(makestring time) ^ " " ^
			(makestring (currentTime()))))
    else ()
  fun checkResetTo time = (resetPrecashTo time; enforce time)
  
  fun evnAt (time:int) = 
      #evn(recall time)
       handle NoMemo => (checkResetTo time;
			 currentEvn())
  
  fun lbtAt (time:int) =
      #lbt(recall time)
       handle NoMemo => (checkResetTo time;
			 currentLbt())

  fun evnLbtAt (time:int) = 
      let val {evn,lbt,...} = recall time
      in (evn,lbt)
      end
       handle NoMemo => (checkResetTo time;
			 (currentEvn(),currentLbt()))

  fun argsAt (time:int) =
     (case #args (recall time) of
        SOME argarr => listofarray argarr
      | NONE => raise NoMemo)
       handle NoMemo => (checkResetTo time;
			 currentArgs())

  fun evnArgsAt (time:int) =
      let val {evn,args,...} = recall time
      in case args of
	   SOME argarr => (evn,listofarray argarr)
	 | NONE => raise NoMemo
      end
       handle NoMemo => (checkResetTo time;
			 (currentEvn(),currentArgs()))
 
  fun evnLbtArgsAt (time:int) =
      let val {evn,lbt,args} = recall time
      in case args of
	   SOME argarr => (evn,lbt,listofarray argarr)
	 | NONE => raise NoMemo
      end
       handle NoMemo => (checkResetTo time;
			 (currentEvn(),currentLbt(),currentArgs()))

end

