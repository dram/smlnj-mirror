functor SafePreCo(MP:MP) =
    struct
	open Array

	fun inc x = x := (!x)+1
	fun dec x = x := (!x)-1

	structure Unsafe_Queue = 
	    struct
		type '1a t = ('1a list ref * '1a list ref * int ref)
		fun create () = (ref [], ref [], ref 0)
		fun empty (f,r,s) = (f := []; r := [];
				     s := 0)
		fun enqf (f,r,s) x = (inc s;
				      f := x :: (!f))
		fun enqr (f,r,s) x = (inc s;
				      r := x :: (!r))
		fun deqf (f,r,s) = 
		    (case (!f) of
			 (hd::tl) => (dec s;
				      f := tl; SOME hd)
		       | [] => (case (rev (!r)) of
				    (hd::tl) => (dec s;
						 f := tl; r := [];
						 SOME hd)
				  | [] => NONE))
		fun deqr (f,r,s) = deqf(r,f,s)
		fun size (_,_,s) = !s
	    end

        structure U = System.Unsafe
(* 	open System.Timer *)

	val cfun = U.CInterface.c_function "SMLNJ-MP"
	fun cfun' s = (print ("looking for \""^s^"\"...");
		       cfun s
		       before
		       print (" found\n"))
	val cfun = cfun'

        fun array2(a,b): U.object array =
            let val c = array(2,U.cast a)
            in update(c,1,U.cast b);
               c
            end

	fun array3(a,b,c): U.object array = 
	    let val d = array(3,U.cast a)
	    in update(d,1,U.cast b);
               update(d,2,U.cast c);
	       d
	    end

 
        (********************)
        (* Thread Structure *)
        (********************)
	structure Thread = 
	    struct
                (************************************************)
                (* per-proc environments                        *)
		(************************************************)
		type env = U.object array
		    (* Environment slots are assigned as follows:
		        0  atomic flag
			1  procnum
		     *)
		val undefval : U.object = U.cast (ref 0)
		val atomic_slot = 0
		val procnum_slot = 1
		val env_size = 2
     	        fun make_env procnum = 
		    let val env = array(env_size,U.cast undefval)
		    in
			update(env,atomic_slot,U.cast true);
			update(env,procnum_slot,U.cast procnum);
			env
		    end

                (* Env handling. 
		   These routines are to be called only when atomic. *)
		fun get_env() : env = U.getvar()
		fun set_env env : unit = U.setvar env

		fun atomic () : bool = 
		    (U.cast(U.subscript(get_env(),atomic_slot)))

		fun interruptOK () = not(atomic())

      		(************************************************)
		(* atomicity                                    *)
		(************************************************)
		(****
		exception EnterAtomic
		exception LeaveAtomic
		*****)

		val checkInterruptStub: (unit -> unit) ref = ref (fn () => ())

		fun enterAtomic () = 
		    (*****
		    if (atomic ()) then raise EnterAtomic else
		    ******)
		    U.update(U.cast (get_env ()),atomic_slot,true)

		fun leaveAtomic () =
		    (*****
		    if (atomic ()) then 
		    *****)
			(U.update(U.cast (get_env ()),atomic_slot,false);
			 (!checkInterruptStub) ())
		    (*****
		    else raise LeaveAtomic
		     ****)

		(*******
		fun atomically f = 
		    (enterAtomic();
		     (f() handle exn => (leaveAtomic(); raise exn)) before
		     (leaveAtomic()))
		********)


		(************************************************)
	        (* procnums                                     *)
		(************************************************)
		fun procnum () : int = 
		    U.cast(U.subscript(get_env(),procnum_slot))


		(* for debugging *)
		val ioLock = MP.spin_lock ()
		fun safeIO id s =  
		    let val _ = MP.debuglock ioLock id
		    in
			print (Makestring.int (procnum())^": "^s^"\n");
			MP.unlock ioLock
		    end

		(************************************************)
	        (* Statistics                                   *)
		(************************************************)
	        val get_max_procs:unit->int = cfun "max_procs"
		val max_procs = get_max_procs()

		(************************************************)
		(* scheduling                                   *)
		(************************************************)
		datatype thread = THREAD of unit cont
		val run_queue : thread Unsafe_Queue.t vector = 
		    Vector.tabulate(max_procs,fn _ => Unsafe_Queue.create ())
		val run_queue_lock =
		    Vector.tabulate(max_procs,fn _ => MP.spin_lock())

		exception Deadlock

(**
		(*---------------------------------------------------------*)
		(* a call to setTimer sets SIGALRM to go off every msec    *)
		(* milliseconds.  Note that if msec=0 then the timer is    *)
		(* turned off.                                             *)
		(*---------------------------------------------------------*)
		local
		    open System.Timer
		    val setitimer = System.Unix.setitimer
		in
		    val timerInterval = ref 0 

		    fun setTimer msec = 
			(setitimer(0,TIME{sec=0,usec=1000*msec}, 
				   TIME{sec=0,usec=1000*msec});  
			 timerInterval := msec)             
		    fun noTimer () = 
			setitimer(0,TIME{sec=0,usec=0},TIME{sec=0,usec=0})
		end
**)

		(*---------------------------------------------------------*)
		(* N.B. All these routines must be executed atomically.    *)
		(*---------------------------------------------------------*)
		    
		fun thread k = THREAD k

	        (* Attempt to get a thread from pnum's runqueue using deq op.*)
                fun getnext (pnum,deq) =
		  let val r_q = U.subscriptv(run_queue,pnum)
		      val r_q_l = U.subscriptv(run_queue_lock,pnum)
		  in MP.lock r_q_l;
		     deq r_q before
		     MP.unlock r_q_l
		  end

                (* Attempt to steal a thread from another proc's run_queue.
		 * If not successful, back off and then try again. *)
	        val initial_count = ref 100;
		val garbage = ref ()
                fun pause 0 = ()
	          | pause n = pause(n-1)
		fun steal_thread my_pnum = 
		    let 
		      fun look count =
			let fun check pnum =
			        if pnum = my_pnum then
				    (garbage := pause count;
				     look count)
				else case (getnext (pnum,Unsafe_Queue.deqr)) of
				    SOME t => t
				  | NONE => check((pnum + 1) mod max_procs)
			in 
			    check ((my_pnum + 1) mod max_procs)
			end
		    in look (!initial_count)
		    end
				  
		fun steal_thread my_pnum = 
		    let 
		      fun look () =
			let fun check pnum =
			        if pnum = my_pnum then
				    look ()
				else case (getnext (pnum,Unsafe_Queue.deqr)) of
				    SOME t => t
				  | NONE => check((pnum + 1) mod max_procs)
			in 
			    check ((my_pnum + 1) mod max_procs)
			end
		    in look ()
		    end
				  
		fun switch () = 
		    let val pnum = procnum()
			val THREAD c =
		           case getnext(pnum,Unsafe_Queue.deqf) of
			       SOME t => t
			     | NONE => steal_thread pnum
		    in 
		       SMLofNJ.throw c ()
		    end

(*		fun switch () = switch ()
*)

                (* This is an optimized version of reschedule followed by
		 * getnext.  We avoid releasing the lock between the two 
		 * operations so that we can guarantee that a user-thread 
		 * is available.  Also, since we're both enqueueing and 
		 * dequeueing, there's no point in trying to wake any 
		 * other procs up.
		 *)
		fun reschedule_and_getnext thread = 
		    let val pnum = procnum()
			val r_q_l = U.subscriptv(run_queue_lock,pnum)
			val r_q = U.subscriptv(run_queue,pnum)
			val _ = MP.lock r_q_l
			val _ = Unsafe_Queue.enqr r_q thread
			(* There has to be a thread since we hold
			 * the lock and have just enqueued one.
			 *)
			val kont = 
			    case (Unsafe_Queue.deqf r_q) of
				SOME (THREAD c) => c
		    in
			MP.unlock r_q_l;
			kont
		    end

                (* Enqueue the thread on the run queue. *)
		fun reschedule thread =
		    let val pnum = procnum()
			val r_q_l = U.subscriptv(run_queue_lock,pnum)
			val r_q = U.subscriptv(run_queue,pnum)
		    in 
			MP.lock r_q_l;
			Unsafe_Queue.enqf r_q thread; 
			MP.unlock r_q_l
		    end

		(*---------------------------------------------------------*)
		(* The current thread gives up the cpu and hands it off to *)
		(* the next thread in the running queue.                   *)
		(*---------------------------------------------------------*)
		(* voluntary version *)
		fun yield () = 
		    (enterAtomic ();
		     (SMLofNJ.callcc (fn k =>
				      (SMLofNJ.throw (reschedule_and_getnext 
					      (thread k)) ())));
		     leaveAtomic ())


		(* Pre-emptive version, suitable for calling from signal
		   handler. Note we're guaranteed atomic here. *)
		fun yield_preempt (k:unit cont) : unit cont =
		    if (atomic()) then k else
			(SMLofNJ.callcc (fn c =>
				 (SMLofNJ.callcc (fn k' =>
						  (enterAtomic();
						   SMLofNJ.throw c 
						   (reschedule_and_getnext (thread k'))
						   ));
				  leaveAtomic();
				  SMLofNJ.throw k ())))

		(*---------------------------------------------------------*)
		(* Creation and termination of threads.                    *)
		(*---------------------------------------------------------*)
		fun exit () = (enterAtomic (); 
			       switch ())

		fun fork child =
		    (enterAtomic ();
		     SMLofNJ.callcc (fn parent => 
				     (reschedule (thread parent);
				      leaveAtomic();
				      (* not atomic at this point *)
				      child () handle exn =>
					  (print "uncaught exception ";
					   print (SMLofNJ.exnName exn);
					   print " raised in thread.\n");
					  exit()));
		     leaveAtomic ())

		(*---------------------------------------------------------*)
		(* Start up threads package                                *)
		(*---------------------------------------------------------*)
		exception StartUp
		fun startUp n = 
		    let fun start 0 = ()
			  | start n = 
			    (print ("started "^Makestring.int n^"\n");
			     if (MP.acquireProc (make_env n, switch)) then
				 start (n-1)
			     else (leaveAtomic ();
				   raise StartUp))
		  in 
		      enterAtomic ();
		      start n;
		      leaveAtomic ()
		  end

		val _ = ((* atomic here *)
			 set_env(make_env 0); 
			 leaveAtomic())

	    end	


        (*********************)	
	(* Control Structure *)
        (*********************)
        structure Control : 
	    sig 
		val setIntHandler: (unit cont -> unit cont) option -> unit
		val startUp : (int * int option) -> unit
	    end =
	    struct
(**
		(*---------------------------------------------------------*)
		(* Turns preemptive scheduling on and off by installing a  *)
		(* handler for SIGALRM and then calling setTimer.          *)
		(*---------------------------------------------------------*)
		local 
		    open System.Signals
			
		    val oldAlarmHandler = inqHandler SIGALRM 
			
		    fun alarmHandler(_,k) = Thread.yield_preempt k
		in
		    fun setPreempt (SOME msec) =
			(setHandler(SIGALRM,SOME(alarmHandler));
			 Thread.setTimer(msec);                  
			 ())               
		      | setPreempt NONE =                         
			(Thread.setTimer(0);    
			 setHandler(SIGALRM,oldAlarmHandler))
		end
**)
		(*---------------------------------------------------------*)
		(* Provides support for user interrupt handling.           *)
		(*---------------------------------------------------------*)
	        local 
		    open System.Signals
                    val oldIntHandler = inqHandler SIGINT
		    fun defaultUserHandler k = 
			case oldIntHandler of
			    SOME h => h (1,k)
			  | NONE => k

		    val userHandler : (unit cont -> unit cont) ref = 
			ref (defaultUserHandler)

		    (* N.B. This is treated like an atomic ref, which
		       should be ok. *)
		    val interruptPending = ref false

		    fun intHandler (_,k) = 
			if Thread.interruptOK() then
			    (!userHandler) k
			else (interruptPending := true;
			      k)

		    fun checkInterrupt () = 
			(* to be called when an interrupt is convenient *)
			if !interruptPending then
			    SMLofNJ.callcc (fn k =>
					    (interruptPending := false;
					     maskSignals true;
					     let val k' = (!userHandler) k
					     in maskSignals false;
						 SMLofNJ.throw k' ()
					     end))
			else ()
		in 
		    val _ = Thread.checkInterruptStub := checkInterrupt
		    fun setIntHandler (SOME handler) =
			(userHandler := handler;
			 interruptPending := false;
			 setHandler (SIGINT, SOME intHandler))
		      | setIntHandler NONE = 
			(setHandler (SIGINT,oldIntHandler);
			 interruptPending := false;
			 userHandler := defaultUserHandler)
		end

		fun startUp (procs, preempt) = 
		    ( (** setPreempt preempt; **)
		     Thread.startUp procs)

	    end (* structure Control *)
    end (* structure SafePreCo *)




