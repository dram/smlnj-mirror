(* DebugSignals

   Support "historical" signals and ctrl/c handling.
*)

signature SIGNALS =
  sig
    datatype signal
      = SIGHUP | SIGINT | SIGQUIT | SIGALRM | SIGTERM | SIGURG
      | SIGCHLD | SIGIO | SIGWINCH | SIGUSR1 | SIGUSR2
      | SIGTSTP | SIGCONT (* not yet supported *)
      | SIGGC
    val setHandler : (signal * ((int * unit cont) -> unit cont) option) -> unit
    val inqHandler : signal -> ((int * unit cont) -> unit cont) option
    val maskSignals : bool -> unit
    val pause : unit -> unit
	(* sleep until the next signal *)
  end

signature DEBUG_SIGNALS = 
sig
  (* user-level functions *)
  include SIGNALS 
  (* debugger-control functions *)
  val enableSignals : bool ref
  val remember: unit -> DebugKernel.doers
  val nextAction: DebugKernel.time -> DebugKernel.action
  val handleSignals: DebugKernel.transform
      (* called when it is convenient to process pending signals *)
  (* CTRL/C handling *)
  val pendingInterrupt: bool ref
  val setIntHand: unit -> unit
  val resetIntHand: unit -> unit
end

structure DebugSignals : DEBUG_SIGNALS =
struct
  open Array List DebugUtil DebugKernel
  infix 9 sub
  structure S = System.Signals
  open S
  val nsigs = 14
  exception UnimplementedSignal

  (* Convert SML signal names to run-time signal codes.  
     For convenience, these should agree with S.Signals. *)
  fun sig2code SIGHUP    = 0
    | sig2code SIGINT    = (* 1 *) raise UnimplementedSignal
    | sig2code SIGQUIT   = 2
    | sig2code SIGALRM   = 3
    | sig2code SIGTERM   = 4
    | sig2code SIGURG    = 5
    | sig2code SIGCHLD   = 6
    | sig2code SIGIO	 = 7
    | sig2code SIGWINCH  = 8
    | sig2code SIGUSR1   = 9
    | sig2code SIGUSR2   = 10
    | sig2code SIGTSTP   = (* 11 *) raise UnimplementedSignal
    | sig2code SIGCONT   = (* 12 *) raise UnimplementedSignal
    | sig2code SIGGC	 = 13

  fun code2sig 0  = SIGHUP
    | code2sig 1  = SIGINT
    | code2sig 2  = SIGQUIT
    | code2sig 3  = SIGALRM
    | code2sig 4  = SIGTERM
    | code2sig 5  = SIGURG
    | code2sig 6  = SIGCHLD
    | code2sig 7  = SIGIO
    | code2sig 8  = SIGWINCH
    | code2sig 9  = SIGUSR1
    | code2sig 10 = SIGUSR2
    | code2sig 11 = SIGTSTP
    | code2sig 12 = SIGCONT
    | code2sig 13 = SIGGC
    | code2sig _ = debugPanic "signals.code2sig"

  type handler = (int * unit cont) -> unit cont

  (* the user's handlers *)
  val sigvec = array(nsigs,NONE:handler option)

  (* Our simulation of masking. Note that since SIGINT must never 
     be masked indefinitely, we don't use S.maskSignals here. *)
  local
    val maskLevel = ref 0  
  in
    fun maskSignals true = inc maskLevel
      | maskSignals false = if !maskLevel > 0 then dec maskLevel else ()
    fun saveLevel () = !maskLevel
    fun resetLevel newLevel = maskLevel := newLevel
    fun masked() = !maskLevel > 0
  end

  (* number of signals of each type pending. *)
  val pendvec = array(nsigs,0)  

  (* flag: should user handlers be active? *)
  val enableSignals = ref false

  (* flag: are we executing a user signal handler now? *)
  val inHandler = ref false

  (* handler we actually install with "real" signal package. *)
  (* N.B. It would be nice to do something about IO here; i.e., to
     emulate the approach taken by the lower level to allow IO
     waits to be interruptible.  Unfortunately, I don't see any way
     to do this without exposing/simulating the inards of the IO package. *)
  fun trueHandler code =
        fn (cnt,cont) =>
	   (update(pendvec,code,pendvec sub code + cnt);
	    if !enableSignals 
		andalso (not(masked())) 
		andalso (not(!inHandler)) then
	         setTargetTime (currentTime() + 1)  (* idempotent *)
	    else ();
	    cont)

  fun setHandler (signal,handler) = 
       let val code = sig2code signal (* may raise *)
       in update(sigvec,code,handler);
	  (case handler of
	     SOME h =>
	       S.setHandler(signal,SOME (trueHandler code))
	   | NONE => 
	       S.setHandler(signal,NONE))
	end

  fun saveHandlers () =  copyarray sigvec

  fun resetHandlers (newsigvec:handler option array) =
      let fun loop n =
	    let val newh = newsigvec sub n
	    in setHandler (code2sig n,newh)
	                       handle UnimplementedSignal => ();
	       loop (n+1)
	    end handle Subscript => ()
      in loop 0
      end

  fun inqHandler signal = sigvec sub (sig2code signal)

  fun pause () =
      (* N.B. This doesn't work very well wrt/atomicity, but neither
         does the built-in version! *)
    case (!execMode) of
      RECORD _ => S.pause ()
    | REPLAY _ => ()
    | STOP => S.pause ()

	  
  type entry = {code:int,cnt:int}
  structure Log = TimedLog (type entry = entry) 
  val mark = Log.new()

  fun remember() =
    let val savedMark = Log.copyMark mark
	val savedHandlers = saveHandlers()
        val savedLevel = saveLevel()
	fun reset _ = 
	    (Log.resetMark mark savedMark;
	     resetHandlers savedHandlers;  (*  AVOID BUG !! *)
	     resetLevel savedLevel)
    in {redo=reset,undo=reset}
    end

  fun nextAction (time0:time) : time * transform = 
    let val (time,{code,cnt}) =
	  (* we advance lazily here, since it's hard to know when our
	     result is used. *)
	  let fun loop () = 
	      let val (time,entry) = Log.next mark
	      in if time < time0 then
		  (Log.advance mark;
		   loop())
		 else (time,entry)
	      end   
	  in loop()
	  end
	fun transform oldCont =
	    callcc (fn rcont => 
		     (callcc (fn cont => throw rcont cont);
		      throw (case sigvec sub code of
			       SOME userHandler => userHandler(cnt,oldCont)
			     | NONE => oldCont) ()))
    in (time,transform)
    end handle Log.Log => (infinity,fn cont => cont)

  local
    fun handleNext cont : unit cont =
      let val nextPending =
	    if masked() orelse (!inHandler) then
	      NONE
	    else
	      let fun loop code = 
		    let val cnt = pendvec sub code
		    in if cnt > 0 then
			 (update (pendvec,code,0);
			  SOME (code,cnt))
		       else loop (code+1)
		    end
	      in S.maskSignals true;
		 loop 0 handle Subscript => NONE
		 before S.maskSignals false
	      end
      in case nextPending of
	   SOME (code,cnt) => 
	      (Log.append mark (*currenttime*) {code=code,cnt=cnt};
	       inHandler := true;
	       let val cont' = 
		      case sigvec sub code of
			SOME userHandler => userHandler(cnt,cont)
		      | NONE => cont
	       in inHandler := false;
		  handleNext cont'
	       end)
	 | NONE => cont
      end
  in
    fun handleSignals oldCont : unit cont =
      callcc (fn rcont => (callcc (fn cont => throw rcont cont);
			   throw (handleNext oldCont) ()))
  end


  (* CTRL/C (SIGINT) handling.  This is immune from user interference.
     We take care to save the default handler to restore when not
     running under the debugger (perhaps we could get this statically,
     but it seems safer this way...) *)
  val pendingInterrupt = ref false
  local
    val setCnt = ref 0
    val normalHandOpt = ref (NONE:handler option)
    fun debugHand (cnt,cont) =
	(if not(!pendingInterrupt) then
	   (pendingInterrupt := true;
  	    setTargetTime (currentTime() + 1))
	 else ();
	 cont)
  in 
    fun setIntHand () =
	(inc setCnt;
	 if !setCnt = 1 then
	     (pendingInterrupt := false;
	      normalHandOpt := S.inqHandler(SIGINT);
	      S.setHandler(SIGINT,SOME(debugHand)))
	 else ())

    fun resetIntHand () =
	(case !setCnt of
	   0 => debugPanic "signals.resetIntHand"
	 | 1 => (setCnt := 0;
		 S.setHandler(SIGINT,!normalHandOpt))
	 | n => setCnt := n-1)
    end
end

