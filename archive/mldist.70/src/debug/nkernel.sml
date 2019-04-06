(* NO TIMES VERSION *)

(* DebugKernel

   Fundamentals of executing user code under debugger control.
   Handles transfer of control between debugger and user program,
   execution mode for user program, and "current state" of user
   program when it is stopped.

   This interface is used directly only by DebugExec and the various
   history-maintaining subsystems (Store,Io,Signals).
   Instrumented code also needs access to times and break; this 
   is provided via a special system ref, set in DebugInterface. *)


signature DEBUG_NKERNEL =  
sig
  (* Basic types *)
  type br (* = System.Unsafe.object array *)
  type evn (* = int *)

  val nullBr : br
  val currentBr: br ref
  type evData  (* = {evn:evn,lbr:br,args:System.Unsafe.object list} 
                     event number, last bind time, arguments *)
  (* Accessing elements of current state. *)
  val evDataAt: br -> evData
  val evnAt: br -> evn
  val lbrAt: br -> br
  val evnLbrAt: br -> evn * br
  val argsAt : br -> System.Unsafe.object list
  val evnArgsAt: br -> evn * System.Unsafe.object list
  val evnLbrArgsAt: br -> evn * br * System.Unsafe.object list


  (* Transfering control between debugger and user program *) 
  val setUserCont: unit cont -> unit
  val continue: unit -> unit
       (* Transfer control to user program. Returns when control transfered
	  back to debugger at break or end of program. *)
  val break: unit -> unit  
       (* Return control to debugger from instrumented code. *)
  val pendingInterrupt: bool ref
  val setIntHand:unit -> unit
  val resetIntHand:unit -> unit
end

structure DebugNKernel: DEBUG_NKERNEL =
struct
  open DebugUtil  DebugNStatic
  structure U = System.Unsafe
  type br = U.object array
  type evData = {evn:evn,lbr:br,args:System.Unsafe.object list} 
  val nullBr:br = array(2,U.cast 0)
  val _ = update(nullBr,1,U.cast nullBr)
  (* evDataAt nullBr should =
           {evn=0,lbr=nullBr,args=nil:System.Unsafe.object list}  *)
  val currentBr:br ref = ref nullBr
  
  val debugCont = ref (NONE:unit cont option)
  val userCont = ref (NONE:unit cont option)

  fun setUserCont c = userCont := SOME c
  fun continue() = 
      callcc (fn cont =>
	       (debugCont := SOME cont;
		case !userCont of
		  SOME ucont => throw ucont ()
		| NONE => debugPanic "Nkernel no user cont"))


  (* called from instrumented code *)
  fun break () =
      callcc(fn cont =>  (userCont := SOME cont;
			  case !debugCont of
			    SOME dcont => throw dcont ()
			  | NONE => debugPanic "NKernel no debug cont"))


  fun evDataAt (stuff:br) : evData =
      {evn=U.cast (stuff sub 0),
       lbr=U.cast (stuff sub 1),
       args=
       let fun makelist n =
	   (Array.sub(stuff,n)::(makelist (n+1)))
	   handle Subscript => nil
       in makelist 2
       end}

  val evnAt = #evn o evDataAt
  val lbrAt = #lbr o evDataAt
  val argsAt = #args o evDataAt
  fun evnLbrAt br = let val {evn,lbr,...} = evDataAt br in (evn,lbr) end
  fun evnArgsAt br = let val {evn,args,...} = evDataAt br in (evn,args) end
  fun evnLbrArgsAt br = let val {evn,lbr,args} = evDataAt br in (evn,lbr,args) end

  (* CTRL/C (SIGINT) handling.  This is immune from user interference.
     We take care to save the default handler to restore when not
     running under the debugger (perhaps we could get this statically,
     but it seems safer this way...) *)
  val pendingInterrupt = ref false
  local
    open System.Signals
    type handler = int * unit cont -> unit cont
    val setCnt = ref 0
    val normalHandOpt = ref (NONE:handler option)
    fun debugHand (cnt,cont) =
	(if not(!pendingInterrupt) then
	   (pendingInterrupt := true;
	    setAllEventBreaks true)
	 else ();
	 cont)
  in 
    fun setIntHand () =
	(inc setCnt;
	 if !setCnt = 1 then
	     (pendingInterrupt := false;
	      normalHandOpt := inqHandler(SIGINT);
	      setHandler(SIGINT,SOME(debugHand)))
	 else ())

    fun resetIntHand () =
	(case !setCnt of
	   0 => debugPanic "signals.resetIntHand"
	 | 1 => (setCnt := 0;
		 setHandler(SIGINT,!normalHandOpt))
	 | n => setCnt := n-1)
    end

end
