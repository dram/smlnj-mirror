(* cleanup.sml
 *
 * COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies.
 * COPYRIGHT (c) 1996 AT&T Research.
 *)

structure CleanUp : sig

    include CML_CLEANUP

    val clean : when -> unit

    val startServers    : unit -> unit
    val shutdownServers : unit -> unit

    val exportFnCleanup : unit -> unit

  end = struct

    datatype when = AtInit | AtInitFn | AtShutdown | AtExit
	(* The CML clean-up times are somewhat different than the SML/NJ
	 * times.
	 *
	 *	AtInit		initialization of a program that is being run
	 *			under RunCML.doit.
	 *	AtInitFn	initialization of a stand-alone program that was
	 *			generated by exportFn.
	 *	AtShutdown	normal program exit of a CML program running
	 *			under RunCML.doit.
	 *	AtExit		normal program exit of a stand-alone CML program.
	 *
	 * Note that the clean-up routines run while CML is still active.  It
	 * may also be useful for an application to register clean-up routines
	 * with SML/NJ (AtExportFn actions are the most useful).
	 *)

  (* at all times *)
    val atAll = [AtExit, AtShutdown, AtInit, AtInitFn]

    val hooks = ref ([] : (string * when list * (when -> unit)) list)

    local
      structure SV = SyncVar
      val lockV = SV.mVarInit ()
    in
    fun lock () = SV.mTake lockV
    fun unlock () =  SV.mPut(lockV, ())
    fun protect f x = if !Running.isRunning
	  then let
	    val _ = lock()
	    val res = (f x) handle ex => (unlock(); raise ex)
	    in
	      unlock (); res
	    end
	  else f x
    end (* local *)

  (* return the list of hooks that apply at when. *)
    fun filter when = let
	  fun f [] = []
	    | f ((item as (_, whenLst, _))::r) =
		  if (List.exists when whenLst) then item :: (f r) else (f r)
	  in
	    f (!hooks)
	  end

  (* apply the clean-up function for the given time.  In some cases, this
   * causes the list of hooks to be redefined.
   * NOTE: we reverse the order of application at initialization time.
   *)
    fun clean when = let
	  val _ = lock()
	  val cleanFns = (case when
		 of (AtInit | AtInitFn) => List.rev (filter (fn w => (w = when)))
		  | _ => filter (fn w => (w = when))
		(* end case *))
	  fun initFnPred AtExit = true
	    | initFnPred _ = false
	  fun doCleaner (_, _, f) = CML.select [
		  CML.joinEvt(CML.spawnc f when),
		  CML.timeOutEvt(Time.fromSeconds 1)
		]
	  in
	  (* remove uneccesary clean-up routines *)
	    case when
	     of AtInitFn => hooks := filter initFnPred
	      | _ => ()
	    (* end case *);
	    unlock();
	  (* now apply the clean-up routines *)
	    List.app doCleaner cleanFns
	  end

  (* find and remove the named hook from the hook list; return the hook
   * and the new hook list; if the named hook doesn't exist, then return NONE.
   *)
    fun removeHook name = let
	  fun remove [] = NONE
	    | remove ((hook as (name', whenLst, cleanFn)) :: r) =
		if (name = name')
		  then SOME((whenLst, cleanFn), r)
		  else (case (remove r)
		     of NONE => NONE
		      | SOME(hook', r') => SOME(hook', hook::r')
		    (* end case *))
	  in
	    remove (! hooks)
	  end

  (* add the named cleaner.  This returns the previous definition, or NONE. *)
    fun addCleaner (arg as (name, _, _)) = (case (removeHook name)
	   of NONE => (hooks := arg :: !hooks; NONE)
	    | (SOME(oldHook, hookLst)) => (
		hooks := arg :: hookLst; SOME oldHook)
	  (* end case *))
    val addCleaner = protect addCleaner

  (* remove and return the named cleaner; return NONE if it is not found *)
    fun removeCleaner name = (case (removeHook name)
	   of NONE => NONE
	    | (SOME(oldHook, hookLst)) => (
		hooks := hookLst; SOME oldHook)
	  (* end case *))
    val removeCleaner = protect removeCleaner

    exception Unlog

    datatype item = ITEM of {
	key : string,
	init : unit -> unit,
	shut : unit -> unit
      }

    val chanList = ref ([] : item list)
    val mboxList = ref ([] : item list)
    val serverList = ref ([] : item list)

    fun unlogItem l name = let
	  fun f [] = raise Unlog
	    | f ((x as ITEM{key, ...})::r) = if (name = key) then r else (x :: (f r))
	  in
	    l := f(!l)
	  end

    fun appInit l () = List.app (fn ITEM{init, ...} => init()) (List.rev (!l))

    fun unlogAll () = (chanList := []; mboxList := []; serverList := [])

    val unlogChannel = protect (unlogItem chanList)
    fun logChannel (name, ch) = let
	  fun f () = Channel.resetChan ch
	  in
	    (unlogChannel name) handle Unlog => ();
	    chanList := ITEM{key=name, init=f, shut=f} :: (!chanList)
	  end
    val logChannel = fn x => protect logChannel x

    val unlogMailbox = protect (unlogItem mboxList)
    fun logMailbox (name, mb) = let
	  fun f () = Mailbox.resetMbox mb
	  in
	    (unlogMailbox name) handle Unlog => ();
	    mboxList := ITEM{key=name, init=f, shut=f} :: (!mboxList)
	  end
    val logChannel = fn x => protect logChannel x

    val unlogServer = protect (unlogItem serverList)

    fun logServer (name, f, g) = (
	  (unlogServer name) handle Unlog => ();
	  serverList := ITEM{key=name, init=f, shut=g} :: (!serverList))
    val logServer = protect logServer

    val startServers = appInit serverList

    fun shutdownServers () = let
	  fun shut (ITEM{key, shut, ...}) = CML.select [
		  CML.joinEvt(CML.spawn shut),
		  CML.timeOutEvt(Time.fromSeconds 2)
		]
	  in
	    app shut (!serverList)
	  end

  (* clean the logged channels and mailboxes. *)
    fun cleanChannels _ = (appInit chanList (); appInit mboxList ())

    val _ = addCleaner ("Channels&Mailboxes", [AtInit,AtShutdown], cleanChannels)

  (* remove useless cleaners and clear the channel/mailbox logs
   * prior to exporting a stand-alone CML program.
   *)
    fun exportFnCleanup () = let
	  fun exportFnPred (AtInitFn | AtExit) = true
	    | exportFnPred _ = false
	  in
	    cleanChannels ();
	    chanList := []; mboxList := [];
	    hooks := filter exportFnPred
	  end

  end (* CleanUp *)

