(* User-level debugger code. *)
structure UserDebugCommands = struct
 open System.Control.Debug
 open UserDebugUtil UserDebugInterface UserDebugBreaks UserDebugEmacs

 val _ = debugging := true

 (* Selection functions *)

 fun selectCurrent () =
 (* Command run by M-c in emacs.  Select the current event. *)
     emacsSelect (!currentEv)

 fun selectBackTrace () =
 (* Select the backtrace event, or the current event if there is no
  * backtrace event.  Not currently used; perhaps this should be bound
  * to some key? *)
     emacsSelect (case !backtrace of
		      SOME (_, (ev, _)) => SOME ev
		    | NONE => !currentEv)

 fun selectNear filename cp =
 (* Command run by M-e in emacs.  Select an event near the given character
  * position. *)
     emacsSelect
     (SOME (hd (ZeventsAfterLocation
		(if filename = instreamBufferName
		     then instreamName else filename, cp)))
      handle Hd => NONE)

 fun selectNext () =
 (* Command run by M-n in emacs.  Select the next event. *)
     case ofSome(nextEvent, !selected) of
	 SOME e => emacsSelect (SOME e)
       | NONE => emacsError "No further events"

 fun selectPrev () =
 (* Command run by M-p in emacs.  Select the previous event. *)
     case ofSome(prevEvent, !selected) of
	 SOME e => emacsSelect (SOME e)
       | NONE => emacsError "No previous events"

 (** Text display functions.
  ** These display debugger information in a textual format. *)

 fun prLoc ((file:filename,charno:charno),vis:visible) =
   (print "file \""; print file; print "\"";
    if not vis then print " [hidden version]" else ();
    print " char "; print charno)

 fun prWhere where =
   let val (s,_,loc,vis) = ensureD(ZeventDesc where, "prWhere") 
   in print (s ^ " event at "); prLoc (loc,vis)
   end

 fun prWhereWhen ((where,when):wherewhen) =
     (prWhere where;
      printL [" (time ", makestring when, ")"])

 fun prExn () =
   let val exn = safeQuery(fn now => 
			     let val (_,finalTime) = YboundingTimes()
			     in case Yexception() of
				  SOME exn => if now = finalTime then
				                SOME exn
					      else NONE
				| NONE => NONE
			     end)
   in case exn of 
        SOME exn => (print "[Execution blocked by exception: ";
		     print (System.exn_name exn);
		     print " ";
		     (case exn of
			Io s => print s
		      | System.Unsafe.CInterface.SystemCall s => print s
		      | _ => ());
		     print "]\n")
      | NONE => ()
   end

 fun showEvents filename =
    let fun f loc =
	  let val whrl = ZeventsAfterLocation loc
	      val (_,_,(_,charno),_) =
		  ensureD(ZeventDesc (hd whrl) (* may raise Hd *),
			  "showEvents")
	      fun p whr =
		let val (s,_,_,_) = ensureD(ZeventDesc whr, "showEvents")
		in print "\t"; print s; print "\n"
		end
	  in print charno;
	     app p whrl;
	     f (filename,charno+1)
	  end
    in f (filename,1) handle Hd => ()
    end

 (** Breakpoints
  ** Every breakpoint has an associated id, which is returned when the
  ** breakpoint is set and which is used to refer to the breakpoint. *)

 fun breakWhen time =
 (* Insert a breakpoint at a given time. *)
     insertBreak (TIME time)

 fun breakWhere place =
 (* Insert a breakpoint at the given event. *)
     emacsModify [place] (fn () => insertBreak (EVENT place))

 fun modifyBreak bn f =
 (* Perform a function that may modify the given breakpoint, updating the
  * screen appropriately. *)
     let val ev =
	 case (getBreak bn) of
	     SOME (EVENT e) => SOME e
	   | _ => NONE
     in emacsModify (somes [ev]) f
     end

 fun deleteBreak (bn:int) =
 (* Delete a breakpoint, given its id. *)
     modifyBreak bn
      (fn () =>
       (resetBreakFunc bn;
	if (not (removeBreak bn)) then
	  (print "[Error: breakpoint #"; print bn; print " doesn't exist.]\n")
	else ()))

 fun clearBreaks () =
 (* Delete all breakpoints. *)
     app (fn (n, _) => deleteBreak n) (!breakList)

 fun setBreakFunc (bn, f) =
 (* Set the break function at the given breakpoint. *)
     modifyBreak bn (fn () =>
		     (resetBreakFunc bn;  (* in case some function was already
					   * there *)
		      breakFuncList := (bn, f) :: (!breakFuncList)))

 fun showBreaks () =
 (* Show all breakpoints.  Primarily intended to be used outside emacs, but
  * might be useful in Emacs as well. *)
   let fun p (n,TIME whn) =
		 (print n; print "\t"; print "Time "; print whn; print "\n")
	 | p (n,EVENT place) =
		 (print n; print "\t"; prWhere place; print "\n")
   in print "Breakpoints:\n";
      app p (!breakList)
   end

 fun showBreakTimes () =
 (* Show break times in the Emacs minibuffer.  Invoked by C-M-k. *)
     let val btimes =
	 (fold
	  (fn ((_, TIME t), s) => s ^ " " ^ makestring t
		      | (_, s) => s) (!breakList) "")
     in
     emacsMessage
     ("Time breakpoints:" ^
      (if (String.length btimes > 0) then btimes else " (none)"))
     end

 fun toggleBreak () =
 (* Command run by M-k in Emacs.  Toggle whether there is a breakpoint at
  * the selected event. *)
     case !selected of
	 NONE => ()
       | SOME ev => 
	     (case breakId ev of
		  SOME n => deleteBreak n
		| NONE => breakWhere ev)

 fun currentBreak () =
 (* Returns the breakpoint number of the breakpoint at the current event
  * or at the current time. *)
     let val eventB = ofSome(breakId, SOME(establishedPlace()))
	 val eventT = ofSome(breakIdAtTime, SOME(establishedTime()))
     in if isSome eventB then eventB else eventT
     end

 fun selectedBreak () =
 (* Returns the breakpoint number of the currently selected breakpoint,
  * if any. *)
     ofSome(breakId, !selected)

 fun doBreakFunc () =
 (* Perform the break function at the current event or time.
  * Executed after we have stopped at a breakpoint. *)
     case ofSome(getBreakFunc, currentBreak ()) of
	 SOME f => f ()
       | NONE => ()

 fun bfunc f =
 (* Sets f to be the break function at the currently selected breakpoint.
  * User function. *)
     case selectedBreak () of
	 SOME bn => setBreakFunc (bn, f)
       | NONE => print "[No breakpoint is selected]\n"

 fun tfunc t f =
 (* Sets f to be the break function at the breakpoint at the given time.
  * User function. *)
     case breakIdAtTime t of
	 SOME bk => setBreakFunc (bk, f)
       | NONE => printL ["[No breakpoint exists at time ", makestring t, "]\n"]

 fun nofunc t =
 (* Resets the break function at the breakpoint at the given time.
  * User function. *)
     case breakIdAtTime t of
	 SOME bk => resetBreakFunc bk
       | NONE => printL ["[No breakpoint exists at time ", makestring t, "]\n"]

 (** Execution-related commands *)


 fun current() = 
 (* For use outside Emacs.  Display the current event in a textual form. *)
    let fun f ww = (print "[At "; prWhereWhen ww; print "]\n"; prExn())
    in if ZinDebug() then
	 f (safeQuery (fn t => (YcurrentPlace(),t)))
       else printNotUnder()
    end

 fun doMove f =
 (* Execute the given function, which causes the current code position to
  * change, while maintaining the display and its state variables. 
  * ZinDebug is assumed true. *)
     if !emacs then
	 (emacsDeselect ();
	  XsetLookerTimeF YcurrentTime;
	  (case f () of
	     COMPLETED _ => ()
	   | INTERRUPTED _ => emacsError "(Interrupted)"
	   | NOTRUNNING => raise (DebugUserError "domove"));
	  emacsUpdate ())
     else (case f () of
	     COMPLETED ww =>
		 (print "[Stopped at "; prWhereWhen ww; print "]\n"; prExn())
	   | INTERRUPTED ww =>
		 (print "[Interrupted at "; prWhereWhen ww; print "]\n"; prExn())
	   | NOTRUNNING => raise (DebugUserError "domove"))

 fun step () =
 (* Attempt step forward.  Does not update display. *)
   Xjump (establishedTime() + 1)

 fun stepb () =
 (* Attempt step backward. Does not update display. *)
   Xjump (establishedTime() - 1)

 fun moveUnderDebug f =
   if ZinDebug() then doMove f else printNotUnder()

 fun ss () = moveUnderDebug step
 fun ssb () = moveUnderDebug stepb

 fun forward() =
 (* Move forward until breakpoint or end of compilation unit.
    Do break function if any. *)
   moveUnderDebug (fn () =>
     let val now = establishedTime()
	 val (_,finalTime) = safeQuery(fn _ => YboundingTimes ())
	 val minbtime = fold (fn ((_,TIME t),m) => if t > now
						then min(t,m)
						else m
				| (_,m) => m) 
			       (!breakList) finalTime
	 fun getetimes () = fold (fn ((_,EVENT p),etl) => 
					  YlastTime p :: etl
				    | (_,etl) => etl) (!breakList) []
         val etimes = safeQuery (fn _ => getetimes())
	 fun minchanged () = 
	    fold (fn ((old,new),m) =>
		       if (new > old) andalso (new > now) andalso 
			  (new < m) then new else m)
		 (pairlist etimes (getetimes())) Zinfinity
     in XbinSearch(minchanged,minbtime)
     end before
     doBreakFunc())
  
 fun backward() =
 (* Move backward until breakpoint or start of compilation unit.
    Do break function if any. *)
    moveUnderDebug (fn () =>
      let val now = establishedTime()
          val (initialTime,_) = safeQuery (fn _ => YboundingTimes())
	  val target = safeQuery (fn _ =>
		 fold (fn ((_,TIME t), m) => if (t < now) then max(t,m) else m
	  	        | ((_,EVENT p), m) => max (YlastTime p,m))
		      (!breakList) initialTime)
      in Xjump target
      end before 
      doBreakFunc())

 fun jump t =
     moveUnderDebug (fn () => Xjump t)

 fun skip () =
 (* Skip forward, using binary search primitive.
  * Does not update display. *)
      let val now = establishedTime()
	  fun check_ancestors () = 
	    (* return upper bound on time when time 'now' is no longer on stack
	       or infinity if no such bound known. *)
	    let fun parent t = let val (_,(_,pt)) = Ycaller t
			       in pt end
		fun f (0,bound) = bound (*??*)
		  | f (when,bound) = 
		       if when = now then Zinfinity
		       else if when < now then bound
		       else f (parent when,when)
		val ct = YcurrentTime()
	    in f (ct,ct)
	    end			    
       in if (safeQuery YatCall) then
	     XbinSearch(check_ancestors,Zinfinity)
          else
	    step()
       end

 fun sk() =
   moveUnderDebug skip

 fun skipb () =
 (* Skip backward.
  * Does not update display. *)
    let fun parent t = let val (_,(_,pt)) = Ycaller t in pt end
	val init_parent = parent (establishedTime())
	fun f(0,s) = s (* paranoia *)
	  | f(t,s) = 
	    if t = init_parent then
	      s
	    else f(parent t,SOME t)
    in case stepb() of
	 COMPLETED (w,t) => 
	  (case XwithEstablishedTime(fn _ => f(t,NONE)) of
	     COMPLETED(NONE) => COMPLETED (w,t)
	   | COMPLETED(SOME t) => Xjump t
	   | INTERRUPTED _  => INTERRUPTED (w,t)
	   | NOTRUNNING => NOTRUNNING)
       | INTERRUPTED ww => INTERRUPTED ww
       | NOTRUNNING => NOTRUNNING
    end

 fun skb() = moveUnderDebug skipb

 fun jumpTrace () =
 (* If the backtrace event is selected, jump to its time.
  * Invoked by M-t in emacs. *)
     case !selected of
	 NONE => ()
       | SOME ev =>
	     case onSome(#2, !backtrace) of
		 SOME (place,time) =>
		     if ev = place then jump time else ()
	       | NONE => ()


 (** Stack backtrace commands *)

 fun showCalls maxdepth = 
   let fun p (top::rest) =
	    let fun prvar ((n:string,t:ty),v:value) = 
		     (print "\t"; print n; print " = "; ZprintVal(v,t))
		fun prcall (w as (whr,whn),vw as (vwhr,vwhn),bvlist) = 
		   (prWhereWhen w; print "\n";
		    if (whn > 0) then
		      (if (vwhn < whn andalso vwhn > 0) then
			 (print "via\t"; prWhereWhen vw; print "\n")
		       else ();
		       print "  bound values:"; app prvar bvlist; print "\n";
		       if (vwhn > 0) then
			 (print "  call: "; YprintBind (vw,8); print "\n")
		       else ())
		    else ())
	    in print "At\t"; prcall top;
	       app (fn c => (print "From\t"; prcall c)) rest
	    end
	 | p _ = ()
       fun f t = p (YcallTrace (max(maxdepth,1)-1) t)
   in interruptableQuery f
   end

 val lookerTimeF : (unit->time) ref = ref(YcurrentTime)

 fun setLTF () = 
   let val timeF = 
         case onSome(#2,!backtrace) of
	   NONE => YcurrentTime
	 | SOME (_,t) => fn () => t
   in XsetLookerTimeF timeF;
      lookerTimeF := timeF
   end

 fun upCall () =
    if ZinDebug() then
      (case onSome(#1, !backtrace) of
 	 NONE => emacsSetBackTrace (SOME 1)
       | SOME n => (emacsSetBackTrace (SOME (n+1))
		    handle SetBackTrace => emacsError "At top of call chain");
       setLTF())
    else printNotUnder()

 fun downCall () =
    if ZinDebug() then
      (case onSome(#1, !backtrace) of
  	 NONE => emacsError "At bottom of call chain"
       | SOME 1 => emacsSetBackTrace NONE
       | SOME n => emacsSetBackTrace (SOME (n-1));
       setLTF())
    else printNotUnder()

 (** Variable display functions *)

 fun showVal n =
 (* Print a value and the position of its binding site.
  * For use outside Emacs. *)
   let fun f _ = 
     case YgetVal n ((!lookerTimeF)()) of
       SOME(v,t,w as (whr,whn)) =>
	 if ZisFn(t)
	 then (print n; print "\tfunction bound by code:\n\t\t";
	       YprintBind(w,16); print "\n";
	       print "\t\t["; prWhereWhen w; print "]\n")
	 else (print n; print "\t"; ZprintVal(v,t); 
	       print "\t["; prWhereWhen w; print "]\n")
     | NONE => (print n; print "\tNot bound\n")
   in interruptableQuery f
   end

 fun emacsShowVal n =
 (* Like the preceding function, but moves the selection to the binding site
  * of the variable or function.
  * Invoked by M-l in Emacs. *)
   let fun f _ = 
       case YgetVal n ((!lookerTimeF)()) of
	 SOME(v, t, w as (whr, _)) =>
	      (printL [n, " = "];
	       ZprintVal(v,t);
	       print "\n";
	       emacsSelect (SOME whr))
       | NONE => printL [n, " is not bound\n"]
   in interruptableQuery f
   end

 (** Miscellaneous functions *)

 fun toggle() = #1 Wdd := (not (!(#1 Wdd)))

 fun finishUp () =
     if !emacs then
	 (emacsDeselect ();
	  XsetLookerTimeF YcurrentTime;
	  clearBreaks ();		(* remove breakpoints from screen *)
	  emacsKillBuffer instreamBufferName
	  ) 
     else ()

 fun complete() = 
 (* Complete the execution of the compilation unit. *)
     if ZinDebug() then
	 (finishUp();
	  Xcomplete();
	  ()) (* doesn't return if successful *)
     else printNotUnder()

 fun abort() = 
 (* Abort execution of compilation unit. *)
     if ZinDebug() then
	 (finishUp();
	  Xabort();
	  ())    (* doesn't return if successful *)
     else printNotUnder()

 (** Source code functions *)

 (* We need to know the current working directory so that we can equate
  * filenames which the user gives us, which may be relative to the current
  * directory, with filenames which Emacs gives us, which are absolute.
  * getWD seems to be horribly slow, so we remember the current directory
  * here.  In system.sml, cd is rebound to update this reference. *)

 val currentWD = ref (System.Directory.getWD())

 datatype useType = USE_DEBUG | USE_LIVEDEBUG | USE_NODEBUG

 val inUseDbg = ref USE_NODEBUG

 fun usedbg file = if ZinDebug() then
		 print "[Already running under debugger.]\n"
	      else let val dbg = !inUseDbg in
		    inUseDbg := USE_DEBUG;
 (* We convert the filename to absolute form because Emacs will send that
  * form of filename to us. *)
		    Xuse_file (FULLDEBUG "DEBUG_PERV") (absolute file (!currentWD))
		      handle e => (inUseDbg := dbg; raise e);
		   inUseDbg := dbg
		  end

fun uselive file = if ZinDebug() then
		print "[Already running under debugger.]\n"
	     else let val dbg = !inUseDbg in
		 inUseDbg := USE_LIVEDEBUG;
      Xuse_file (LIVEDEBUG "DEBUG_PERV") (absolute file (!currentWD))
        handle e => (inUseDbg := dbg; raise e);
	         inUseDbg := dbg
		 end

fun usedbg_stream s = if ZinDebug() then
		print "[Already running under debugger.]\n"
	     else Xuse_stream (FULLDEBUG "DEBUG_PERV") s

(*
fun run s =
    if ZinDebug() then
	print "[Already running under debugger.]\n"
    else
	(if !emacs then emacsBeginDebug s else ();
	 Xuse_stream (LIVEDEBUG "DEBUG_PERV") (open_string s))
*)

fun run s =
    if ZinDebug() then
      (if safeQuery(fn now => 
		     let val (_,finalTime) = YboundingTimes()
		     in now < finalTime
                     end) then
	 (print "[Compiling interpolation]\n";
	  Xuse_stream (FULLDEBUG "DEBUG_PERV") (open_string s))
       else print "[Cannot interpolate at end of compilation unit.]\n")
    else 
	(if !emacs then emacsBeginDebug s else ();
	 Xuse_stream (LIVEDEBUG "DEBUG_PERV") (open_string s))

end

open UserDebugCommands
    

    
