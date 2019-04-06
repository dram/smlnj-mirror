structure UserDebug = struct

(*** User-level interface to the debugger, including Emacs interface. ***)

open System.Control.Debug

val _ = debugging := true

(** shared with DbgStat: *)
type filename = string 
type charno = int (* counting from 1 *)
type location = filename * charno
type visible = bool  (* true if file has not been hidden by reusing *)

(** shared with DbgCom: *)
type when = int
type where = int * int (* coarse event * index number *)
type wherewhen = where * when

datatype break = TIME of when |
	         EVENT of where
type value = System.Unsafe.object
type valtype = System.Unsafe.object
datatype valinfo = UNBOUND |
     	           BOUND of value * valtype * wherewhen

datatype 'a outcome = COMPLETED of 'a | INTERRUPTED of 'a | NOTRUNNING

(** shared with Interact: *)
datatype debuglevel = NODEBUG | FULLDEBUG of string 
			      | LIVEDEBUG of string*string

(** Local types and exceptions *)

type eventn = where  (* event descriptor
		      * This is an abstract type at the user-level: we
		      * don't access its components. *)

exception NotDebugging	    (* Raised when a debugging function is
			     * is called when no program is being debugged. *)

exception DebugUserError of string  (* Indicates a problem in this code. *)

(** Interface to the debugging functions in the compiler. *)

val Xuse_file:(debuglevel->string->unit) = !interface 1
val Xuse_stream:(debuglevel->instream->unit) = !interface 2
val XclearBreaks:(unit->unit) = !interface 3
val XinsertBreak:(break->int) = !interface 4
val XdeleteBreak:(int->bool) = !interface 5
val XgetBreaks:(unit->(int*break) list) = !interface 6
val Xcurrent:(unit->wherewhen outcome) = !interface 7
val Xforwards:(unit->wherewhen outcome) = !interface 8
val Xbackwards:(unit->wherewhen outcome) = !interface 9
val XcallTrace:(int->(((wherewhen*wherewhen*(((string*valtype)*value) list)) list) outcome)) = !interface 10 
val XgetVal:(string->valinfo) = !interface 11
val XprintVal:((value*valtype)->unit) = !interface 12
val XisFn:(valtype->bool) = !interface 13
val XprintBind:((wherewhen*int)->unit) = !interface 14 
val Xdd:(bool ref * bool ref) = System.Unsafe.cast !interface 15
(* history store stuff uses interface 16 *)
val XeventsAfterLocation:(location -> where list) = !interface 17
val XeventsBeforeLocation:(location -> where list) = !interface 18
val Xreset:(unit->unit) = !interface 19
val Xcomplete:(unit->unit) = !interface 20
val Xabort:(unit->unit) = !interface 21
val Xindebug:(unit->bool) = !interface 22
val Xexception:(unit->exn option) = !interface 23
(* history io stuff uses interface 24 *)
val XeventDesc:(wherewhen->(string*location*visible) option) = !interface 25
(* XeventDesc should really take a where, not a wherewhen, as its argument;
 * however, the debugger currently needs the time of an I/O event to determine
 * where it occurred.  In all other cases we can safely set the when component
 * of the argument to this function to zero. *)
val Xdeltas:(int ref * int ref * int ref) = System.Unsafe.cast !interface 26
val Xtimes:(int array) = System.Unsafe.cast !interface 27
val Xactivate:(bool ref * bool ref) = System.Unsafe.cast !interface 28
val Xprovoke:(exn option->unit) = !interface 29

(** Generally useful functions.  Perhaps these belong in a separate file. *)

(* Find the first element in a list for which a given condition is
 * satisfied. *)
fun select f nil = NONE
  | select f (a :: a') = if (f a) then SOME a else select f a'

(* Construct a list of members of a given list for which a given condition
 * is satisfied *)
fun choose f l =
    fold (fn (m, l) => (if (f m) then [m] else []) @ l) l []

(* look up an element of an association list *)
fun lookup nil l = NONE
  | lookup ((k, v) :: rest) l = if l = k then SOME v else lookup rest l

fun first f l =
(* Find the index of the first element in a list for which a given condition
 * is satisfied. *)
    let fun first' f n nil = NONE
	  | first' f n (a :: b) =
	    if (f a) then SOME n else first' f (n + 1) b
    in first' f 0 l end

val printL = print o implode

val foot = hd o rev  (* last element of a list
		      * probably not a terribly efficient implementation *)

(* Some simple option functions *)

fun eqOption (a, SOME b) = (a = b)
  | eqOption _ = false

fun onSome (f, SOME a) = SOME (f a)
  | onSome (_, NONE) = NONE

fun ofSome (f, SOME a) = f a
  | ofSome (_, NONE) = NONE

fun ifSome (f, SOME a) = f a
  | ifSome (_, NONE) = ()

fun isSome (SOME _) = true
  | isSome NONE = false

fun somes (SOME a :: a') = a :: (somes a')
  | somes (NONE :: a') = (somes a')
  | somes nil = nil

fun ensure (SOME x, _) = x
  | ensure (NONE, e) = raise e

fun ensureD (v, s) = ensure (v, DebugUserError s)

infix isIn
fun elem isIn set = isSome (select (fn x => x = elem) set)

(* Strip duplicate elements from a list *)
fun uniq nil = nil
  | uniq (a :: a') =
    let val u = uniq a' in
	if a isIn u then u else (a :: u)
    end

fun string_first s s' =
(* Find the first occurrence of a given character in a string. *)
    first (fn c => (c = s')) (explode s)

fun rightstring (s, n) =
(* Return the substring of s extending from position n to the end *)
    substring (s, n, size s - n)

fun splitString delimiter s =
(* Return a list of elements (strings separated by single or multiple
   delimiters) in a given string. *)
    if s = "" then [] else
	case string_first s delimiter of
	    NONE => [s]
	  | SOME n =>
		let val rest = splitString delimiter (rightstring (s, n + 1))
		in
		    if n = 0 then rest else substring (s, 0, n) :: rest
		end

fun dewords l c =
(* Return a string constructed by prefixing each word in the given list with
 * the given character. *)
    implode (fold (fn (i, s) => c :: i :: s) l [])

fun absolute pathname directory =
(* Convert the given pathname, which is relative to the given directory,
 * to absolute form. *)
    let fun simplify nil = nil
          | simplify (a :: rest) =
	    let val rest' = simplify rest in
		case rest' of
		    nil => [a]
		  | ".." :: r' => if a <> ".." then r' else a :: rest'
		  | _ => a :: rest'
	    end
	val already = (substring(pathname, 0, 1) = "/")
	val path = splitString "/"
	    (if already then pathname else directory ^ "/" ^ pathname)
    in dewords (simplify path) "/"
    end

(** Useful functions on events and breakpoints, built up from the
 ** X-functions. *)

fun runStatus (COMPLETED s) = SOME s
  | runStatus (INTERRUPTED s) = SOME s
  | runStatus NOTRUNNING = NONE

fun currentWhereWhen () = runStatus (Xcurrent ())

fun currentWhere () = onSome (#1, currentWhereWhen ())
fun currentWhen () = onSome (#2, currentWhereWhen ())
fun currentTime () =
    case currentWhen () of
	SOME t => t
      | NONE => 0

fun eventText ev =
(* Some events (i.e. START and IO events) cannot be accessed out of the
 * context of a particular time, and are in fact never seen when they are
 * not current.  We pass the current time so that those events will be
 * handled correctly; the time will be ignored for other types of events. *)
    case XeventDesc (ev, currentTime()) of
	SOME (t, _, _) => t
      | NONE => raise DebugUserError "eventText"

fun eventLocation ev =
    case XeventDesc (ev, currentTime()) of
        SOME (_, l, _) => l
      | NONE => raise DebugUserError "eventLocation"

fun adjacentEvent forward ev =
(* Return the event that lexically follows (if forward = true) or
 * precedes (if forward = false) the given event, i.e. the next or
 * previous event in the source text.  This allows us to sequentially
 * traverse all the events in a compilation unit. *)
    let val loc as (file, cp) = eventLocation ev
        val elist = XeventsAfterLocation loc
	val index =
	    case (first (fn x => x = ev) elist) of
		SOME i => i
(* It is possible that an event will not be found in the list: fine-grained
 * events are sometimes selected as the binding sites of variables (see
 * the function emacsShowVal).  In that case, simply move to an event near
 * the fine event. *)
	      | NONE => 0
    in
	SOME (nth (elist, (if forward then op + else op -) (index, 1)))
	handle Nth =>
	    if forward then
		(SOME (hd (XeventsAfterLocation (file, cp + 1)))
		 handle Hd => NONE)
	    else
		(SOME (foot (XeventsBeforeLocation (file, cp - 1)))
		 handle Hd => NONE)
    end

val nextEvent = adjacentEvent true
val prevEvent = adjacentEvent false

fun traceEvent n =
(* Returns the n-th whereWhen in the call traceback from the current
 * location, where the 0-th event is the current location. *)
    let val trace = ensure (runStatus (XcallTrace n),
			    NotDebugging)
	val ((where, when), _, _) = nth (trace, n)
    in SOME (where, when)
    end handle Nth => NONE

fun getBreak n =
(* Return the breakpoint with given id, if any. *)
    onSome(#2, select (fn (k, _) => k = n) (XgetBreaks ()))

fun breakId ev =
(* Return the id of the breakpoint at the given event. *)
    let fun eqb (_, EVENT e) = (e = ev)
	  | eqb _ = false
    in onSome (#1, select eqb (XgetBreaks ()))
    end

fun breakIdAtTime tm =
(* Return the id of the breakpoint at the given time. *)
    let fun eqb (_, TIME t) = (t = tm)
	  | eqb _ = false
    in onSome (#1, select eqb (XgetBreaks ()))
    end

(* Each function on the breakFuncList will be invoked whenever the
 * corresponding breakpoint is reached.
 * Typically f would be a function to show a variable's value, or to
 * continue (e.g. by calling forward()) if a certain value holds.
 * Using this technique breakpoints can be made conditional. *)
val breakFuncList = ref ([] : (int * (unit -> unit)) list)

fun getBreakFunc bn =
(* Get the break function at the given breakpoint. *)
    lookup (!breakFuncList) bn

fun atIo () =
(* Is the current event an I/O event ? *)
    case runStatus (Xcurrent()) of
	SOME wherewhen =>
	    (case XeventDesc wherewhen of
		 SOME (eventType, _, _) => eventType = "IO"
	       | NONE => false)
      | NONE => false

(** Display functions.  sml can be run inside emacs using a sml mode written
 ** in Emacs Lisp (see sml-mode.el, sml-debug.el).
 ** All display updates are performed through these functions. *)

val emacs = ref false     (* Are we running sml in an Emacs window?
			     * Emacs sends a "inEmacs := true" to sml when
			     * sml starts up. *)

fun emacsInit () =
    (emacs := true;
     set_term_in (std_in, true);
     set_term_out (std_out, true))

(* currentEv is the event that is displayed as the current event.
 * Each time that execution stops, currentEv is set to the value
 * returned by currentWhere ().*)

val currentEv = ref (NONE : eventn option)

(* The selected event is an event which the user uses as a cursor
 * to browse through and choose events. *)

val selected = ref (NONE : eventn option)

(* The backtrace event is the event which the user has chosen by moving
 * up and down in the calling stack.  We store an index into the XcallTrace()
 * list as well as the (where, when) of the trace event, since calling
 * XcallTrace() is expensive. *)

val backtrace = ref (NONE : (int * wherewhen) option)

fun emacsMessage message =
    printL ["(emacs (message \"", message, "\"))\n"]

val instreamBufferName = "*sml-debug-command*"

(* The command being run is read from a stream; the filename
 * for its events will be "<instream>". *)
val instreamName = "<instream>"

fun emacsLabelCommand display file (pos:int) string cursor =
(* Display (display = true) or undisplay (display = false) a label in an
 * Emacs buffer.  For displaying, cursor should have the value SOME b,
 * where b represents whether we should move the cursor to the beginning
 * of the label. *)
    printL ["(emacs (sml-",
	    if display then "label" else "unlabel",
	    "-buffer \"",
	    if file = instreamName then instreamBufferName else file,
	    "\" ", makestring pos,
	    " \"", string, "\"",
	    case cursor of SOME b => if b then " t" else " nil" | NONE => "",
	    "))\n"]

fun emacsCreateBuffer name contents =
    printL ["(emacs (sml-create-buffer \"", name, "\" \"", contents, "\"))\n"]

fun emacsKillBuffer bufName =
    printL ["(emacs (sml-kill-buffer \"", bufName, "\"))\n"]

fun emacsError s =
    printL ["(emacs (sml-error \"", s, "\"))\n"]

fun emacsGoodBye () =
    printL ["(emacs (sml-good-bye))\n"]

fun emacsEvent display ev =
(* Display (display = true) or undisplay (display = false) a given event.
 * An event is only displayed if
     - it is the selected event
     - it is the current event
     - it is the backtrace event
     - there is a breakpoint at that event *)
    let val isSelected = eqOption (ev, !selected)
	val isCurrent = eqOption (ev, !currentEv)
	val isBacktrace =
	    case !backtrace of
		SOME (i, (where, when)) =>
		    if ev = where
			then SOME when
		    else NONE
	      | NONE => NONE
	val isBreakpoint = isSome (breakId ev)
	val displayed = isSelected orelse isCurrent orelse
	                isSome(isBacktrace) orelse isBreakpoint
    in
	if displayed then
	    let val s =
		implode [if isSelected then "[" else "<",
			 case breakId ev of
			     NONE => ""
			   | SOME bn =>
				 if isSome (getBreakFunc bn)
				     then "bk*:"
				     else "bk:",
			 eventText ev,
			 if isCurrent then
			     ":" ^ (makestring (ensureD (currentWhen (),
							 "emacsEvent")))
			     else "",
			 case isBacktrace of
			     SOME when => ":bt:" ^ (makestring when)
			   | NONE => "",
			 if isSelected then "]" else ">"]
		val (file, pos) = eventLocation ev
	    in emacsLabelCommand display file pos s
		(if display then SOME isSelected else NONE)
	    end
	else ()
    end

val emacsDisplay = emacsEvent true
val emacsUndisplay = emacsEvent false

fun emacsModify evl f =
(* Perform a function that may modify the appearance of the given events. *)
    let val evl' = uniq evl in	(* don't display or undisplay an event twice *)
	(app emacsUndisplay evl';
	 f ();
	 app emacsDisplay evl')
    end

fun emacsSelect ev =
(* Select the given event and update the display.
 * The selected event is the one on which the event cursor rests. *)
    emacsModify (somes [!selected, ev]) (fn () => selected := ev)

exception SetBackTrace
fun emacsSetBackTrace bt =
(* Set the backtrace event to that indexed by the given integer in the
 * call trace list, or to NONE; selects the backtrace event, or the
 * current event if there is no backtrace event.
 * Updates the display. *)
    let val new = case bt of
	    SOME n =>
		(case traceEvent n of
		    SOME ww => SOME (n, ww)
		  | NONE => raise SetBackTrace)
	  | NONE => NONE
	val oldEv = onSome(#1 o #2, !backtrace)
	  (* event number of old backtrace event *)
	val newEv = onSome(#1 o #2, new)
	val newSel = case newEv of
	              SOME e => SOME e
		    | NONE => !currentEv
    in
	emacsModify (somes [oldEv, newEv, !selected, newSel])
	  (fn () => (backtrace := new;
		     selected := newSel))
    end

fun emacsSetCurrent ev =
(* Set the current event, updating the display appropriately. *)
    emacsModify (somes [!currentEv, ev]) (fn () => currentEv := ev)

(* The functions emacsDeselect and emacsUpdate could be expressed using
 * the three functions above, but call emacsModify themselves to optimize
 * the number of emacs commands that are generated. *)

fun emacsDeselect () =
(* Invoked each time before execution begins.
 * Reset the selected, backtrace and current events. *)
    emacsModify (somes [!selected, onSome(#1 o #2, !backtrace),
			!currentEv]) (fn () =>
				      (selected := NONE;
				       backtrace := NONE;
				       currentEv := NONE))

fun emacsUpdate () =
(* Invoked each time that execution stops, to set the current and selected
 * events. *)
	let val where = currentWhere () in
	    emacsModify (somes [!currentEv, !selected, where])
	    (fn () => (currentEv := where; selected := where))
	end

fun emacsBeginDebug s =
    printL ["(emacs (sml-start-debugging \"", s, "\"))\n"]

fun emacsInitDebug s =
    if Xindebug() then
	(emacsCreateBuffer instreamBufferName s;
	 emacsUpdate())
    else ()  (* command to be run failed to compile *)

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
    (SOME (hd (XeventsAfterLocation
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
  let val (s,loc,vis) = ensureD(XeventDesc (where, 0), "prWhere")
  in
    (print (s ^ " event at "); prLoc (loc,vis))
  end

fun prWhereWhen ((where,when):wherewhen) =
    (prWhere where;
     printL [" (time ", makestring when, ")"])

fun prExn () =
  let val exn = Xexception() 
  in case exn of
       SOME exn => (print "Execution blocked by exception: ";
		    print (System.exn_name exn);
		    print " ";
		    (case exn of
		       Io s => print s
		     | System.Unsafe.CInterface.SystemCall s => print s
		     | _ => ());
		    print "\n")
     | NONE => ()
  end

fun showEvents filename =
   let fun f loc =
     	 let val whrl = XeventsAfterLocation loc
	     val (_, (_,charno), _) =
		 ensureD(XeventDesc ((hd whrl),0) (* may raise Hd *),
			 "showEvents")
	     fun p whr =
	       let val (s,_,_) = ensureD(XeventDesc (whr,0), "showEvents")
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
    XinsertBreak (TIME time)

fun breakWhere eventn =
(* Insert a breakpoint at the given event. *)
    emacsModify [eventn] (fn () => XinsertBreak (EVENT eventn))

fun modifyBreak bn f =
(* Perform a function that may modify the given breakpoint, updating the
 * screen appropriately. *)
    let val ev =
	case (getBreak bn) of
	    SOME (EVENT e) => SOME e
	  | _ => NONE
    in emacsModify (somes [ev]) f
    end

fun resetBreakFunc bn =
(* Remove the break function from the given breakpoint.
 * Does not update the display. *)
    breakFuncList := choose (fn (n, _) => n <> bn) (!breakFuncList)

fun deleteBreak (bn:int) =
(* Delete a breakpoint, given its id. *)
    modifyBreak bn
     (fn () =>
      (resetBreakFunc bn;
       if (not (XdeleteBreak bn)) then
	   (print "Error: breakpoint #"; print bn; print " doesn't exist.\n")
       else ()))

fun clearBreaks () =
(* Delete all breakpoints. *)
    app (fn (n, _) => deleteBreak n) (XgetBreaks ())

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
	| p (n,EVENT eventn) =
		(print n; print "\t"; prWhere eventn; print "\n")
  in print "Breakpoints:\n";
     app p (XgetBreaks())
  end

fun showBreakTimes () =
(* Show break times in the Emacs minibuffer.  Invoked by C-M-k. *)
    let val btimes =
	(fold
	 (fn ((_, TIME t), s) => s ^ " " ^ makestring t
                     | (_, s) => s) (XgetBreaks()) "")
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
    let val eventB = ofSome(breakId, currentWhere())
	val eventT = ofSome(breakIdAtTime, currentWhen())
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
      | NONE => print "No breakpoint is selected\n"

fun tfunc t f =
(* Sets f to be the break function at the breakpoint at the given time.
 * User function. *)
    case breakIdAtTime t of
	SOME bk => setBreakFunc (bk, f)
      | NONE => printL ["No breakpoint exists at time ", makestring t, "\n"]

fun nofunc t =
(* Resets the break function at the breakpoint at the given time.
 * User function. *)
    case breakIdAtTime t of
	SOME bk => resetBreakFunc bk
      | NONE => printL ["No breakpoint exists at time ", makestring t, "\n"]

(** Execution-related commands *)

fun current() = 
(* For use outside Emacs.  Display the current event in a textual form. *)
   let fun f ww = (print "At "; prWhereWhen ww; print "\n"; prExn())
   in f (ensure (currentWhereWhen(), NotDebugging))
   end
 
fun doMove f =
(* Execute the given function, which causes the current code position to
 * change, while maintaining the display and its state variables. *)
    if !emacs then
	(emacsDeselect ();
	 f ();
	 emacsUpdate ())
    else (f ();
	case Xcurrent () of
	    COMPLETED ww =>
		(print "Stopped at "; prWhereWhen ww; print "\n"; prExn())
	  | INTERRUPTED ww =>
		(print "Interrupted at "; prWhereWhen ww; print "\n"; prExn())
	  | NOTRUNNING => print "Not executing under debugger\n")
 
fun provoke exnopt = 
      if Xindebug() then 
	Xprovoke exnopt
      else print "Not executing under debugger\n" 

(* I/O events are problematic for a number of reasons; for now, we will
 * simply ignore them. *)

fun step () =
(* Step forward, moving past any I/O events.
 * Does not update the display. *)
    let val bp = XinsertBreak (TIME((Xtimes sub 0) + 1))
    in Xforwards();
	XdeleteBreak bp;
	if atIo () then step () else ()
    end

fun stepb () =
(* Step backward, moving past any I/O events. *)
  let val bp = XinsertBreak (TIME((Xtimes sub 0) - 1))
  in Xbackwards();
      XdeleteBreak bp;
      if atIo () then stepb () else ()
  end

fun ss () =
  if Xindebug() then doMove step
  else print "Not executing under debugger\n"

fun ssb () =
  if Xindebug() then doMove stepb
  else print "Not executing under debugger\n"

fun forward () =
(* Move forward until breakpoint or end of compilation unit *)
    doMove (fn () =>
	  (Xforwards();
	   if atIo () then step () else ();
	   doBreakFunc()))		(* tail-recursive *)

fun backward () =
    doMove (fn () =>
	  (Xbackwards();
	   if atIo () then stepb () else ();
	   doBreakFunc()))		(* tail-recursive *)

fun jumpTo t =
(* We set a breakpoint at the given time and then repeatedly try to move
 * to it, stopping when we get to that time or when we are no longer moving.
 * We have to move repeatedly because we may hit other breakpoints. *)
    let val bp = XinsertBreak (TIME t)
	fun go (last,oc as COMPLETED(ww as (_,now))) =
	    if last <> now then
		(if now > t then 
		     go(now,Xbackwards())
		 else if now < t then
		     go(now,Xforwards())
		      else oc)
	    else oc
	  | go (last,oc as INTERRUPTED(ww as (_now))) = oc
	  | go _ = raise DebugUserError "jump"
    in
	go(~1,Xcurrent());
	XdeleteBreak bp;
	if atIo () then step () else ()
    end

fun jump t =
    if Xindebug() then
	doMove (fn () => jumpTo t)
    else print "No active debug session\n"

fun jumpTrace () =
(* If the backtrace event is selected, jump to its time.
 * Invoked by M-t in emacs. *)
    case !selected of
	NONE => ()
      | SOME ev =>
	    case onSome(#2, !backtrace) of
		SOME (where, when) =>
		    if ev = where then jump when else ()
	      | NONE => ()

(** Stack backtrace commands *)

fun showCalls maxdepth = 
  let fun p (top::rest) =
      let fun prvar ((n:string,t:valtype),v:value) = 
			(print "\t"; print n; print " = "; XprintVal(v,t))
          fun prcall (w as (whr,whn),vw as (vwhr,vwhn),bvlist) = 
	   (prWhereWhen w; print "\n";
            if (whn > 0) then
              (if (vwhn < whn andalso vwhn > 0) then
	         (print "via\t"; prWhereWhen vw; print "\n")
	       else ();
	       print "  bound values:"; app prvar bvlist; print "\n";
	       if (vwhn > 0) then
	         (print "  call: "; XprintBind (vw,8); print "\n")
	       else ())
	    else ())
      in print "At\t"; prcall top;
         app (fn c => (print "From\t"; prcall c)) rest
      end
	| p _ = raise DebugUserError "showCalls"
  in case (XcallTrace (max(maxdepth,1)-1)) of
       COMPLETED x => p x
     | INTERRUPTED x => (p x; print "(Interrupted)\n")
     | NOTRUNNING => print "Not executing under debugger\n"
  end

fun upCall () =
    case onSome(#1, !backtrace) of
	NONE => emacsSetBackTrace (SOME 1)
      | SOME n => (emacsSetBackTrace (SOME (n+1))
		   handle SetBackTrace => emacsError "At top of call chain")

fun downCall () =
    case onSome(#1, !backtrace) of
	NONE => emacsError "At bottom of call chain"
      | SOME 1 => emacsSetBackTrace NONE
      | SOME n => emacsSetBackTrace (SOME (n-1))

(** Variable display functions *)

fun printVal n =
(* Print a variable and its value. *)
    case (XgetVal n) of
	BOUND(v, t, w as (whr, _)) =>
	     (printL [n, " = "];
	      XprintVal(v,t);
	      print "\n")
      | UNBOUND => printL [n, " is not bound\n"]

fun valBindSite n =
    case (XgetVal n) of
	BOUND(_, _, (whr, _)) => SOME whr
      | UNBOUND => NONE

fun showVal n =
(* Print a value and the position of its binding site.
 * For use outside Emacs. *)
  case (XgetVal n) of
    BOUND(v,t,w as (whr,whn)) =>
      if XisFn(t)
      then (print n; print "\tfunction bound by code:\n\t\t";
            XprintBind(w,16); print "\n";
            print "\t\t["; prWhereWhen w; print "]\n")
      else (print n; print "\t"; XprintVal(v,t); 
  	    print "\t["; prWhereWhen w; print "]\n")
  | UNBOUND => (print n; print "\tNot bound\n")

fun emacsShowVal n =
(* Like the preceding function, but moves the selection to the binding site
 * of the variable or function.
 * Invoked by M-l in Emacs. *)
    (printVal n;
     case valBindSite n of
	 SOME whr => emacsSelect (SOME whr)
       | NONE => ())

fun emacsShowTraceVal n =
(* Like the preceding function, but operates in the scope of the backtrace
 * event.  Invoked by C-M-l in Emacs. *)
    case onSome(#2, !backtrace) of
	SOME (_, when) =>
	    (case currentWhen () of
		SOME t => 
		    (jumpTo when;      (* don't update display *)
		     printL ["(backtrace ", makestring when, ") "];
		     printVal n;
		     (* We must be careful not to select the binding event
		      * until we return to the actual current time, so that
		      * the display will be updated properly. *)
		     let val site = valBindSite n in
			 jumpTo t;
			  case site of
			      SOME whr => emacsSelect (SOME whr)
			    | NONE => ()
		     end)
	      | NONE => ())
      | NONE => ()

(** Miscellaneous functions *)

fun toggle() = #1 Xdd := (not (!(#1 Xdd)))

fun reset () = (Xreset ();
		print "Warning: DebugList definitions have been lost\n")

fun finishUp () =
    if !emacs then
	(emacsDeselect ();
	 clearBreaks ();		(* remove breakpoints from screen *)
	 emacsKillBuffer instreamBufferName
	 ) else ();

fun complete() = 
(* Complete the execution of the compilation unit. *)
    if Xindebug() then
	(finishUp();
	 Xcomplete()) (* doesn't return if successful *)
    else print "Not executing under debugger\n"

fun abort() = 
(* Abort execution of compilation unit, or exit if at top level *)
    if Xindebug() then
	(finishUp();
	 Xabort())    (* doesn't return if successful *)
    else emacsGoodBye ();

(** Source code functions *)

(* We need to know the current working directory so that we can equate
 * filenames which the user gives us, which may be relative to the current
 * directory, with filenames which Emacs gives us, which are absolute.
 * getWD seems to be horribly slow, so we remember the current directory
 * here.  In system.sml, cd is rebound to update this reference. *)

val currentWD = ref ""

datatype useType = USE_DEBUG | USE_LIVEDEBUG | USE_NODEBUG

val inUseDbg = ref USE_NODEBUG

fun usedbg file = if Xindebug() then
		print "Already running under debugger.\n"
	     else let val dbg = !inUseDbg in
		   inUseDbg := USE_DEBUG;
(* We convert the filename to absolute form because Emacs will send that
 * form of filename to us. *)
		   Xuse_file (FULLDEBUG "DEBUGZ") (absolute file (!currentWD))
		     handle e => (inUseDbg := dbg; raise e);
		   inUseDbg := dbg
		  end

fun uselive file = if Xindebug() then
		print "Already running under debugger.\n"
	     else let val dbg = !inUseDbg in
		 inUseDbg := USE_LIVEDEBUG;
      Xuse_file (LIVEDEBUG ("DEBUGZ","DEBUGN")) (absolute file (!currentWD))
        handle e => (inUseDbg := dbg; raise e);
	         inUseDbg := dbg
		 end

fun usedbg_stream s = if Xindebug() then
		print "Already running under debugger.\n"
	     else Xuse_stream (FULLDEBUG "DEBUGZ") s

fun run s =
    if Xindebug() then
	print "Already running under debugger.\n"
    else
	(if !emacs then emacsBeginDebug s else ();
	 Xuse_stream (LIVEDEBUG ("DEBUGZ","DEBUGN")) (open_string s))

end

open UserDebug
