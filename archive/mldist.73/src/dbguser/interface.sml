structure UserDebugInterface = struct
local  open UserDebugUtil 
       structure U = System.Unsafe 
in
  open System.Control.Debug

(** User-level end of interface mechanism. Should match debug/interface.sml *)
  
 (* Data types *)
 (** shared with DebugStatic: *)
 type filename = string 
 type charno = int (* counting from 1 *)
 type location = filename * charno
 type visible = bool  (* true if file has not been hidden by reusing *)
 type time = int
 type place = int * int (* evn * evindex, but we will treat abstractly *)

 (** shared with DebugMotions, DebugQueries: *)
 type wherewhen = place * time 
 type value = U.object
 type ty = U.object (* Basics.ty *)

 (** shared with DebugMotions, DebugNRun [same defn.] *)
 datatype 'a outcome = COMPLETED of 'a | INTERRUPTED of 'a | NOTRUNNING

 (** shared with Interact: *)
 datatype debuglevel = NODEBUG 
                     | FULLDEBUG of string 
		     | LIVEDEBUG of string * string * instream option

 (** shared with DebugNrun: *)
 type br = U.object array
 type wherewhich = place * br

 (** Interface to the debugging functions in the compiler. *)

 (* Naming conventions:

  X routines are heavy-weight: they take care of establishing and resetting
    times as appropriate, and generally return outcomes to note interrupts.

  Y routines typically take a time as argument, and are more fragile; they
    must be called under the protection of an X routine 
    (such as XwithEstablishedTime) to reset times and deal with interrupts.

  Z routines generally operate independent of current time/context.

  W routines are strictly private, for debugging the debugger.
 
 For each routine, we indicate whether it is used for timed, untimed or both
  versions. 
 *)

 val Xuse_file:(debuglevel->string->unit) = !interface 1 (* both *)
 val Xuse_stream:(debuglevel->instream->unit) = !interface 2 (* both *)
 val XwithEstablishedTime:((time->'a) -> 'a outcome) = !interface 3 (* timed *)
 val YcurrentTime:(unit->time) = !interface 4 (* timed *)
 val YcurrentPlace:(unit->place) = !interface 5 (* both *)
 val YboundingTimes:(unit->(time*time)) = !interface 6 (* timed *)
 val YlastTime:(place->time) = !interface 7 (* timed *)
 val Xjump:(time->wherewhen outcome) = !interface 8 (*timed *)
 val XbinSearch:((unit->time) * time -> wherewhen outcome) = !interface 9 (*timed *)
 val YcallTrace:(int->time->((wherewhen*wherewhen*(((string*ty)*value) list)) list)) = !interface 10  (* timed *)
 val YgetVal:(string->time->(value*ty*wherewhen) option) = !interface 11 (*timed *)
 val ZprintVal:((value*ty)->unit) = !interface 12  (* both *)
 val ZisFn:(ty->bool) = !interface 13  (* both *)
 val YprintBind:((wherewhen*int)->unit) = !interface 14 (* timed only *)
 val Wdd:bool ref = U.cast !interface 15 (* both *)
 (* history store stuff uses interface 16 *) (* timed *)
 val ZeventsAfterLocation:(location -> place list) = !interface 17  (* both *)
 val ZeventsBeforeLocation:(location -> place list) = !interface 18  (* both *)
 (* history signals stuff uses interface 19 *) (* timed *)
 val Xcomplete:(unit->unit outcome) = !interface 20  (* for both *)
 val Xabort:(unit->unit outcome) = !interface 21  (* for both *)
 val ZinDebug:(unit->bool) = !interface 22   (* for both *)
 val Yexception:(unit->exn option) = !interface 23 (* for both *)
 (* history io stuff uses interface 24 *) (* timed *)
 val ZeventDesc:(place ->(string*bool*location*visible) option) = !interface 25
                  (* for both *)
 val Wdeltas:(int ref * int ref * int ref) = U.cast !interface 26
                 (* timed *)
 val Wtimes:(int array) = U.cast !interface 27  (* timed *)
 val Ycaller:(time->(wherewhen*wherewhen)) = !interface 28 (* timed *)
 val Zinfinity:int = U.cast !interface 29 (* both *)
 val XsetLookerTimeF:((unit->time)->unit) = !interface 30  (* timed *)
 val YatCall:(time->bool) = !interface 31 (* timed *)
 val WuseSpecial:(unit -> bool ref) = !interface 32 (* timed only for now *)
 val WeventCount: (unit -> int ref) = !interface 33 (* both *)
 val Wsizereport: (string->unit) ref = U.cast !interface 34 (* both *)
 val Wwithtime: bool ref = U.cast !interface 35 (* both *)
 val ZnullBr: br = U.cast !interface 36    (* untimed *)
 val YcurrentBr: unit -> br = !interface 37  (* untimed *)
 val Xcontinue: unit -> unit = !interface 38  (* untimed *)
 val Yncaller:(br->(wherewhich*wherewhich)) = !interface 39 (* untimed *)
 val YnprintBind:((wherewhich*int) -> unit) = !interface 40  (* untimed *)
 val YngetVal:(string->br->(value*ty*wherewhich) option) = !interface 41 (* untimed *)
 val ZsetBreak:(place*bool->unit) = !interface 42 (* untimed *)
 val ZsetAllBreaks:(bool->unit) = !interface 43 (* untimed *)
 val XncheckInterrupts:((unit->'a)->'a outcome) = !interface 44 (* untimed *)
 val WinstrumLevel:(unit -> int ref) = !interface 45 (* both *)
 val WmemoLevel:(unit -> int ref) = !interface 46 (* timed *)
 val WrankBase:(unit -> real ref) = !interface 47 (* timed *)
 val WdumpCache:(unit -> unit) = !interface 48 (* timed *)
 (** Useful functions on events, built up from interface functions. *)

 val WminDelta = #1 Wdeltas
 val WmaxDelta = #2 Wdeltas
 val WmaxStates = #3 Wdeltas

 fun interruptableQuery (f:time->unit)  =
   (* Suitable for operations that play with time and do text-style output *)
   case (XwithEstablishedTime f) of
     COMPLETED x => x
   | INTERRUPTED _ => print "(Interrupted)\n"
   | NOTRUNNING => printNotUnder()

 fun safeQuery f  =
   (* Suitable for operations that don't change the time *)
   case (XwithEstablishedTime f) of
     COMPLETED x => x
   | INTERRUPTED x => x
   | NOTRUNNING => raise (DebugUserError "safeQuery")

   
 fun establishedTime() = safeQuery (fn t => t)
 fun establishedPlace() = 
       if !Wwithtime then
	 safeQuery (fn _ => YcurrentPlace())
       else YcurrentPlace()

 fun eventText ev =
     #1 (ensureD(ZeventDesc ev, "eventText"))

 fun eventLocation ev : location option =
     case ZeventDesc ev of
       SOME (_,pseudo,filpos,visible) =>
	   if (not pseudo) andalso visible then
             SOME filpos
	   else NONE
     | NONE => NONE

 fun traceEvent (ww:wherewhen) (n:int) : wherewhen option = 
 (* Return the nth caller above the given location, counting that location
  * as 0th. *)
   let fun trace (ww as (_,t:time)) =
         if t > 0 then
	   fn 0 => SOME ww
	    | n => 
	       let val (_,ww) = Ycaller t
	       in trace ww (n-1)
	       end
	 else fn _ => NONE
   in case (XwithEstablishedTime (fn _ => trace ww n)) of
        COMPLETED wwop => wwop
      | INTERRUPTED _ => NONE
      | NOTRUNNING => raise (DebugUserError "traceEvent")
   end

 fun ntraceEvent ((ww as (_,br)):wherewhich) (n:int) : wherewhich option = 
 (* Return the nth caller above the given location, counting that location
  * as 0th. *)
     if br <> ZnullBr then
       if n = 0 then 
	 SOME ww
       else 
	 let val (_,ww) = Yncaller br
	 in ntraceEvent ww (n-1)
	 end
     else NONE

 local 
   exception NotAvailable
   fun findEv ev =
   (* Return file, character position for event, Xevents list containing event
    * and its position in that list. *)
       case eventLocation ev of
	 SOME (loc as (file,cp)) =>
	   let val elist = ZeventsAfterLocation loc
	       val index =
		   case (first (fn x => x = ev) elist) of
		     SOME i => i
   (* It is possible that an event will not be found in the list: fine-grained
    * events are sometimes selected as the binding sites of variables (see
    * the function emacsShowVal).  In that case, simply move to an event near
    * the fine event. *)
		   | NONE => ~1
	   in (file,cp,elist,index)
	   end
        | _ => raise NotAvailable
 in
 fun prevEvent  ev =
 (* Return the event that lexically  precedes the given event, i.e. the
  * previous event in the source text.  This allows us to sequentially
  * traverse all the events in a compilation unit. *)
     let val (file,cp,elist,index) = findEv ev
     in	
	 SOME (nth (elist, index-1 ))
	 handle Nth =>
		 (SOME (foot (ZeventsBeforeLocation (file, cp - 1)))
		  handle Hd => NONE)
     end handle NotAvailable => NONE

 fun nextEvent ev =
 (* Return the event that lexically follows the given event, i.e. the next
  * event in the source text.  This allows us to sequentially
  * traverse all the events in a compilation unit. *)
     let val (file,cp,elist,index) = findEv ev
     in
	 SOME (nth (elist, index+1))
	 handle Nth =>
		 (SOME (hd (ZeventsAfterLocation (file, cp + 1)))
		  handle Hd => NONE)
     end handle NotAvailable => NONE
 end (* local *)


(*
 fun atPseudo () =
 (* Is the current event a pseudo event? *)
     #2 (ensureD(ZeventDesc(ensureD(currentPlace(),"atPseudo1")), "atPseudo2"))
*)

end (* local *)
end (* structure *)

