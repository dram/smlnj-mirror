structure UserDebugInterface = struct
local  open UserDebugUtil in
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
 type value = System.Unsafe.object
 type ty = System.Unsafe.object (* Basics.ty *)

 datatype 'a outcome = COMPLETED of 'a | INTERRUPTED of 'a | NOTRUNNING

 (** shared with Interact: *)
 datatype debuglevel = NODEBUG | FULLDEBUG of string 
			       | LIVEDEBUG of string

 (** Interface to the debugging functions in the compiler. *)

 (* Naming conventions:

  X routines are heavy-weight: they take care of establishing and resetting
    times as appropriate, and generally return outcomes to note interrupts.

  Y routines typically take a time as argument, and are more fragile; they
    must be called under the protection of an X routine 
    (such as XwithEstablishedTime) to reset times and deal with interrupts.

  Z routines generally operate independent of current time/context.

  W routines are strictly private, for debugging the debugger.
 *)

 val Xuse_file:(debuglevel->string->unit) = !interface 1
 val Xuse_stream:(debuglevel->instream->unit) = !interface 2
 val XwithEstablishedTime:((time->'a) -> 'a outcome) = !interface 3
 val YcurrentTime:(unit->time) = !interface 4
 val YcurrentPlace:(unit->place) = !interface 5
 val YboundingTimes:(unit->(time*time)) = !interface 6
 val YlastTime:(place->time) = !interface 7
 val Xjump:(time->wherewhen outcome) = !interface 8
 val XbinSearch:((unit->time) * time -> wherewhen outcome) = !interface 9
 val YcallTrace:(int->time->((wherewhen*wherewhen*(((string*ty)*value) list)) list)) = !interface 10 
 val YgetVal:(string->time->(value*ty*wherewhen) option) = !interface 11
 val ZprintVal:((value*ty)->unit) = !interface 12
 val ZisFn:(ty->bool) = !interface 13
 val YprintBind:((wherewhen*int)->unit) = !interface 14 
 val Wdd:(bool ref * bool ref) = System.Unsafe.cast !interface 15
 (* history store stuff uses interface 16 *)
 val ZeventsAfterLocation:(location -> place list) = !interface 17
 val ZeventsBeforeLocation:(location -> place list) = !interface 18
 (* history signals stuff uses interface 19 *)
 val Xcomplete:(unit->unit outcome) = !interface 20
 val Xabort:(unit->unit outcome) = !interface 21
 val ZinDebug:(unit->bool) = !interface 22
 val Yexception:(unit->exn option) = !interface 23
 (* history io stuff uses interface 24 *)
 val ZeventDesc:(place ->(string*bool*location*visible) option) = !interface 25
 val Wdeltas:(int ref * int ref * int ref) = System.Unsafe.cast !interface 26
 val Wtimes:(int array) = System.Unsafe.cast !interface 27
 val Ycaller:(time->(wherewhen*wherewhen)) = !interface 28
 val Zinfinity:int = System.Unsafe.cast !interface 29
 val XsetLookerTimeF:((unit->time)->unit) = !interface 30
 val YatCall:(time->bool) = !interface 31
 (** Useful functions on events, built up from interface functions. *)

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
 fun establishedPlace() = safeQuery (fn _ => YcurrentPlace())

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

