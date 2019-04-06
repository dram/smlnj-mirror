signature HISTORYIO = sig
    type iohandle
    val silent: bool ref
    val remember: (unit -> iohandle)
    val restore: (iohandle -> unit)
    val logit:((unit->'a)*('a->unit)*bool)->'a  
    val nexttime:(unit->int)
    val reset:(unit -> unit)
    val zap:(int->unit)
    val usebreak:(unit->bool) ref
    val activate:bool ref
end

structure HistIO:HISTORYIO = struct
  open DbgUtil DbgKern

  val activate = ref true

  fun dbprint (s:string) = if !debugdebug then print s else ()

  datatype ioresult = NORMAL of System.Unsafe.object 
	            | EXCEPTION of exn
		    | NILentry

  type entry = {time:int,value:ioresult,shouldbreak:bool}

  structure Log = Dynamic (struct 
			      open Array
			      type array = entry array
			      type elem = entry
			   end)

  val log = Log.array {time=0,value=NILentry,shouldbreak=false}

  type iohandle = int
  val entries = ref 0  (* number filled *)
  val currentry = ref 0 (* next to fill or use *)

  val silent = ref false
  val usebreak = ref (fn () => false)

  fun reset () = (entries := 0;
		  currentry := 0;
		  silent := false;
		  usebreak := (fn () => false))

  fun remember () = !currentry
		    before dbprint ("*remembering " ^ makestring(!currentry) ^ "\n") 
	
  fun restore h =  currentry := h
		   before dbprint ("*restoring "^makestring(!currentry)^"\n")

  fun zap t = (* cut off tail end of log; all times >= t *)
    (dbprint ("*zapping >= " ^  makestring t ^ "\n"); 
     while ((!entries > 0) andalso (#time(Log.sub(log,!entries-1)) >= t)) do
      dec entries;
     currentry := min(!currentry,!entries))

  fun nexttime () = (* return time of first entry (starting with
			currentry) with 'shouldbreak' marked true;
			infinity if none *)
    let fun f e = if (e < !entries) then
		    if #shouldbreak(Log.sub(log,e)) then
		      #time(Log.sub(log,e))
		    else f (e+1)
		  else infinity
    in f (!currentry)
    end

  local 
    val iobrarray:System.Unsafe.object array = 
				System.Unsafe.cast array(3,1)	(* io event *)
  in
    fun logit (f:unit->'a,g:'a->unit,shouldbreak:bool) :'a = 
     ((*dbprint ("*entering logit "^makestring (times sub 0)^"\n"); *)
      update(times,0,(times sub 0) + 1);   (* currentTime *)
      if (shouldbreak andalso (!usebreak)())
	       orelse ((times sub 0) = (times sub 1)) then
        (update(System.Unsafe.cast iobrarray,1,(times sub 0)-1); (* fake lbt *)
         update(System.Unsafe.cast iobrarray,2,shouldbreak);(* flag cause *)
         (!DbgKern.break) iobrarray)
      else ();
      if (!currentry) < (!entries)
      then
	let val {time,value,...} = Log.sub(log,!currentry) before 
				     inc currentry
	in if not (time = (times sub 0)) then (* just a double-check *)
		  debugPanic ("history log time mismatch" ^ (makestring time)
				^ " " ^ (makestring (times sub 0)))
	   else case value of
		  NORMAL v => (if not (!silent) then
				 g(System.Unsafe.cast v)
			       else (); 
			       System.Unsafe.cast v)
		| EXCEPTION e => raise e
		| NILentry => debugPanic "history log bad"
	end 
      (*  before dbprint ("*logit "^makestring (times sub 0)^ " reuse\n")*)
      else 
	let fun enter v = 
             if !activate then
	      (Log.update (log, !currentry, 
		{time=(times sub 0),value=v,shouldbreak=shouldbreak});
	      inc entries;
	      inc currentry)
             else ()
	in let val v = f()
	   in enter (NORMAL (System.Unsafe.cast v));
	      v
	   end 
	   handle e => (enter (EXCEPTION e);
			raise e)
	end
       (* before dbprint ("*logit " ^ makestring (times sub 0) ^ " add\n")*)
      )
  end (*local *)

end (* structure *)

(* Notes:

(1) On replay, appearance of terminal i/o is controlled by the silent flag.
If flag is false, terminal input is echoed to std_out, and terminal
output is performed to std_out. If flag is true, terminal input is not
echoed and terminal output is thrown away.  All non-terminal output is
always thrown away.

The choice of std_out for echoing input is somewhat arbitrary.
Using std_out for output too avoids problems
with closed output streams, although it gives the wrong results when
multiple streams are writing to same terminal due to buffering.

(2) There may be a problem with exceptions being raised in the middle
of I/O operations: on input, this means no input is returned, but
input pointer may have changed (we get this right); on output, some
output may have occured (we get this wrong to terminals).
*)

