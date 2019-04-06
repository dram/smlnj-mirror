signature DEBUGSTATIC =
sig
  datatype event
   = APPev of Absyn.exp
   | RAISEev of Absyn.exp
   | HANDLEev of Absyn.rule
   | FNev of Absyn.rule
   | CASEev of Absyn.exp * Absyn.rule
   | VALev of Absyn.dec
   | VALRECev of Absyn.dec
   | STRev of Absyn.dec * (int list)
   | ABSev of Absyn.dec * (int list)
   | FCTev of Absyn.dec * (int list)
   | FCTENTev of Absyn.fctb
   | FCTAPPev of Absyn.strexp
   | STRENDev of Absyn.strexp
   | STRVARev of Absyn.strexp
   | OPENev of Absyn.dec
   | LETev of Absyn.exp
   | LOCALev of Absyn.dec
   | LOCINev of Absyn.dec
   | LOCENDev of Absyn.dec
   | IOev
   | STARTev of Absyn.dec (* entire compilation unit *)
   | ENDev of Absyn.dec (* entire compilation unit *)
   | NULLev

  val eventText: event -> string

  type filename
  type charno
  type location
  type visible
  type evnum
  type evindex

  type cud
  exception Evnum of evnum
  val initialCud:cud
  val addCud: cud -> unit
  val eventtimeArray: evnum -> int array
  val lastTime: evnum -> int
  val eventsFor: evnum -> event list
  val elbFor: evnum -> evnum
  val filenameFor: evnum -> filename * visible
  val hideCud: cud -> unit


  type eventtimescookie
  val saveEventTimes: unit -> eventtimescookie
  val restoreEventTimes: eventtimescookie -> unit
  val zeroEventTimes: unit -> eventtimescookie

  val reset:unit -> unit
end

structure DbgStat: DEBUGSTATIC =
struct
  open DbgUtil Access Basics Absyn

  datatype event
   = APPev of exp
   | RAISEev of exp
   | HANDLEev of rule
   | FNev of rule
   | CASEev of exp * rule
   | VALev of dec
   | VALRECev of dec
   | STRev of dec * (int list)
   | ABSev of dec * (int list)
   | FCTev of dec * (int list)
   | FCTENTev of Absyn.fctb
   | FCTAPPev of Absyn.strexp
   | STRENDev of Absyn.strexp
   | STRVARev of Absyn.strexp
   | OPENev of dec
   | LETev of Absyn.exp
   | LOCALev of Absyn.dec
   | LOCINev of Absyn.dec
   | LOCENDev of Absyn.dec
   | IOev
   | STARTev of Absyn.dec
   | ENDev of Absyn.dec
   | NULLev
  
  fun eventText (evt:event) : string =
       case evt of
	 VALev(_) => "VAL"
       | VALRECev(_) => "VALREC"
       | FNev(_) => "FN"
       | CASEev(_) => "CASE"
       | APPev(_) => "APP"
       | RAISEev(_) => "RAISE"
       | HANDLEev(_) => "HANDLE"
       | STRev(_) => "STRUCTURE"
       | ABSev(_) => "ABSTRACTION"
       | FCTev(_) => "FUNCTOR"
       | FCTENTev(_) => "FUNCTOR ENTRY"
       | FCTAPPev(_) => "FUNCTOR APP"
       | STRENDev(_) => "STRUCTURE END"
       | STRVARev(_) => "STRUCTURE VAR"
       | OPENev(_) => "OPEN"
       | LETev(_) => "LET"
       | LOCALev(_) => "LOCAL"
       | LOCINev(_) => "LOCAL IN"
       | LOCENDev(_) => "LOCAL END"
       | IOev => "IO"
       | STARTev(_) => "START"
       | ENDev(_) => "END"
       | NULLev => "NULL"

  type filename = string
  type charno = int  (* counting from 1 *)
  type location = filename * charno
  type visible = bool (* true if file has not been hidden by reusing *)
  type evnum = int
  type evindex = int

  type cud = {file:filename,visible:visible ref,
	      firstev:evnum, evcount: int, lastbindev: evnum,
	      events:event list array, elb:int array, 
	      eventtimes: int array}
  
  exception Evnum of evnum

  (* initial cud contains backstop NULLev and general IOev *)
  val initialCud = {file="",visible=ref true,
		    firstev=0,evcount=2,lastbindev=0,
		    events=array(2,[NULLev]),
		    elb=array(2,0),eventtimes=array(2,0)}

  val _ = update(#events initialCud,1,[IOev])
  
  (* cud structure *)
  
  structure CudSet = SortSet (
	   struct
	     type t = cud
	     type k = evnum
	     fun key ({firstev,...}:cud) : evnum = firstev
	     val lt = Integer.<
	   end)
  
  local 
      open CudSet
      val cuds = ref (insert(new(),initialCud)) 
  in
    fun resetCuds () = cuds := insert(new(),initialCud)
    
    fun addCud (cud as {firstev,...}:cud) =
	 ((cuds := delete (!cuds,firstev)) handle NotFound => ();
	  cuds := insert (!cuds,cud))	
    
    fun eventtimeArray evn = 
      #eventtimes (find (!cuds,evn))
	handle _ => raise (Evnum evn)
    
    fun lastTime evn  =  (* fetches value from within array *)
      let val {firstev,eventtimes,...} = findp (!cuds, evn)
      in eventtimes sub (evn-firstev)
      end handle _ => raise (Evnum evn)
    
    fun eventsFor evn =
      let val {firstev,events,...} = findp (!cuds,evn:int)
      in events sub (evn-firstev)
      end handle _ => raise (Evnum evn)
    
    fun elbFor evn = 
      let val {firstev,elb,...} = findp (!cuds,evn)
      in elb sub (evn-firstev)
      end handle _ => raise (Evnum evn)
    
    fun filenameFor evn =
      let val {file,visible,...} = findp (!cuds,evn)
      in (file,!visible)
      end handle _ => raise (Evnum evn)

    fun hideCud cud = #visible(cud:cud) := false

    type eventtimescookie = (int * int list) list
    
(*    fun dumpEventTimesCookie (cookie:eventtimescookie) =
      let fun prl f l =
	let fun prr (e::nil) = f e
	      | prr (e::r) = (f e; print ","; prr r)
	      | prr nil = ()
	in print "["; prr l; print "]"
	end
      in prl (fn (i,l) => (print "("; print i; print ","; prl Integer.print l;
			    print ")")) cookie; print "\n"
      end *)
				    
    fun saveEventTimes() =  (* returns cookie from list of arrays *)
(*      let val a = *)
      fold (!cuds,
	    fn ({firstev,eventtimes,...},evts) => 
		    (firstev,listofarray eventtimes)::evts,nil)
(*      in print "saving event times: \n"; dumpEventTimesCookie a; a end *)

    fun restoreEventTimes c = (* restores cookie into list of arrays *)
      let fun updarr(a,l) =
	let fun f n (e::r) = (Array.update(a,n,e); f (n+1) r)
	      | f n nil = ()
	in f 0 l
	end
      in(* print "restoring event times: "; dumpEventTimesCookie c; *)
	 (app (fn (firstev,l) => updarr(#eventtimes(find(!cuds,firstev)),l)) c)
	    handle NotFound => debugPanic "bad restoreEventTimes"
      end
     
    fun zeroEventTimes () = (* returns cookie representing times all 0 *)
      let fun zerolist 0 = nil
	    | zerolist n = 0 :: (zerolist (n-1))
      in
	fold (!cuds, fn ({firstev,evcount,...},evts) => 
		    (firstev,zerolist evcount)::evts,nil)
      end
  end (* let open structure CudSet *)

  fun reset() = resetCuds()

end (* structure DbgStat *)


