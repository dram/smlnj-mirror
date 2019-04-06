signature DEBUGSTATIC =
sig
  datatype event
   = APPev of Absyn.exp
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
   | NULLev

  type location

  type cud
  val initialCud:cud
  val addCud: cud -> unit
  val eventtimeArray: int -> int array
  val lastTime: int -> int
  val eventFor: int -> event list
  val elbFor: int -> int
  val locFor: int -> location
  val cp2lnfFor: int -> (int -> string*int*int)

  type eventtimescookie
  val saveEventTimes: unit -> eventtimescookie
  val restoreEventTimes: eventtimescookie -> unit
  val zeroEventTimes: unit -> eventtimescookie

  val reset:unit -> unit
end

structure DebugStatic: DEBUGSTATIC =
struct
  open DebugUtil Access Basics Absyn

  datatype event
   = APPev of exp
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
   | NULLev
  
  type location = string * int * int
  
  type cud = {firstev:int, evcount: int, lastbindev: int,
	      events:event list array, elb:int array, 
	      eventtimes: int array, locindex: location array,
              cp2lnf:int->(string*int*int)}
  
  (* initial cud contains backstop NULLev and general IOev *)
  val initialCud = {firstev=0,evcount=2,lastbindev=0,
		    events=array(2,[NULLev]),
		    elb=array(2,0),eventtimes=array(2,0),
		    locindex=array(2,("",0,0)),
		    cp2lnf=(fn x=>("",0,0))}
  val _ = update(#events initialCud,1,[IOev])
  
  (* cud structure *)
  
  structure CudSet = SortedSet (
	   struct
	     type t = cud
	     type k = int
	     fun key ({firstev,...}:cud) : int = firstev
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
	handle _ => debugPanic ("bad timeArray " ^ (makestring evn))
    
    fun lastTime evn  =  (* fetches value from within array *)
      let val {firstev,eventtimes,...} = findp (!cuds, evn)
      in eventtimes sub (evn-firstev)
      end handle _ => debugPanic ("bad lastTime " ^ (makestring evn))
    
    fun eventFor evn =
      let val {firstev,events,...} = findp (!cuds,evn:int)
      in events sub (evn-firstev)
      end handle _ => debugPanic ("bad eventFor " ^ (makestring evn))
    
    fun elbFor evn = 
      let val {firstev,elb,...} = findp (!cuds,evn)
      in elb sub (evn-firstev)
      end handle _ => debugPanic ("bad elbFor " ^ (makestring evn))
    
    fun cp2lnfFor evn =
        #cp2lnf (findp (!cuds,evn))
	     handle _ => debugPanic ("bad cp2lnfFor " ^ (makestring evn))

    fun locFor evn =
      let val {firstev,locindex,...} = findp(!cuds,evn)
      in locindex sub (evn-firstev)
      end handle _ => debugPanic ("bad locFor " ^ (makestring evn))
    

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

end (* structure DebugStatic *)


