structure RealDebug : DEBUGINTERFACE =
struct
  (* Set up the interactive system interface. *)
  open DebugUtil  (* to get datatype 'a result *)
  structure U = System.Unsafe
  val withtime = ref true
      (* kludgy: true for time-travel system, false for no-time system *)
  fun init f = if !withtime then
                  DebugMotions.runCompUnit f
	       else DebugNRun.setCompUnit f
  fun abort() = (if !withtime then
		   DebugMotions.abort()
		 else DebugNRun.abortCompUnit();
		 DebugUtil.debugPanic "Interface returned from abort")
  fun complete() = (if !withtime then
		      DebugMotions.complete()
		    else DebugNRun.completeCompUnit();
		    DebugUtil.debugPanic "Interface returned from complete")
  fun commit () = if !withtime then
                    DebugStatic.commit()
		  else DebugNStatic.commit()
  fun rollback () = if !withtime then
                      DebugStatic.rollback()
                    else DebugNStatic.rollback()
  fun instrumAndInstall (file:string,absyn:Absyn.dec) =
    if !withtime then
      let open DebugInstrum DebugStatic DebugExec
	  val firstEvn = nextEvn()
	  val (lastBindEvn,lastBindTime)  = 
	       if inCompUnit() then
		   (currentEvn(),currentTime())
	       else (firstEvn-1,lastTime (firstEvn-1))
	  val {absyn,evCount,events,elb} =
	      instrumDec{absyn=absyn,
			 firstEvn=firstEvn,
			 lastBindEvn=lastBindEvn,
			 lastBindTime=lastBindTime}
      in install{file=file,
		 firstEvn=firstEvn,
		 evCount=evCount,
		 events=events,
		 elb=elb};
	 absyn
      end
    else (* not !withtime *)
      let open DebugNInstrum DebugNStatic
          val firstEvn = nextEvn()
	  val lastBindEvn = firstEvn - 1
          val {absyn,evCount,events,elb} =
	      instrumDec{absyn=absyn,
			 firstEvn=firstEvn,
			 lastBindEvn=lastBindEvn}
      in install{file=file,
		 firstEvn=firstEvn,
		 evCount=evCount,
		 events=events,
		 elb=elb};
         absyn
      end
  val printDec = DPrintAbsyn.printDec
  val sizereport = DebugUtil.sizereport
  fun hideFile s = if !withtime then
                     DebugStatic.hideFile s
		   else DebugNStatic.hideFile s
  val env = DebugUtil.debugEnv
  val looker = DebugEnv.looker
  val blookup = DebugEnv.blookup
  val bclear = DebugEnv.bclear


  (* Set up ref which contains values needed by instrumented code at run time.
     This is not very neatly modularized! *)
  val _ = System.Control.Debug.getDebugf := 
	   (fn firstEvn =>
	      if !withtime then
		U.cast
		(DebugKernel.times,
		 DebugStatic.eventTimesArray firstEvn,
		 DebugKernel.break,
		 DebugStore.hcreater,
		 U.Weak.weak,
		 DebugStore.updatedRList,
		 Array.array)
	      else 
		U.cast
		(DebugNKernel.currentBr,
		 DebugNStatic.eventBreaksArray firstEvn,
		 DebugNKernel.break,
		 Array.array))

  (* Set up values for user-space interface.  
     It is very important that types match those on user-space end,
     since the interface is not type-checked. *)
val _ =
  let val old_interface = !System.Control.Debug.interface
  in System.Control.Debug.interface := 
   (fn 
     (* 1 and 2 reserved for use function -- see build/interact.sml *)
       3 => U.cast DebugMotions.withEstablishedTime
     | 4 => U.cast DebugExec.currentTime
     | 5 => U.cast 
	    (fn () => if !withtime then
	                (DebugExec.currentEvn(),0)
		      else (DebugNKernel.evnAt (!DebugNKernel.currentBr),0))
     | 6 => U.cast 
	               (fn () => (!DebugExec.initialTime,!DebugExec.finalTime))
     | 7 => U.cast (fn (evn:int,_:int) => DebugStatic.lastTime evn)
     | 8 => U.cast DebugMotions.jump
     | 9 => U.cast DebugMotions.binSearch
     | 10 => U.cast DebugQueries.callTrace
     | 11 => U.cast DebugQueries.getVal
     | 12 => U.cast DebugUtil.printVal
     | 13 => U.cast DebugUtil.isFn
     | 14 => U.cast DebugQueries.printBind 
     | 15 => U.cast DebugUtil.debugdebug
     | 16 => U.cast (DebugStore.updatedAList,
				 DebugStore.updatedRList,
				 DebugStore.createdList,
				 DebugStore.hcreatea,
				 DebugStore.hcreater)
     | 17 => U.cast 
	      (fn x => if !withtime then
	                 DebugStatic.eventPlacesAfter x
		       else DebugNStatic.eventPlacesAfter x)
     | 18 => U.cast
	      (fn x => if !withtime then
	                 DebugStatic.eventPlacesBefore x
		       else DebugNStatic.eventPlacesBefore x)
     | 19 => U.cast (DebugSignals.setHandler,
				 DebugSignals.inqHandler,
				 DebugSignals.maskSignals,
				 DebugSignals.pause)
     | 20 => U.cast (fn () => if !withtime then
				            DebugMotions.complete()
					  else DebugNRun.completeCompUnit())
     | 21 => U.cast (fn () => if !withtime then
				            DebugMotions.abort()
					  else DebugNRun.abortCompUnit())
     | 22 => U.cast (fn () => if !withtime then
				             DebugExec.inCompUnit()
					  else !DebugNRun.inCompUnit)
     | 23 => U.cast (fn () => if !withtime then
				            !DebugExec.blockingExn
                                          else !DebugNRun.blockingExn)
     | 24 => U.cast DebugIO.logit
     | 25 => U.cast (fn x => if !withtime then
				           DebugQueries.eventDesc x
					 else DebugNRun.eventDesc x)
     | 26 => U.cast (DebugRun.minTimeDelta,
				 DebugRun.maxTimeDelta,
				 DebugRun.maxStates)
     | 27 => U.cast DebugKernel.times
     | 28 => U.cast DebugQueries.caller
     | 29 => U.cast DebugUtil.infinity
     | 30 => U.cast DebugEnv.setLookerTimeF
     | 31 => U.cast DebugQueries.atCall
     | 32 => U.cast (fn () => DebugEnv.useSpecial)
     | 33 => U.cast (fn () => if !withtime then 
				            DebugStatic.eventCount 
					  else DebugNStatic.eventCount)
     | 34 => U.cast DebugUtil.sizereport
     | 35 => U.cast withtime
     | 36 => U.cast DebugNKernel.nullBr
     | 37 => U.cast (fn () => !DebugNKernel.currentBr)
     | 38 => U.cast DebugNKernel.continue
     | 39 => U.cast DebugNRun.caller
     | 40 => U.cast DebugNRun.printBind
     | 41 => U.cast DebugNRun.getVal
     | 42 => U.cast 
	    (fn ((evn:int,_:int),state) => DebugNStatic.setEventBreak evn state)
     | 43 => U.cast DebugNStatic.setAllEventBreaks
     | 44 => U.cast DebugNRun.checkInterrupts
     | 45 => U.cast 
	   (fn () => if !withtime then
	               DebugInstrum.instrumLevel
		     else DebugNInstrum.instrumLevel)
     | 46 => U.cast (fn () => DebugRun.memoLevel)
     | 47 => U.cast (fn () => DebugRun.rankBase)
     | 48 => U.cast DebugRun.dumpCache
     | q => old_interface q)
  end (* let ... *)

end
