structure RealDebug : DEBUGINTERFACE =
struct
  (* Set up the interactive system interface. *)
  open DebugExec  (* to get datatype 'a result *)
  val init = DebugMotions.runCompUnit
  fun abort() = (DebugMotions.abort(); 
		 DebugUtil.debugPanic "Interface returned from abort")
  fun complete() = (DebugMotions.complete();
		    DebugUtil.debugPanic "Interface returned from complete")
  val commit = DebugStatic.commit
  val rollback = DebugStatic.rollback
  fun instrumAndInstall (file:DebugStatic.filename,absyn:Absyn.dec) =
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
  val printDec = DPrintAbsyn.printDec
  val sizereport = DebugUtil.sizereport
  val hideFile = DebugStatic.hideFile
  val env = DebugUtil.debugEnv
  val looker = DebugEnv.looker
  val blookup = DebugEnv.blookup
  val bclear = DebugEnv.bclear


  (* Set up ref which contains values needed by instrumented code at run time.
     This is not very neatly modularized! *)
  val _ = System.Control.Debug.getDebugf := 
	 (System.Unsafe.cast)  (fn firstEvn =>
		(DebugKernel.times,
		 DebugStatic.eventTimesArray firstEvn,
		 DebugKernel.break,
		 DebugStore.hcreater,
		 System.Unsafe.Weak.weak,
		 DebugStore.updatedRList,
		 Array.array))

  (* Set up values for user-space interface.  
     It is very important that types match those on user-space end,
     since the interface is not type-checked. *)
val _ =
  let val old_interface = !System.Control.Debug.interface
  in System.Control.Debug.interface := 
   (fn 
     (* 1 and 2 reserved for use function -- see build/interact.sml *)
       3 => System.Unsafe.cast DebugMotions.withEstablishedTime
     | 4 => System.Unsafe.cast DebugExec.currentTime
     | 5 => System.Unsafe.cast (fn () => (DebugExec.currentEvn(),0))
     | 6 => System.Unsafe.cast 
	               (fn () => (!DebugExec.initialTime,!DebugExec.finalTime))
     | 7 => System.Unsafe.cast (fn (evn:int,_:int) => DebugStatic.lastTime evn)
     | 8 => System.Unsafe.cast DebugMotions.jump
     | 9 => System.Unsafe.cast DebugMotions.binSearch
     | 10 => System.Unsafe.cast DebugQueries.callTrace
     | 11 => System.Unsafe.cast DebugQueries.getVal
     | 12 => System.Unsafe.cast DebugUtil.printVal
     | 13 => System.Unsafe.cast DebugUtil.isFn
     | 14 => System.Unsafe.cast DebugQueries.printBind 
     | 15 => System.Unsafe.cast (DebugUtil.debugdebug,
				 DebugUtil.sizing)
     | 16 => System.Unsafe.cast (DebugStore.updatedAList,
				 DebugStore.updatedRList,
				 DebugStore.createdList,
				 DebugStore.hcreatea,
				 DebugStore.hcreater)
     | 17 => System.Unsafe.cast DebugStatic.eventPlacesAfter
     | 18 => System.Unsafe.cast DebugStatic.eventPlacesBefore
     | 19 => System.Unsafe.cast (DebugSignals.setHandler,
				 DebugSignals.inqHandler,
				 DebugSignals.maskSignals,
				 DebugSignals.pause)
     | 20 => System.Unsafe.cast DebugMotions.complete
     | 21 => System.Unsafe.cast DebugMotions.abort
     | 22 => System.Unsafe.cast DebugExec.inCompUnit
     | 23 => System.Unsafe.cast (fn () => !DebugExec.blockingExn)
     | 24 => System.Unsafe.cast DebugIO.logit
     | 25 => System.Unsafe.cast DebugQueries.eventDesc
     | 26 => System.Unsafe.cast (DebugRun.minTimeDelta,
				 DebugRun.maxTimeDelta,
				 DebugRun.maxStates)
     | 27 => System.Unsafe.cast DebugKernel.times
     | 28 => System.Unsafe.cast DebugQueries.caller
     | 29 => System.Unsafe.cast DebugUtil.infinity
     | 30 => System.Unsafe.cast DebugEnv.setLookerTimeF
     | 31 => System.Unsafe.cast DebugQueries.atCall
     | q => old_interface q)
  end (* let ... *)

end
