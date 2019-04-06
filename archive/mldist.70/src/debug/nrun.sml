(* NO TIMES VERSION *)

(* DebugNRun - includes ordinary exec, run, queries, motions, etc. *)
signature DEBUG_NRUN =
sig
  val blockingExn: exn option ref
      (* exception blocking current outermost comp unit, if any. *)
  val setCompUnit: (unit -> System.Unsafe.object array) -> 
                      System.Unsafe.object array  DebugUtil.result
  val abortCompUnit: unit ->  'a
  val completeCompUnit: unit -> 'a
  val inCompUnit: bool ref
  type place (* = evn * evindex *) 
  type wherewhich  (* = place * br *)
  val printBind: (wherewhich * int) -> unit
  val getVal: string -> DebugNKernel.br -> (System.Unsafe.object * Basics.ty * wherewhich) option
  val eventDesc: place -> (string * bool * DebugNStatic.location * DebugNStatic.visible) option
  val caller: DebugNKernel.br -> (wherewhich*wherewhich)
  datatype 'a outcome = COMPLETED of 'a
                      | INTERRUPTED of 'a
		      | NOTRUNNING
  val checkInterrupts: (unit -> 'a) -> 'a outcome
end


structure DebugNRun: DEBUG_NRUN =
struct
  open DebugUtil DebugNStatic DebugNKernel DebugNInstrum DebugNBindings
       Access Absyn Basics PrintAbsyn PrintBasics PrintUtil
  structure U = System.Unsafe

  val blockingExn : exn option ref = ref NONE
  val inCompUnit: bool ref = ref false
  val committedBr: br ref = ref nullBr

  (* Setting up thunks to run. *)
  val resultCont: U.object array result cont ref = ref(makeCont())
  fun setCompUnit (f:unit -> U.object array) : U.object array result =
    let fun runit() =
       let val result = f() 
       in inCompUnit := false;
	  committedBr := !currentBr;
	  NORMAL result
       end handle exn => 
	   (blockingExn := SOME exn;
	    break();
	    inCompUnit := false;
	    EXCEPTION exn)
    in callcc (fn cont => (resultCont := cont;
			   callcc (fn c => (callcc (fn c' =>
						    (setUserCont c';
						     throw c ()));
					    throw cont (runit())));
			   currentBr := !committedBr;
			   inCompUnit := true;
			   blockingExn := NONE;
			   continue();
			   (* should return here from auto break at STARTev *)
			   SUSPENDED))
    end
			   

    fun abortCompUnit() =
      (inCompUnit := false;
       throw (!resultCont) ABORTED)
 
    fun completeCompUnit() = 
       (setAllEventBreaks false;
	continue();
	debugPanic "back from complete")

    type wherewhich = place * br

    val eventsAt = eventsFor o evnAt

    fun caller (now:br) : (wherewhich*wherewhich) =
      (* return top of function containing now, caller of this function *)
       let fun lastfunc(br,(evn,lbr)) =
	         if br = nullBr then 
		   (nullBr,evn,0)
		 else
		   let fun f(FNev _::_,c) = (br,evn,c)
			 | f(HANDLEev _::_,c) = (br,evn,c)
			 | f(evt::rest,c) = f(rest,c+1)
			 | f(nil,_) = lastfunc(lbr,evnLbrAt lbr)
		   in f (eventsFor evn,0)
                   end
	   val (br,evn,c) = lastfunc(now,evnLbrAt now)
	   val br' = callerBr(br,c)
	   val evn' = evnAt br'
       in (((evn,c),br),((evn',0),br'))
       end

  fun eventDesc (evn:evn, c:evindex) : 
         (string * bool * location * visible) option =
    let val evt = nth(eventsFor evn,c)
	val (filename,visible) = filenameFor evn
	val charno = locOfEvent evt
    in SOME (eventText evt,evn = 0,(filename,charno),visible)
    end handle Nth => NONE
	     | Evn _ => NONE


  local
     fun split s =
	 let fun sp s =
	     let val pos = index (fn c => c = ".") (explode s)
	     in substring (s,0,pos) :: 
		 sp (substring(s,pos+1, String.length s - (pos+1)))
	     end handle Index => [s]
	 in rev (sp s)
	 end
  in
  fun getVal (n:string) (now:br) : 
                     (U.object * ty * wherewhich) option  =
      let val (br,c,(i,binding)) = findVARCONBind (split n,now,0) 
      in case binding of
	   VARbind(v as VALvar _) =>
	     let val (evn,args) = evnArgsAt br
 	         val bv = nth (nthArgs(evn,c,args),i)
	         val ty = chaseType (br,c) (VARexp(ref v))
	         val ww = ((evn,c),br)
             in SOME(bv,ty,ww)
             end 
	 | VARbind (OVLDvar _) => NONE
	 | CONbind(dc as (DATACON{const,rep,sign,...}))  =>
	     let val (evn,args) = evnArgsAt br
		 val ty = chaseType(br,c) (CONexp dc)
		 val bv = case rep of
	  	  	     VARIABLE _ =>  nth(nthArgs(evn,c,args),i)
			   | VARIABLEc _ => nth(nthArgs(evn,c,args),i)
	   	   	   | _ => U.cast 0 
		                     (* no run-time object exists *)
		 val ww = ((evn,c),br)
	     in SOME(bv,ty,ww)
	     end 
      end handle Env.Unbound => NONE
  end  (* local *)


  val pdepth = 100

  fun printBind (((evn:evn,c:evindex),br:br),indent:int) : unit =
     case nth(eventsFor evn,c) of
       VALev(MARKdec(dec as VALdec(_),_,_)) => 
				  printDec(!debugEnv) (dec,indent,pdepth)
     | VALRECev(MARKdec(dec as VALRECdec(_),_,_)) => 
				  printDec(!debugEnv) (dec,indent,pdepth)
     | FNev(RULE(pat,_)) => 
	  (printPat(!debugEnv) (pat,pdepth); print " <=== ";
	   case hd(eventsAt (callerBr(br,c))) of
	     APPev(APPexp(_,exp)) => printExp(!debugEnv) (exp,indent+8,pdepth)
	   | _ => print "unknown call site")
     | HANDLEev(RULE(pat,_)) =>
	  (printPat(!debugEnv) (pat,pdepth); print " <=== ";
	   case hd(eventsAt (callerBr(br,c))) of
	     RAISEev(MARKexp(RAISEexp exp,_,_)) => printExp(!debugEnv) (exp,indent+8,pdepth)
	   | _ => print "implicit exception")
     | CASEev(exp,RULE(pat,_)) => 
	   ( printPat(!debugEnv) (pat,pdepth); print " <=== "; printExp(!debugEnv) (exp,indent+8,pdepth))
       | _ => debugPanic "bad event type in queries.printBind"

  datatype 'a outcome = COMPLETED of 'a
                      | INTERRUPTED of 'a
		      | NOTRUNNING

  fun checkInterrupts f =
    if !inCompUnit then
      (setIntHand();
       let val r = f()
       in resetIntHand();
	  if !pendingInterrupt then
	    INTERRUPTED r
	  else COMPLETED r
       end)
     else NOTRUNNING

end
