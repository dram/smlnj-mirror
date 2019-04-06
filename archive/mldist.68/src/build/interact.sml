(* Copyright 1989 by AT&T Bell Laboratories *)
functor Interact(structure Machm : CODEGENERATOR
		 structure Importer: IMPORTER
		 structure D: DEBUGINTERFACE
		) : sig end =
struct

  open ErrorMsg Access Basics BareAbsyn PrintUtil ProcessFile NewParse

(*  structure Reopen = Reopen(structure Machm = Machm) *)

  structure CGoptions = System.Control.CG
    fun debugmsg  (msg : string) =
	let val printit = !System.Control.debugging
	in  if printit then (print msg; print "\n")
	    else ();
	    printit
	end

  datatype debuglevel = NODEBUG | FULLDEBUG of string 
				| LIVEDEBUG of string*string

  exception Stop 
  exception INTERRUPT
  val printDepth = System.Control.Print.printDepth
  val saveLambda = System.Control.saveLambda
  val bucket = ref (Lambda.RECORD [])
  val _ = System.Control.prLambda :=
     (fn () => (MCprint.printLexp (!bucket); newline()))
  fun spoolLambda l = if !saveLambda then bucket := l else ()
  val lambda = ref (Lambda.RECORD [])
  val vars = ref (nil : int list)
  fun prLambda () = (MCprint.printLexp(!lambda); newline())
  fun prFun lv = (MCprint.printFun(!lambda) lv; newline())
  fun timemsg (s : string) =
      let val printit = !System.Control.timings
       in if printit then (print s; newline()) else ();
	  printit
      end

 open Linkage
    open System.Timer
    val update = System.Stats.update

    exception Eof

    (* toplevel loop *)
    (* initialize static environment *)
    val (stdPervEnv,(vCore,vInitial,vMath)) = bootEnv load
    
    (* these are refs because we can call toploop from use *)
    val topEnv = ref(Env.empty: Basics.env)           (* environment toploop adds to *)
    val baseEnv = ref stdPervEnv     (* environment toploop assumes below *)
    val lookupLvar = ref lookup      (* global lvar lookup function *)
    val _ = D.env := stdPervEnv      (* for debugger debugging *)

    (* interesting cases:
       (outer loop): baseEnv = std. pervasives
                   topEnv = everything else so far
		   lookup = std lookup
       (debugger use): baseEnv = dbg. pervasives
                   topEnv = everything else so far
		   lookup = std lookup
       (inner loop): baseEnv = std. pervasives + everything prior to inner loop
                                    + special
		     lookup = std lookup + special lookup
		     topEnv = everything defined in inner loop.
    *)
   
    fun toploop(parser,inputSource as {anyErrors,lineNum,...}:inputSource,
		dbglevel) =
        let 
   	    val _ = 
 		 (System.Unsafe.toplevelcont := 
			callcc(fn k => (callcc(fn k' => (throw k k'));
		  		        raise INTERRUPT)))

	    fun sizereport string  (* available for debugging *) =
		(case dbglevel of 
		   NODEBUG => ()
	 	 | _ => D.sizereport string)

	    fun translate env absyn =
		let val _ = sizereport "translate"
		    val timer = start_timer()
		    val newlvars = getvars absyn
		    val (absyn', profileList) = Prof.instrumDec(absyn)
		    val lambda' = Lambda.FN(mkLvar(), Translate.transDec env inputSource absyn' 
				    (Lambda.RECORD (map Lambda.VAR newlvars)))
		    val lambda = Prof.bindLambda(lambda',profileList)
		    val time = check_timer timer
		 in update(System.Stats.translate,time);
		    timemsg ("translate, " ^ makestring time ^ "s")
			orelse debugmsg "translate";
		    if !anyErrors then raise Stop else ();
		    (newlvars, lambda)
		end
	
	    fun convert lambda =
		let val _ = sizereport "opt"
		    val timer = start_timer()
		    val function =  Convert.convert(Reorder.reorder lambda)
		    val _ = if !anyErrors then raise Stop else ()
		    val time = check_timer timer
		in  update(System.Stats.convert,time);
		    timemsg ("convert, " ^ makestring time ^ "s")
			orelse debugmsg "convert";
		    function
		end

	    fun codegen lambda =
		let (* val lambda = if !System.Control.reopen
					then Reopen.instrument lambda
					else lambda *)
		    val _ = sizereport "codegen"
		    val timer = start_timer()
		    val complainer = ErrorMsg.error inputSource (!lineNum,!lineNum)
		    val code = Machm.generate(convert lambda,NONE,complainer)
		    val _ = if !anyErrors then raise Stop else ()
		    val _ = System.Unsafe.CInterface.flush_cache code;
		    val executable =
			  (debugmsg "about to boot";
			   debugmsg ("code size =" ^ 
			     Integer.makestring(String.size(System.Unsafe.cast code)));
			   (System.Unsafe.boot : 
			      string -> ((int->System.Unsafe.object) ->
					  ((unit -> System.Unsafe.object Array.array) *
					   ByteArray.bytearray Array.array)))
			   code)
		    val time = check_timer timer
		in  update(System.Stats.codegen,time);
		    timemsg ("codegen, " ^ makestring time ^ "s")
			orelse debugmsg "codegen";
		    executable
		end
	
	    fun perform f =
		let val _ = sizereport "perform"
		    val timer = start_timer()
		    val result = 
		      case dbglevel of
			  NODEBUG => f()				       
	  	      | FULLDEBUG _ =>
	                 (case D.init f of
		            D.SUSPENDED => 
				(D.Xcomplete(); (* no return *)
				 raise Stop (* !! *))
	   	          | D.NORMAL r =>  
				(D.commit(); r)
			  | D.EXCEPTION e => 
				raise e
			  | D.ABORTED => (* shouldn't happen *)
				raise Stop
			  | D.INTERRUPTED =>
				raise INTERRUPT)
		      | LIVEDEBUG _ =>
			 let val saveEnv = !topEnv
			     fun reset() =(topEnv := Env.atop(!topEnv,saveEnv);
					   baseEnv := stdPervEnv;
					   lookupLvar := lookup;
					   D.bclear())
			 in baseEnv := Env.special(D.looker,Env.atop(saveEnv,stdPervEnv));
			                       
			    topEnv := Env.empty;
			    lookupLvar := (fn x => (case D.blookup x of
						     SOME x => x
						   | NONE => lookup x));
			    case D.init f of 
			      D.SUSPENDED =>
				(print "[ready to execute under debugger]\n";
				 interact();
				 (* return only via ctrl/d *)
     		                 D.Xabort();
				 reset();
				 raise Stop (*!!*))
		            | D.NORMAL r =>
			        (print "[completing normal execution]\n";
     		         	 D.commit();
				 reset();
     		         	 r) 
		            | D.EXCEPTION e => 
				(print "[execution terminated by exception]\n";
				 reset();
     		                 raise e)
		            | D.ABORTED => 
				(print "[execution aborted]\n";
				 reset();
                                 raise Stop)
			    | D.INTERRUPTED =>
				(print "[execution interrupted]\n";
				 reset();
				 raise INTERRUPT)
                         end
		    val time = check_timer timer
		in  update(System.Stats.execution,time);
		    timemsg ("execution, " ^ makestring time ^ "s")
			orelse debugmsg "execution";
		    result
		end

	    fun debug_instrument env absyn =
                let val dopos = filepos inputSource
		    val (filename,_,_) = dopos 1
		    fun dump label absyn = 
		       if !System.Control.debugging andalso
			  !System.Control.Debug.debugging then
			 (print ("\n" ^ label ^ "\n");
		          PrintAbsyn.printDec env (absyn,0,1000); print "\n";
		          D.printDec env (absyn,dopos,0,1000);
			  print "\n")
     		       else ()
		    val _ = dump "BEFORE:" absyn
		    val _ = sizereport "instrument"
		    val timer = start_timer ()
		    val absyn' =
		      case dbglevel of
			NODEBUG => absyn
		      | FULLDEBUG _ => D.instrumDec(filename,absyn)
		      | LIVEDEBUG _ => D.instrumDec(filename,absyn)
		    val time = check_timer timer
		 in 
		   (*update(System.Stats.debuginstrum,time);*)
		   timemsg ("debug instrument, " ^ makestring time ^ "s")
			orelse debugmsg "debug instrument";
		   dump "AFTER:" absyn';
		   absyn'
		 end

	    val _ = sizereport "parse"
	    val absyn0 = parser (Env.atop(!topEnv,!baseEnv))
	in
	    topEnv := 	  	      
             (case absyn0
               of ABORT => raise Stop
		| EOF => raise Eof
		| ERROR => raise Stop
		| PARSE(IMPORTdec fnames,_) =>
		   let fun f (fname,accenv) =
 			   Env.atop(Importer.getAndExecModule stdPervEnv fname, accenv)
		       val envi = revfold f fnames Env.empty
		                        handle Importer.Import =>
		                         (print "IMPORT failed\n"; raise Stop)
		   in Env.consolidate(Env.atop(envi,!topEnv))
		   end
                | PARSE (absyn,envr) => (* normal program *)
		   let val fullEnv = Env.atop(envr,Env.atop(!topEnv,!baseEnv))
		       val oldlvars = 
			    EnvAccess.staleLvars(envr,Env.atop(!topEnv,!baseEnv))
		       val printdec = PrintDec.printDec fullEnv absyn
		       val absyn' = debug_instrument fullEnv absyn
		       val (newlvars,lambda) = translate fullEnv absyn'
		       val lambda = Opt.closetop(lambda, ProcessFile.getCore())
		       val executable =
			    if !System.Control.interp then Interp.interp lambda
				else codegen lambda
		       val isolate = if !System.Control.Debug.debugging
					then fn x => x
					else System.Unsafe.isolate
		       val (result',profile) = isolate executable (!lookupLvar)
		       val result = 
			 (System.Control.ProfileInternals.add profile;
			  System.Control.ProfileInternals.setOther ();
			  perform (isolate result') before
			  System.Control.ProfileInternals.setToplevel ())
			   (* N.B. performing a use causes topEnv to change *)
		   in  bindLvars(newlvars,result); (*add new runtime bindings*)
		       app unbind oldlvars; (* remove stale runtime bindings *)
		       (* spoolLambda lambda;	     save lambda code *)
		       printdec (!lookupLvar);
		       Env.consolidate(Env.atop(envr,!topEnv))
		   end);
            toploop(parser,inputSource,dbglevel)
        end

    (* interactive loop, with error handling *)
    and interact ()  = 
      let val inputSource = ErrorMsg.newSource("std_in",std_in,true,std_out,NONE);
	  val parser = NewParse.parse inputSource
          fun restart() = (input(std_in,(can_input std_in))
			     handle Io _ => "";
			   interact ())
      in
         (toploop(parser,inputSource,NODEBUG))
	  handle Eof => ()
  	     | INTERRUPT => (print "\nInterrupt\n"; restart())
	     | f => if true (* is_term_in std_in *)
		    then case f
			   of  Stop => restart()
			    | Syntax => restart()
			    | Cascade s => (ErrorMsg.error inputSource
					    (!(#lineNum inputSource),
					     !(#lineNum inputSource))
						CASCADE s 
					    handle Syntax => restart())
			    | Io s => (print("\nuncaught exception Io \""
					^ s ^ "\"\n");
				    restart())
			    | _ => (print("\nuncaught exception "
					^ System.exn_name f ^ "\n");
				    restart())
		    else (print("\nuncaught exception "
				^ System.exn_name f ^ "\n");())
      end (* interact *)

    fun use_source dbglevel (fname,stream) =
      (* N.B. If called from within inner debugging interactive loop, 
              dbglevel MUST be NODEBUG. *)
	let val _ = print("[opening " ^ fname ^ "]\n")
            val interactive = is_term_in stream
	    val inputSource = 
		ErrorMsg.newSource(fname,stream,interactive,std_out,
					   if not interactive 
					       then Index.openIndexFile fname
					       else NONE)
	    val parser = NewParse.parse inputSource
	    fun debugPervEnv s = EnvAccess.openStructureVar stdPervEnv
			(EnvAccess.lookSTR (!topEnv) (Symbol.strSymbol s))
            fun hidefile () = if not interactive then
			          D.hideFile fname 
			      else ()
	in (case dbglevel of 
	      FULLDEBUG d => (hidefile(); baseEnv := debugPervEnv d)
	    | LIVEDEBUG (d,_) => (hidefile(); baseEnv := debugPervEnv d)
	    | NODEBUG => ());
	   toploop(parser,inputSource,dbglevel)
	      handle exn =>
 	        ((case dbglevel of 
		    FULLDEBUG _ => baseEnv := stdPervEnv
		  | LIVEDEBUG _ => baseEnv := stdPervEnv
		  | NODEBUG => ());
		 case exn
		   of Eof => ErrorMsg.closeSource inputSource
		    | Cascade s => 
			  (ErrorMsg.error inputSource
			     (!(#lineNum inputSource), !(#lineNum inputSource))
			     CASCADE s;
			   ErrorMsg.closeSource inputSource)
		    | Stop => (ErrorMsg.closeSource inputSource;
			       raise Syntax)
		    | _ => (ErrorMsg.closeSource inputSource; raise exn))
	  end

    fun use_file dbglevel fname =
         use_source dbglevel (fname,(open_in fname handle Io s =>
			   (print("[use failed: "^s^"\n");
			    raise Syntax)))

    fun use_stream dbglevel s = use_source dbglevel ("<instream>",s)

    (* bind runtime boot structures:  Core, Math, and Initial *)
    val {core,math,initial} = !System.Unsafe.pstruct
    val _ = (bind(vCore,core); bind(vInitial,initial); bind(vMath,math))

    val _ =
      (System.Unsafe.use_f := use_file NODEBUG;
       System.Unsafe.use_s := System.Unsafe.cast (use_stream NODEBUG);
       let val old_interface = !System.Control.Debug.interface
       in  
         System.Control.Debug.interface :=
           (fn 
             1 => System.Unsafe.cast use_file
	   | 2 => System.Unsafe.cast use_stream
           | q => old_interface q)
       end;
       System.Control.ProfileInternals.setToplevel ();
       print "Go for it\n";
       interact ())

end (* functor Interact *)
