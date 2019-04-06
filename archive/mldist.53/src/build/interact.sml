(* Copyright 1989 by AT&T Bell Laboratories *)
functor Interact(structure Machm : CODEGENERATOR
		 structure Importer: IMPORTER
		 structure D: DEBUGINTERFACE
		) : sig end =
struct

  open ErrorMsg Access Basics BareAbsyn Lambda PrintUtil ProcessFile NewParse

(*  structure Reopen = Reopen(structure Machm = Machm)*)

  structure CGoptions = System.Control.CG
    fun debugmsg  (msg : string) =
	let val printit = !System.Control.debugging
	in  if printit then (print msg; print "\n")
	    else ();
	    printit
	end

  datatype debuglevel = NODEBUG | FULLDEBUG of string | LIVEDEBUG of string

  exception Stop
  val printDepth = System.Control.Print.printDepth
  val saveLambda = System.Control.saveLambda
  val bucket = ref (Lambda.RECORD [])
  val _ = System.Control.prLambda :=
     fn () => (MCprint.printLexp (!bucket); newline())
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

 (* functions for retrieving new bound lvars from declaration abstract syntax *)

  fun smash f l = fold (fn (a,c) => f a @ c) l []

  fun patvars (VARpat(VALvar{access=LVAR v,...})) = [v]
    | patvars (VARpat(VALvar{access=INLINE _,...})) = []
    | patvars (VARpat _ ) = impossible "non-LVAR in translate.patvars"
    | patvars (RECORDpat{fields,...}) = smash (fn (_,p) => patvars p) fields
    | patvars (APPpat(_,p)) = patvars p
    | patvars (CONSTRAINTpat(p,_)) = patvars p
    | patvars (LAYEREDpat(p,q)) = patvars p @ patvars q
    | patvars _ = []

  fun getvars (VALdec vbl) = smash (fn VB{pat,...} => patvars pat) vbl
    | getvars (a as VALRECdec rvbl) =
	smash (fn RVB{var=VALvar{access=LVAR(var),...},exp,...} => [var]
	        | _ => impossible "#738 in translate")
	      rvbl
    | getvars (LOCALdec (localdec,visibledec)) = getvars visibledec
    | getvars (EXCEPTIONdec ebl) =
	map (fn EBgen{exn=DATACON{rep=VARIABLE(LVAR v),...},...} => v
	      | EBdef{exn=DATACON{rep=VARIABLE(LVAR v),...},...} => v
	      | _ => impossible "in getvars EXCEPTIONdec")
	    ebl
    | getvars (SEQdec decl) = smash getvars decl
    | getvars (DATATYPEdec _) = []
    | getvars (ABSTYPEdec{body,...}) = getvars body
    | getvars (TYPEdec _) = []
    | getvars (STRdec sbl) =
	map (fn STRB{strvar=STRvar{access=LVAR(v),...},...} => v
	      | _ => impossible "getvars(STRdec)/fn"
	    ) sbl
    | getvars (ABSdec sbl) =
	map (fn STRB{strvar=STRvar{access=LVAR(v),...},...} => v
	      | _ => impossible "getvars(ABSdec)/fn"
	    ) sbl
    | getvars (FCTdec fbl) =
	map (fn FCTB{fctvar=FCTvar{name,access=LVAR(v),...},...} => v
	      | _ => impossible "getvars(FCTdec)/fn"
	    ) fbl
    | getvars (OPENdec _) = []
    | getvars (SIGdec _) = []
    | getvars (IMPORTdec _) = impossible "getvars(IMPORTdec)"
    | getvars (MARKdec (dec,_,_)) = getvars dec

    open System.Timer
    val update = System.Stats.update

    (* set up top-level runtime environment, represented as intmap *)
    exception Runbind
    val t = Intmap.new(32, Runbind) : System.Unsafe.object Intmap.intmap
    val bind = Intmap.add t   (* add runtime binding *)
    val unbind = Intmap.rem t (* remove runtime binding *)
    val _ = System.Unsafe.lookup_r := Intmap.map t
    val lookup = System.Unsafe.lookup

    exception Eof

    (* toplevel loop *)
    (* initialize static environment *)
    val (vCore,vInitial,vMath) = bootEnv load
    val pervasiveEnv = Env.closeCurrentNewEnv()
    val _ =  Env.resetEnv pervasiveEnv
    val _ =  Env.commit()

    fun toploop(parser,inputSource as {anyErrors,lineNum,...}:inputSource,
		dbglevel) =
        let 
	    fun translate absyn =
		let val timer = start_timer()
		    val newlvars = getvars absyn
		    val (absyn', profileList) = Prof.instrumDec(absyn)
		    val lambda' = FN(mkLvar(), Translate.transDec inputSource absyn' 
				    (Lambda.RECORD (map Lambda.VAR newlvars)))
		    val lambda = Prof.bindLambda(lambda',profileList)
		    val time = check_timer timer
		 in update(System.Stats.translate,time);
		    timemsg ("translate, " ^ makestring time ^ "s")
			orelse debugmsg "translate";
		    if !anyErrors then raise Stop else ();
		    (newlvars, lambda)
		end
	
	    fun opt lambda =
		let val timer = start_timer()
		    val lambda = if !CGoptions.reduce then Opt.reduce lambda
				 else lambda
		    val _ = if !anyErrors then raise Stop else ()
		    val lambda = if !CGoptions.hoist then Opt.hoist lambda
				 else lambda
		    val time = check_timer timer
		in  update(System.Stats.codeopt,time);
		    timemsg ("codeopt, " ^ makestring time ^ "s")
			orelse debugmsg "codeopt";
		    lambda
		end
	
	    fun codegen lambda =
		let (*val lambda = if !System.Control.reopen
					then Reopen.instrument lambda
					else lambda*)
		    val timer = start_timer()
		    fun complain s = ErrorMsg.error inputSource (!lineNum,!lineNum)
					COMPLAIN ("real constant out of range: "^s)
		    val code = Machm.generate lambda
				handle Machm.BadReals l => (app complain l; raise Stop)
		    val _ = System.Unsafe.flush_cache code;
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
		let val timer = start_timer()
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
			  | D.ABORTED => 
				raise Stop)
		      | LIVEDEBUG _ =>
			 (case D.init f of 
			    D.SUSPENDED =>
				(print "[ready to execute under debugger]\n";
     		                 interact (); (* return only via ctrl/d *)
     		                 D.abort())
		          | D.NORMAL r =>
			        (print "[completing normal execution]\n";
     		         	 D.commit();
     		         	 r) 
		          | D.EXCEPTION e => 
				(print "[execution terminated by exception]\n";
     		                 raise e)
		          | D.ABORTED => 
				(print "[execution aborted]\n";
    	                         raise Stop))
		    val time = check_timer timer
		in  update(System.Stats.execution,time);
		    timemsg ("execution, " ^ makestring time ^ "s")
			orelse debugmsg "execution";
		    result
		end

	    val toplevelFns =
			Importer.TOPLEVEL_FNS{bind=bind, lookup=lookup,
			     getvars=getvars, opt=opt
			    }

	    fun debug_instrument absyn =
                let val dopos = filepos inputSource
		    fun dump label absyn = 
		       if !System.Control.debugging then
			 (print ("\n" ^ label ^ "\n");
		          PrintAbsyn.printDec(absyn,0,1000); print "\n";
		          D.printDec(absyn,dopos,0,1000);
			  print "\n")
     		       else ()
		    val _ = dump "BEFORE:" absyn	
		    val absyn' =
		      case dbglevel of
			NODEBUG => absyn
		      | FULLDEBUG _ => D.instrumDec(absyn,dopos)
		      | LIVEDEBUG _ => D.instrumDec(absyn,dopos)
		 in 
		   dump "AFTER:" absyn';
		   absyn'
		 end

	    val (oldenv, oldenv') = (Env.openScope(), Env.current())
	    val (absyn0,newenv) = (parser(),
				   Env.current() before 
					(Env.resetEnv oldenv; Env.openScope()))
			handle e => (Env.resetEnv oldenv; raise e)
         in case absyn0
               of ABORT => (Env.resetEnv oldenv; raise Stop)
		| EOF => raise Eof
		| ERROR => (Env.resetEnv oldenv; raise Stop)
		| PARSE(IMPORTdec fnames) =>
	             let fun doit f = Importer.getAndExecModule(
				         f, pervasiveEnv, toplevelFns)
		     in app doit fnames
			handle Importer.Import verdict =>
			   (print("IMPORT failed (" ^ verdict ^ ")\n");
			    raise Stop)
		     end
                | PARSE absyn => (* normal program *)
		   let val printdec = PrintDec.printDec absyn;
		       val absyn' = debug_instrument absyn
		       val (newlvars,lambda) = translate absyn'
		       val oldlvars =  EnvAccess.staleLvars(newenv,oldenv)
		       val lambda = opt(Opt.closetop(lambda, 
						ProcessFile.getCore()))
		       val executable =
			    if !System.Control.interp then Interp.interp lambda
				else codegen lambda
		       val isolate = if !System.Control.Debug.debugging
					then fn x => x
					else System.Unsafe.isolate
		       val (result',profile) = isolate executable lookup
		       val result = 
			 (System.Control.ProfileInternals.add profile;
			  System.Control.ProfileInternals.setOther ();
			  perform (isolate result') before
			  System.Control.ProfileInternals.setToplevel ())
		       fun bindlvars (i,v::r) = (bind(v,result sub i);
						 bindlvars (i+1,r))
			 | bindlvars (_,nil) = ()
		   in  Env.splice(Env.current(),oldenv');
		       Env.resetEnv newenv;
		       bindlvars(0,newlvars);   (* add new runtime bindings *)
		       app unbind oldlvars; (* remove stale runtime bindings *)
		       (* spoolLambda lambda;	     save lambda code *)
		       printdec lookup;
		       Env.consolidate()  (* consolidate static environment *)
		   end;
            toploop(parser,inputSource,dbglevel)
        end

    (* interactive loop, with error handling *)
    and interact ()  = 
      let val inputSource = ErrorMsg.newSource("std_in",std_in,true,std_out);
	  val parser = NewParse.parse inputSource
          fun restart() = (input std_in (can_input std_in);
			   interact ())
       in toploop(parser,inputSource,NODEBUG)
	  handle Eof => ()
	     | f => if is_term_in std_in
		    then case f
			   of  Stop => restart()
			    | Syntax => restart()
			    | Io s => (print("uncaught exception Io \""
					^ s ^ "\"\n");
				    restart())
			    | _ => (print("uncaught exception "
					^ System.exn_name f ^ "\n");
				    restart())
		    else (print("uncaught exception "
				^ System.exn_name f ^ "\n");())
      end

    fun use_source dbglevel (fname,stream) =
	let   val _ = print("[opening " ^ fname ^ "]\n")
              val interactive = is_term_in stream
	      val inputSource = ErrorMsg.newSource(fname,stream,interactive,std_out);
	      val parser = NewParse.parse inputSource
	      val oldenv = Env.current()
	      fun opendbg s = EnvAccess.openStructureVar
				(EnvAccess.lookSTR (Symbol.symbol s))
 	      val _ = case dbglevel of
			FULLDEBUG s => opendbg s
		      | LIVEDEBUG s => opendbg s
		      | NODEBUG => ()
	      val oldenv' = (Env.openScope(); Env.current())
	  in  toploop(parser,inputSource,dbglevel)
	      handle exn =>
		(print("[closing " ^ fname ^ "]\n");
		 Env.splice(oldenv,oldenv');
		 close_in stream handle Io _ => ();
		 case exn
		   of Eof => ()
		    | Stop => raise Syntax
		    | _ => raise exn)
	  end

    fun use_file dbglevel fname =
         use_source dbglevel (fname,(open_in fname handle e as Io _ =>
			   (print("[cannot open " ^ fname ^ "]\n"); raise e)))

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
