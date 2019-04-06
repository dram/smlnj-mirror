functor Interact(structure Machm : CODEGENERATOR
		 structure Importer: IMPORTER
		) : sig end =
struct
(* structure Reopener = Reopen(struct structure C=Machine and Machm=Machm end) *)

  open ErrorMsg Access Basics BareAbsyn Lambda PrintUtil ProcessFile

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

  fun patvars (VARpat(VALvar{access=LVAR v,name,...})) = [v]
    | patvars (VARpat(VALvar{access=INLINE _,name,...})) = []
    | patvars (VARpat _ ) = impossible "non-LVAR in translate.patvars"
    | patvars (RECORDpat{fields,...}) = smash (fn (_,p) => patvars p) fields
    | patvars (APPpat(_,p)) = patvars p
    | patvars (CONSTRAINTpat(p,_)) = patvars p
    | patvars (LAYEREDpat(p,q)) = patvars p @ patvars q
    | patvars _ = []

  fun getvars (VALdec vbl) = smash (fn VB{pat,...} => patvars pat) vbl
    | getvars (a as VALRECdec rvbl) =
	smash (fn RVB{var=VALvar{access=LVAR(var),name,...},exp,...} => [var]
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
	map (fn STRB{strvar=STRvar{name,access=LVAR(v),...},...} => v
	      | _ => impossible "getvars(STRdec)/fn"
	    ) sbl
    | getvars (ABSdec sbl) =
	map (fn STRB{strvar=STRvar{name,access=LVAR(v),...},...} => v
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

    fun parse() =
	let val ref linenum = ErrorMsg.lineNum
	    val timer = start_timer()
	    val absyn = Parse.interdec()
	    val time = check_timer timer
	    val lines = !ErrorMsg.lineNum - linenum
	in  update(System.Stats.parse,time);
	    System.Stats.lines := !System.Stats.lines + lines;
	    timemsg ("parse, " ^ Integer.makestring lines
		     ^ " lines, " ^ makestring time ^ "s")
		orelse debugmsg "parse";
	    if !System.Control.debugging
	    then (PrintAbsyn.printDec(absyn,0,!printDepth); newline())
	    else ();
	    if !anyErrors then raise Stop else ();
	    case absyn
	      of OPENdec(strvars)  =>
	          (Env.restore(); SEQdec(map Misc.dumpStructure strvars))
	       | _ => absyn
	end

    fun translate absyn =
	let val timer = start_timer()
	    val newlvars = getvars absyn
	    val (absyn', profileList) = Prof.instrumDec(absyn)
	    val lambda' = FN(mkLvar(), Translate.transDec absyn' 
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
	    val lambda = if !CGoptions.reduce then Opt.reduce lambda else lambda
	    val _ = if !anyErrors then raise Stop else ()
	    val lambda = if !CGoptions.hoist then Opt.hoist lambda else lambda
	    val time = check_timer timer
	in  update(System.Stats.codeopt,time);
	    timemsg ("codeopt, " ^ makestring time ^ "s")
		orelse debugmsg "codeopt";
	    lambda
	end

    fun codegen lambda =
	let val timer = start_timer()
	    val (size,emit) = Machm.generate lambda
	    val a = System.Unsafe.create_s size
	    val pos = ref 0
	    fun writebyte i = (System.Unsafe.store_s(a,!pos,i); inc pos)
	    val executable =
		  (emit writebyte;
		   if !anyErrors then raise Stop else ();
		   debugmsg "about to boot";
		   (System.Unsafe.boot : 
		      string -> ((int->System.Unsafe.object) ->
				  ((unit -> System.Unsafe.object Array.array) *
				   ByteArray.bytearray Array.array)))
		   a)
	    val time = check_timer timer
	in  update(System.Stats.codegen,time);
	    timemsg ("codegen, " ^ makestring time ^ "s")
		orelse debugmsg "codegen";
	    executable
	end

    fun exec executable =
	let val timer = start_timer()
	    val result = executable lookup
	    val time = check_timer timer
	in  update(System.Stats.execution,time);
	    timemsg ("execution, " ^ makestring time ^ "s")
		orelse debugmsg "execution";
	    result
	end


    (* toplevel loop *)
    (* initialize static environment *)
    val (vCore,vInitial,vMath) = bootEnv load
    val pervasiveEnv = Env.closeCurrentNewEnv()
    val _ =  Env.resetEnv pervasiveEnv
    val _ =  Env.commit()

   (* Build an environment (record) of the interactive compilation functions
      to pass to the Importer, so that it can compile and run things. *)

    val toplevelFns =
       Importer.TOPLEVEL_FNS{bind=bind, lookup=lookup,
			     parse=parse, getvars=getvars, opt=opt
			    }

    fun toploop() =
	let val _ = Lex.toplevel := true
	    val absyn = parse()
         in (case absyn
               of IMPORTdec fnames =>
	             let fun doit f = Importer.getAndExecModule(
				         f, pervasiveEnv, toplevelFns
				      )
		     in
		        app doit fnames
			handle Importer.Import verdict =>
			   (print("IMPORT failed (" ^ verdict ^ ")\n"); raise Stop)
		     end

                | _ => (* normal program *)
		   let val (newlvars,lambda) = translate absyn
		       val oldlvars =
			     EnvAccess.staleLvars(Env.current(),Env.previous())
		       val lambda = opt(Opt.closetop(lambda, ProcessFile.getCore()))
		       val executable = codegen lambda
		       val (result',profile) = exec executable
		       val result = 
			 (System.Control.ProfileInternals.add profile;
			  System.Control.ProfileInternals.setOther ();
			  System.Unsafe.isolate result' before
			  System.Control.ProfileInternals.setToplevel ())
		       fun bindlvars (i,v::r) = (bind(v,result sub i);
						 bindlvars (i+1,r))
			 | bindlvars (_,nil) = ()
		   in  bindlvars(0,newlvars);   (* add new runtime bindings *)
		       app unbind oldlvars;     (* remove stale runtime bindings *)
		       spoolLambda lambda;	     (* save lambda code *)
		       PrintDec.printDec lookup absyn;  (* print result *)
		       Env.consolidate();	     (* consolidate static environment *)
		       Env.commit()     	     (* accept static bindings *)
		   end);
            toploop()
        end

    fun use_file fname =
	let val stream = open_in fname handle e as Io _ =>
			 (print("[cannot open " ^ fname ^ "]\n"); raise e)
	      val _ = print("[opening " ^ fname ^ "]\n")
	      val _ = Lex.pushSource(stream, fname)
	  in  toploop()
	      handle exn =>
		(print("[closing " ^ fname ^ "]\n");
		 close_in stream handle Io _ => ();
		 Lex.popSource();
		 case exn
		   of Parse.Eof => ()
		    | Stop => (Env.restore(); raise Syntax)
		    | _ => (Env.restore(); raise exn))
	  end

    fun use_stream s =
	  let val _ = print("[opening <instream>]\n")
	      val _ = Lex.pushSource(s, "<instream>")
	  in  toploop()
	      handle exn =>
		(print("[closing <instream>]\n");
		 Lex.popSource();
		 case exn
		   of Parse.Eof => ()
		    | Stop => (Env.restore(); raise Syntax)
		    | _ => (Env.restore(); raise exn))
	  end

    (* outer interactive loop, with error handling *)
    fun interact() =
	toploop()
	handle Parse.Eof => ()
	     | f => if !System.interactive
		    then case f
			   of  Stop => restart()
			    | Syntax => restart()
			    | _ => (print("uncaught exception "
					^ System.exn_name f ^ "\n");
				    restart())
		    else (print("uncaught exception "
				^ System.exn_name f ^ "\n");())

    and restart() = (Env.restore(); Lex.flush(); interact())

    (* bind runtime boot structures:  Core, Math, and Initial *)
    val {core,math,initial} = !System.Unsafe.pstruct
    val _ = (bind(vCore,core); bind(vInitial,initial); bind(vMath,math))

    val _ =
      (IO.use_f := use_file;
       IO.use_s := use_stream;
       System.Control.ProfileInternals.setToplevel ();
       Lex.pushSource(std_in,"std_in");  (* initialize scanner *)
       print "Go for it\n";
       interact())

end (* functor Interact *)
