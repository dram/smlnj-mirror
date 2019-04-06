functor Interact(Machm : CODEGENERATOR) : sig end =
struct

(* structure Reopener = Reopen(struct structure C=Machine and Machm=Machm end) *)

  open ErrorMsg Access Basics BareAbsyn Lambda PrintUtil ProcessFile

  exception Stop
  val printDepth = System.Control.Print.printDepth
  val saveLambda = System.Control.saveLambda
  val bucket = ref (Lambda.RECORD [])
  val _ = System.prLambda := fn () => (MCprint.printLexp (!bucket); newline())
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

  fun smash f =
      let fun loop nil = (nil,nil)
	    | loop (a::r) = let val (x,y) = f a
				val (x',y') = loop r
			    in (x@x', y@y')
		            end
      in loop
      end

  val getVARvars = EnvAccess.getVARvars
  val getSTRvars = EnvAccess.getSTRvars
  val getFCTvars = EnvAccess.getFCTvars

  fun patvars (VARpat(VALvar{access=LVAR v,name,...})) = (tl(getVARvars name), [v])
    | patvars (VARpat(VALvar{access=INLINE _,name,...})) = 
	(tl(getVARvars name), nil)
    | patvars (VARpat _ ) = impossible "non-LVAR in translate.patvars"
    | patvars (RECORDpat{fields,...}) = smash (fn (_,p) => patvars p) fields
    | patvars (APPpat(_,p)) = patvars p
    | patvars (CONSTRAINTpat(p,_)) = patvars p
    | patvars (LAYEREDpat(p,q)) = 
	let val (old1,new1) = patvars p 
	    and (old2,new2) = patvars q
	 in (old1 @ old2, new1 @ new2)
	end
    | patvars _ = (nil,nil)

  fun getvars (VALdec vbl) = smash (fn VB{pat,...} => patvars pat) vbl
    | getvars (a as VALRECdec rvbl) =
	smash (fn RVB{var=VALvar{access=LVAR(var),name,...},exp,...} => 
	          (tl(getVARvars name), [var])
	        | _ => impossible "#738 in translate")
	      rvbl
    | getvars (LOCALdec (localdec,visibledec)) = getvars visibledec
    | getvars (EXCEPTIONdec ebl) =
	(nil,map (fn EBgen{exn=DATACON{rep=VARIABLE(LVAR v),...},...} => v
	           | EBdef{exn=DATACON{rep=VARIABLE(LVAR v),...},...} => v
	           | _ => impossible "in getvars EXCEPTIONdec")
	         ebl)
    | getvars (SEQdec decl) = smash getvars decl
    | getvars (DATATYPEdec _) = ([],[])
    | getvars (ABSTYPEdec{body,...}) = getvars body
    | getvars (TYPEdec _) = ([],[])
    | getvars (STRdec sbl) =
	smash (fn STRB{strvar=STRvar{name,access=LVAR(v),...},...} => 
		  (tl(getSTRvars name), [v]))
	          sbl
    | getvars (ABSdec sbl) =
	smash (fn STRB{strvar=STRvar{name,access=LVAR(v),...},...} => 
		  (tl(getSTRvars name), [v]))
	      sbl
    | getvars (FCTdec fbl) =
	smash (fn FCTB{fctvar=FCTvar{name,access=LVAR(v),...},...} => 
		  (tl(getFCTvars name), [v]))
	      fbl
    | getvars (OPENdec l) =
        (fold (fn (STRvar{binding=STRstr{table,...},...},acc) => 
		  let val ans = ref acc
		  in Table.app (table,
			    fn (id,VARbind _) => ans := getVARvars id @ !ans
			     | (id,STRbind _) => ans := getSTRvars id @ !ans
			     | _ => ());
	             !ans
		  end) l nil, nil)
    | getvars (SIGdec _) = ([],[])

    open System.Timer
    val update = System.Stats.update

    (* set up top-level runtime environment, represented as intmap *)
    exception Runbind
    val t = Intmap.new Runbind : System.object Intmap.intmap
    val bind = Intmap.add t   (* add runtime binding *)
    val unbind = Intmap.rem t (* remove runtime binding *)
    val lookup = Intmap.map t (* lookup runtime binding *)

    fun parse() =
	let val ref linenum = ErrorMsg.lineNum
	    val timer = start_timer()
	    val absyn = (anyErrors := false; Lex.toplevel := true; Parse.interdec())
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
	    val vars as (old,new) = getvars absyn
	    val lambda = Translate.transDec absyn 
			    (Lambda.RECORD (map Lambda.VAR new))
	    val time = check_timer timer
	in  update(System.Stats.translate,time);
	    timemsg ("translate, " ^ makestring time ^ "s")
		orelse debugmsg "translate";
	    if !anyErrors then raise Stop else ();
	    (vars,lambda)
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
	    val a = System.create_s size
	    val pos = ref 0
	    fun writebyte i = (System.store_s(a,!pos,i); inc pos)
	    val executable =
		  (emit writebyte;
		   if !anyErrors then raise Stop else ();
		   debugmsg "about to boot";
		   (System.boot : string -> 
				  ((int->System.object) ->
				   (System.object Array.array *
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

    val profiling = System.Control.Profile.profiling

    fun topLevel() =
        let (* initialize static environment by loading boot files *)
	    val [c,i,m] = bootEnv load

	    (* bind runtime structure Initial *)
	    val {core,math,initial} = !System.pstruct

	    fun toploop() =
		let val absyn = parse()
		    val ((old,new),lambda) = translate absyn
		    val lambda = opt (Opt.closetop lambda)
		    val executable = codegen lambda
		    val (result,profile) = exec executable
		    fun bindlvars (i,v::r) = (bind(v,result sub i);
					      bindlvars (i+1,r))
		      | bindlvars (_,nil) = ()
		in  bindlvars(0,new);   (* add new runtime bindings *)
    		    System.Control.Profile.add profile;
		    spoolLambda lambda;
		    PrintDec.printDec lookup absyn;
		    Env.commit();     (* accept static bindings *)
		    app unbind old;   (* remove old runtime bindings *)
		    toploop()
		end

	    fun use_file fname =
		let val stream = open_in fname handle e as Io_failure _ =>
				 (print("[cannot open " ^ fname ^ "]\n"); raise e)
		      val _ = print("[opening " ^ fname ^ "]\n")
		      val _ = Lex.pushSource(stream, fname)
		  in  toploop()
		      handle exn =>
			(print("[closing " ^ fname ^ "]\n");
			 close_in stream handle Io_failure _ => ();
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

	    fun cleanup() =
		(Env.restore();
		 Lex.toplevel := true;
		 Lex.flush())

	    fun interact() =
 		toploop()
 		handle Parse.Eof => ()
		     | f => if !System.interactive
 			    then case f of
				   Stop => (cleanup(); interact())
				 | Syntax => (cleanup(); interact())
				 | _ => (print("uncaught exception "
 						^ System.exn_name f ^ "\n");
 				         cleanup(); interact())
 			    else (print("uncaught exception "
 					^ System.exn_name f ^ "\n");())

         in IO.use_f := use_file;
	    IO.use_s := use_stream;
	    bind(c,core); bind(i,initial); bind(m,math);
	    Env.commit();
	    Lex.pushSource(std_in,"std_in");
	    print "Go for it\n";
	    interact()
	end

    val _ = topLevel()

end (* functor Interact *)
