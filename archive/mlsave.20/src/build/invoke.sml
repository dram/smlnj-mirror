(* invoke.sml *)

signature CODES =
  sig
    structure Machm : MACHINECODE
    structure Macha : ASSEMBLYCODE
  end

signature INVOKE =
  sig
    structure BareAbsyn : BAREABSYN
    structure Lambda : LAMBDA
    val lambda : Lambda.lexp ref
    val vars : int list ref
    val prLambda : unit -> unit
    val prFun : int -> unit
    val saveLambda : bool ref
    exception Stop
    val load : string -> unit
    val assemble : string -> unit
    val check : string -> unit
    val compile : string -> unit
    val reset : unit -> unit
    val bootEnv : bool -> int
    val topLevel : unit -> unit
    val dumpMap : unit -> unit
  end

functor Invoke(Codes : CODES) : INVOKE =
struct

  structure BareAbsyn = BareAbsyn
  structure Lambda = Lambda

  structure Machine = Codegen(Codes.Machm.M)
  structure Amachine = Codegen(Codes.Macha.M)

(*
  structure Reopener = Reopen(struct structure C=Machine and Machm=Codes.Machm end)
*)

  (* lvar -> string environment used by batch compiler to map module
     lvars to names of modules *)
  local 
      open Intmap
      val m : string intmap = new()
      val lookup = map m
   in val enterName = add m
      fun lookupName v =
	  lookup v 
	  handle Intmap => 
	    let val s = Access.lvarName v
	     in ErrorMsg.complain ("Compiler bug: bad free variable "
				   ^ Access.lvarName v);
		s
	    end
      fun dumpMap() =
	  let fun p(i:int,s:string) =
		  (print i; print " -> "; print s; print "\n"; ())
	   in print "lvar -> structure mapping:\n"; app p m
	  end
  end

  open ErrorMsg Access Basics BareAbsyn Lambda PrintUtil
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
	in  if printit then (print s; newline()) else ();
	    printit
	end

  local 
    open System.Timer
    fun for l f = app f l
    val update = System.Stats.update
    fun opt lam =
	let val timer = start_timer()
	    val lam = if !CGoptions.reduce then Opt.reduce lam else lam
	    val _ = if !anyErrors then raise Stop else ()
	    val lam = if !CGoptions.hoist then Opt.hoist lam else lam
	    val time = check_timer timer
	in  update(System.Stats.codeopt,time);
	    timemsg ("codeopt, " ^ makestring time ^ "s")
		orelse debugmsg "codeopt";
	    if !anyErrors then raise Stop else ();
	    lam
	end
    fun codegen(gencode,lam,name) =
	let val timer = start_timer()
	    val _ = gencode(lam, name)
	    val time = check_timer timer
	in  update(System.Stats.codegen,time);
	    timemsg ("codegen, " ^ makestring time ^ "s")
		orelse debugmsg "codegen";
	    if !anyErrors then raise Stop else ()
	end
    fun parse() =
	let val ref linenum = ErrorMsg.lineNum
	    val timer = start_timer()
	    val absyn = (anyErrors := false; Parse.tdec())
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
	    absyn
	end
    fun transStrb sb =
	let val timer = start_timer()
	    val lam = Translate.transStrb sb
	    val time = check_timer timer
	in  update(System.Stats.translate,time);
	    timemsg ("translate, " ^ makestring time ^ "s")
		orelse debugmsg "translate";
	    if !anyErrors then raise Stop else ();
	    lam
	end
    fun transFctb fb =
	let val timer = start_timer()
	    val lam = Translate.transFctb fb
	    val time = check_timer timer
	in  update(System.Stats.translate,time);
	    timemsg ("translate, " ^ makestring time ^ "s")
		orelse debugmsg "translate";
	    if !anyErrors then raise Stop else ();
	    lam
	end
  in
    fun process (fname, gencode) =
	let val stream = open_in fname
	    val _ = Lex.pushSource(stream, fname)
	    fun cleanup() = (print("[closing " ^ fname ^ "]\n");
			     close_in stream;
			     Lex.popSource())
	    fun proc(name,lvar,lam) =
		let val lam = Opt.closestr(lookupName,lam) before debugmsg "closed"
		    val lam = opt lam
		in  if !saveLambda then lambda := lam else ();
		    enterName(lvar, name);
		    codegen(gencode,lam,name);
		    Env.commit()
		end
	    fun loop() =
		let val absyn = parse()
		in  case absyn
		     of SIGdec _ =>
			 (PrintAbsyn.printDec(absyn,0,!printDepth);
			  newline())
		      | STRdec sbs =>
			  for sbs
			  (fn sb as
			      STRB{strvar=STRvar{name,access=LVAR v,...},...} =>
			     (print "structure "; printSym name; newline();
			      let val lam = transStrb sb
			      in  proc(Symbol.name name, v, lam)
			      end))
		      | ABSdec sbs =>
			  for sbs
			  (fn sb as
			      STRB{strvar=STRvar{name,access=LVAR v,...},...} =>
			     (print "abstraction "; printSym name; newline();
			      let val lam = transStrb sb
			      in  proc(Symbol.name name, v, lam)
			      end))
		      | FCTdec fbs =>
			  for fbs
			  (fn fb as
			      FCTB{fctvar=FCTvar{name,access=LVAR v,...},...} =>
			     (print "functor "; printSym name; newline();
			      let val lam = transFctb fb
			      in  proc(Symbol.name name, v, lam)
			      end));
		    loop()
	       end
	in  loop() 
	    handle Parse.Eof =>
		     (cleanup();
		      if !anyErrors
		      then (Env.restore(); raise Stop)
		      else ())
		 | Intmap.Intmap =>
			(Env.restore(); cleanup();
			 print "Compiler bug: intmap\n"; raise Intmap.Intmap)
		 | e => (Env.restore(); cleanup(); raise e)
	end

    fun load fname =
        let val _ = print ("[Loading " ^ fname ^ "]\n")
	    val stream = open_in fname
	    val _ = Lex.pushSource(stream, fname)
	    val _ = Env.commit()
	    fun cleanup() = (print("[closing "^ fname ^ "]\n");
			     close_in stream;
	    		     Lex.popSource())
	    fun load0() =
   	     let val absyn = parse()
	     in  case absyn
		  of SIGdec _ =>
			 (PrintAbsyn.printDec(absyn,0,!printDepth);
			  newline())
		   | STRdec sbs =>
		      for sbs
		      (fn STRB{strvar=STRvar{name,access=LVAR v,...},...} =>
			 (print "structure "; printSym name; newline();
			  enterName(v,Symbol.name name);
			  Env.commit()))
		   | ABSdec sbs =>
		      for sbs
		      (fn STRB{strvar=STRvar{name,access=LVAR v,...},...} =>
			 (print "abstraction "; printSym name; newline();
			  enterName(v,Symbol.name name);
			  Env.commit()))
	           | FCTdec fbs =>
		      for fbs
		      (fn FCTB{fctvar=FCTvar{name,access=LVAR v,...},...} =>
			 (print "functor "; printSym name; newline();
			  enterName(v,Symbol.name name);
			  Env.commit()));
		 load0()
	     end
	in  load0() handle Parse.Eof =>
			     (cleanup();
			      if !anyErrors
			      then (Env.restore(); raise Stop)
			      else ())
			 | e => (Env.restore(); cleanup(); raise e)
	end

    fun assemble(fname : string) =
	(Env.commit();
	 print ("[Assembling " ^ fname ^ "]\n");
	 process(fname,
	         (fn (lexp,s) => 
		    let val outfile = open_out(s ^ ".s")
		    in  Codes.Macha.outfile := outfile;
			output outfile "base:\n";
		        Amachine.generate lexp
		        handle e
			 => (close_out outfile; raise e);
			close_out outfile
		    end)))

    fun check(fname : string) =
	(Env.commit();
	 process(fname, (fn (lexp,s) => ())))

    fun compile(fname : string) =
	(Env.commit();
	 print ("[Compiling " ^ fname ^ "]\n");
	 process(fname,
	         (fn (lexp,s) => 
		   let val (size,f) = (Machine.generate lexp;
		                       Codes.Machm.assemble())
		       val _ = if !anyErrors then raise Stop else ()
		       val outfile = open_out(s ^ ".mo")
		       val write = output outfile
		       fun writebyte i = write(chr i)
		   in  f writebyte;
		       close_out outfile
		   end)))
  end (* local ... *)

    fun smash nil f = (nil,nil)
      | smash (a::r) f = let val (x,y) = f a
			     val (x',y') = smash r f
			  in (x@x', y@y')
			 end

    val getVARvars = EnvAccess.getVARvars
    val getSTRvars = EnvAccess.getSTRvars
    val getFCTvars = EnvAccess.getFCTvars

    fun patvars (VARpat(VALvar{access=LVAR v,name,...})) = (tl(getVARvars name), [v])
      | patvars (VARpat(VALvar{access=INLINE _,name,...})) = (tl(getVARvars name), nil)
      | patvars (VARpat _ ) = impossible "non-LVAR in translate.patvars"
      | patvars (RECORDpat{fields,...}) = smash fields (fn (_,p) => patvars p)
      | patvars (APPpat(_,p)) = patvars p
      | patvars (CONSTRAINTpat(p,_)) = patvars p
      | patvars (LAYEREDpat(p,q)) = 
	  let val (old1,new1) = patvars p 
	      and (old2,new2) = patvars q
	   in (old1 @ old2, new1 @ new2)
	  end
      | patvars _ = (nil,nil)

    fun getvars (VALdec vbl) =
	  smash vbl (fn VB{pat,...} => patvars pat)
      | getvars (a as VALRECdec rvbl) =
	  smash rvbl
	    (fn RVB{var=VALvar{access=LVAR(var),name,...},exp,...} => 
		(tl(getVARvars name), [var])
	      | _ => impossible "#738 in translate")
      | getvars (LOCALdec (localdec,visibledec)) = getvars visibledec
      | getvars (EXCEPTIONdec ebl) =
	  smash ebl
	    (fn EBgen{exn=DATACON{rep=VARIABLE(LVAR v),...},...} => ([],[v])
	      | EBdef{exn=DATACON{rep=VARIABLE(LVAR v),...},...} => ([],[v])
	      | _ => impossible "in getvars EXCEPTIONdec")
      | getvars (SEQdec decl) = smash decl getvars
      | getvars (DATATYPEdec _) = ([],[])
      | getvars (ABSTYPEdec{body,...}) = getvars body
      | getvars (TYPEdec _) = ([],[])
      | getvars (STRdec sbl) =
	  smash sbl
	    (fn STRB{strvar=STRvar{name,access=LVAR(v),...},...} => 
	         (tl(getSTRvars name), [v]))
      | getvars (ABSdec sbl) =
	  smash sbl
	    (fn STRB{strvar=STRvar{name,access=LVAR(v),...},...} => 
	         (tl(getSTRvars name), [v]))
      | getvars (FCTdec fbl) =
	  smash fbl
	    (fn FCTB{fctvar=FCTvar{name,access=LVAR(v),...},...} => 
	         (tl(getFCTvars name), [v]))
      | getvars (OPENdec l) =
	  smash l 
	    (fn STRvar{binding=STRstr{table,...},...} => 
		smash (Table.list table)
		  (fn (id,VARbind _) => (getVARvars id, nil)
		    | (id,STRbind _) => (getSTRvars id, nil)
		    | _ => (nil, nil)))
      | getvars (SIGdec _) = ([],[])

    val nameofPT = Symbols.stringToSymbol "PrimTypes"
    val varofPT = STRvar{name=nameofPT,access=LVAR(0),binding=Prim.primTypes}
    val varofPT' = STRvar{name=nameofPT,access=PATH[0],binding=Prim.primTypes}
    val nameofIL = Symbols.stringToSymbol "InLine"
    val varofIL = STRvar{name=nameofIL,access=LVAR(0),binding=Prim.inLine}

    fun reset() =
        (Env.reset();
	 EnvAccess.reset();
	 Typecheck.reset();
	 Parse.reset())

    val pervNames = map Symbols.stringToSymbol ["Initial","Overloads"]
    val pervSigNames =
	  map Symbols.stringToSymbol
	      ["PERVASIVES","ARRAY","BASICIO","IO","BOOL","BYTEARRAY",
	       "INTEGER","LIST","REAL","REF","STRING","GENERAL"]

    fun bootEnv (code:bool) =
	(reset();
    	 EnvAccess.setPervasives [varofPT'];
	 EnvAccess.openPervasives();
	 Env.add(nameofPT,STRbind(varofPT));
	 Env.add(nameofIL,STRbind(varofIL));
	 load "boot/perv.sig";
	 load "boot/assembly.sig";
	 load "boot/system.sig";
	 (if code then compile else load) "boot/math.sml";
	 (if code then compile else load) "boot/perv.sml";
	 load "boot/assembly.sml";
	 load "boot/overloads.sml";
	 let val pervSigs = map EnvAccess.lookSIG pervSigNames
	     val pervStrs as (STRvar{access=PATH[lvarOfInitial],...} :: _) =
		   map EnvAccess.lookSTR pervNames
	  in EnvAccess.setPervasives(pervStrs);
	     reset();
	     EnvAccess.openPervasives();
	     List2.app2 EnvAccess.bindSIG (pervSigNames,pervSigs);
	     lvarOfInitial
	 end)

  local 
    open System.Timer
    val update = System.Stats.update
    (* set up top-level runtime environment, represented as intmap *)
    val t = Intmap.new() : System.object Intmap.intmap
    val bind = Intmap.add t   (* add runtime binding *)
    val unbind = Intmap.rem t (* remove runtime binding *)
    val lookup = Intmap.map t (* lookup runtime binding *)
    fun parse() =
	let val ref linenum = ErrorMsg.lineNum
	    val timer = start_timer()
	    val absyn = (anyErrors := false; Lex.toplevel := true;
			 Parse.interdec())
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
	          (Env.restore();
		   SEQdec(map Misc.dumpStructure strvars))
	       | _ => absyn
	end
    fun translate absyn =
	let val timer = start_timer()
	    val vars as (old,new) = getvars absyn
	    val lambda = Translate.makedec absyn
				(Lambda.RECORD(map Lambda.VAR new))
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
	    val (size,emit) = (Machine.generate lambda;
			       Codes.Machm.assemble())
	    val a = System.create_s size
	    val pos = ref 0
	    fun writebyte i = (System.store_s(a,!pos,i); inc pos)
	    val executable = (emit writebyte;
			      if !anyErrors then raise Stop else ();
	   		      debugmsg "about to boot";
	   		      (System.boot : string -> ((int->System.object) -> System.object Array.array)) a)
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
  in
    fun topLevel() =
        let (* initialize static environment by loading boot files *)
	    val lvarOfInitial = bootEnv false
	    (* bind runtime structure Initial *)
	    val _ = bind(lvarOfInitial, !System.pstruct);
	    val _ = Env.commit()
	    fun toploop() =
		let val absyn = parse()
		    val ((old,new),lambda) = translate absyn
		    val lambda = opt (Opt.closetop lambda)
		    val executable = codegen lambda
		    val result = exec executable
		    fun bindlvars (i,v::r) =
		          (bind(v,result sub i); bindlvars (i+1,r))
		      | bindlvars (_,nil) = ()
		in  bindlvars(0,new);   (* add new runtime bindings *)
		    spoolLambda lambda;
		    PrintDec.printDec lookup absyn;
		    Env.commit();     (* accept static bindings *)
		    app unbind old;   (* remove old runtime bindings *)
		    toploop()
		end
	    fun use_file fname =
		  let val stream = open_in fname handle e as Io_failure _ =>
					(print("[cannot open " ^ fname ^ "]\n");
					 raise e)
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
		let fun clean0() =
			(Lex.flush();
			 Lex.advance()
				 handle Syntax => clean0()
				      | Interrupt =>
					(print "uncaught exception Interrupt\n";
					 clean0()))
		in  Env.restore();
		    Lex.toplevel := true;
		    clean0()
		end
	    fun interact() =
			toploop()
			handle Parse.Eof => ()
			     | Stop => (cleanup(); interact())
			     | Syntax => (cleanup(); interact())
			     | Intmap.Intmap =>
				(print "Compiler bug: Intmap\n";
				 cleanup(); interact())
			     | f => (print("uncaught exception " 
					   ^ System.exn_name f ^ "\n");
				     cleanup();
				     interact())
         in IO.use_f := use_file;
	    IO.use_s := use_stream;
	    Lex.pushSource(std_in,"std_in");
	    print "Go for it\n";
	    interact()
	end

  end (* local open ... *)

end (* structure invoke *)
