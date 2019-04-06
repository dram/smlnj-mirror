(* Copyright 1989 by AT&T Bell Laboratories *)
signature PROCESSFILE =
sig
  exception Stop
  val dumpMap : unit -> unit
  val prLambda : unit -> unit
  val prFun : int -> unit
  val process : Modules.env * string * (CPS.function * string -> unit) option ->
			Modules.env
  val load : Modules.env -> string -> Modules.env
  val primeEnv : Modules.env
  val getCore : unit -> int list
  val bootEnv : (Modules.env -> string -> Modules.env) -> (Modules.env * (int * int * int))
end

structure ProcessFile : PROCESSFILE =

struct
 open Access Modules Types Variables PrintUtil ModuleUtil NewParse
      PrintBasics

 structure CGoptions = System.Control.CG

     fun debugmsg  (msg : string) =
	let val printit = !System.Control.debugging
	in  if printit then (print msg; print "\n")
	    else ();
	    printit
	end

  exception Stop

  fun timemsg (s : string) =
    if !System.Control.timings then (print s; newline(); true) else false

  val saveLambda = System.Control.saveLambda
  val lambda = ref (Lambda.RECORD [])
  (* really needed only for interactive version *)
  val _ = System.Control.prLambda := (fn () => (MCprint.printLexp (!lambda); newline()))
  fun prLambda() = (MCprint.printLexp(!lambda); newline())
  fun prFun lv = (MCprint.printFun(!lambda) lv; newline())

  open ErrorMsg BareAbsyn Lambda System.Timer

  fun for l f = app f l
  val update = System.Stats.update

  (* lvar -> string environment used by batch compiler to map module
     lvars to names of modules *)
  exception Modname
  val m : string Intmap.intmap = Intmap.new(32, Modname)
  val lookup = Intmap.map m
  val enterName = Intmap.add m
  fun lookupName v =
      lookup v 
      handle Modname => 
	 ErrorMsg.impossible ("Bad free variable: " ^ Access.lvarName v)
  fun dumpMap() =
      let fun p(i:int,s:string) = (print i; print " -> "; print s; print "\n")
      in  print "lvar -> structure mapping:\n"; Intmap.app p m
      end

  val is_core = ref false;

  fun getCore () = if !is_core then [] else tl(!CoreInfo.stringequalPath)

  fun process(env, fname, gencode) =
      let 
          val stream = open_in fname
	  val inputSource as {anyErrors,...} =
			 ErrorMsg.newSource(fname,stream,false,std_out,
					    Index.openIndexFile fname)
	  val parser = NewParse.parse inputSource

	 fun convert lam =
		let val timer = start_timer()
	     	    val lam = Reorder.reorder lam
		    val function = Convert.convert lam
	     	    val time = check_timer timer
	     	in  update(System.Stats.convert,time);
	     	    timemsg("convert, " ^ makestring time ^ "s")
	     		  orelse debugmsg "convert";
	     	    function
	     	end
       
         exception Eof
       
         fun transStrb env sb =
	     	let val timer = start_timer()
	     	    val (sb,profil) = Prof.instrumStrb sb
	     	    val Absyn.STRB{strvar=STRvar{access=PATH[v],...},...} = sb
	     	    val lam = Translate.transDec env inputSource
					(Absyn.STRdec[sb]) (Lambda.VAR v)
	     	    val lam = Prof.bindLambda(lam,profil)
	     	    val time = check_timer timer
	     	 in update(System.Stats.translate,time);
	     	    timemsg("translate, " ^ makestring time ^ "s")
	     		  orelse debugmsg "translate";
	     	    if !anyErrors then raise Stop else ();
	     	    lam
	     	end
	       
         fun transFctb env fb =
	     	let val timer = start_timer()
	     	    val (fb,profil) = Prof.instrumFctb fb
	     	    val Absyn.FCTB{fctvar=FCTvar{access=PATH[v],...},...} = fb
	     	    val lam = Translate.transDec env inputSource
				(Absyn.FCTdec[fb]) (Lambda.VAR v)
	     	    val lam = Prof.bindLambda(lam,profil)
	     	    val time = check_timer timer
	     	 in update(System.Stats.translate,time);
	     	    timemsg("translate, " ^ makestring time ^ "s")
	     		  orelse debugmsg "translate";
	     	    if !anyErrors then raise Stop else ();
	     	    lam
	     	end

	  fun proc(name,lvar,mkLam) =
	      (enterName(lvar, name);
	       case gencode of
		 NONE => ()
	       | SOME gencode =>
		 let val lam = Opt.closestr(lookupName, mkLam(), getCore())
		 in debugmsg "closed";
		    if !saveLambda then lambda := lam else ();
		    gencode(convert lam, name);
		    if !anyErrors then raise Stop else ()
		 end)

	  val env' = ref env
	  fun loop () =
	    let val rec f =
	      fn absyn =>
	        let val pr = fn () =>
		                PrintDec.printDec (!env') absyn (fn _ => impossible "Process.f")
		in case absyn
		   of (SEQdec decs) => app f decs
		    | (MARKdec(d,_,_)) => f d
		    | (SIGdec sl) => pr ()
		    | (OPENdec _) => pr ()
		    | (STRdec sbs) =>
		       (pr ();
			for sbs
			  (fn sb as
			      STRB{strvar as STRvar{name=n,access=PATH[v],...},...} =>
			      let val mkLam = fn () => transStrb (!env') sb
			      in  proc(Symbol.name n, v, mkLam)
			      end))
		   | (ABSdec sbs) =>
			(pr ();
			 for sbs
			  (fn sb as
			      STRB{strvar as STRvar{name=n,access=PATH[v],...},...} =>
			      let val mkLam = fn () => transStrb (!env') sb
			      in  proc(Symbol.name n, v, mkLam)
			      end))
		   | (FCTdec fbs) =>
			 (pr ();
			  for fbs
			  (fn fb as
			      FCTB{fctvar as FCTvar{name,access=PATH[v],...},...} =>
			      let val mkLam = fn () => transFctb (!env') fb handle Match => impossible "transFctb: match exception"
			      in  proc(Symbol.name name, v, mkLam)
			      end))
                   | _ => ErrorMsg.complain "signature, functor, or structure expected"
              end
	    in (case (parser (!env')) of
		  EOF => raise Eof
	        | ABORT => raise Stop
	        | ERROR => raise Stop
	        | PARSE(absyn,envr) =>
		    (env' := Env.atop(envr,!env');
		     f absyn));
	       loop()
	    end 
      in  loop ()
	    handle Eof =>
		(closeSource inputSource;
		 if !anyErrors
		      then raise Stop
		      else (!env'))
		 | e => (closeSource inputSource; raise e)
      end

  fun load env fname = process(env,fname,NONE)

 (* initializing static environment *)

 (* priming structures: PrimTypes and InLine *)
  val nameofPT = Symbol.strSymbol "PrimTypes"
  val varofPT = STRvar{name=nameofPT,access=PATH[0],binding=Prim.primTypes}
  val nameofIL = Symbol.strSymbol "InLine"
  val varofIL = STRvar{name=nameofIL,access=PATH[0],binding=Prim.inLine}

  val primeEnv = Env.bind(nameofIL,STRbind varofIL, 
			  Env.bind(nameofPT,STRbind varofPT,
				   openStructureVar (Env.empty,varofPT)))

  fun bootEnv (loader:env -> string -> env) =
    let val err = fn _ => impossible "bootEnv"
	val sigSymbols =
           map Symbol.sigSymbol ["REF","LIST","ARRAY","BYTEARRAY","IO","BOOL",
 				 "ENVIRON", "COMPILE", 
		                 "STRING","INTEGER","REAL","GENERAL"]
        val NJsymbol = Symbol.strSymbol "NewJersey"
        val signatures = !System.Control.Print.signatures
	val _ = System.Control.Print.signatures := 0
        val _ = CoreInfo.resetCore();
        val env = load primeEnv "boot/assembly.sig"
	val env = (is_core := true;  loader env "boot/core.sml" 
				handle e => (is_core := false; raise e))
        val _ = is_core := false;
        val env = load env "boot/dummy.sml";
        val markabs = !System.Control.markabsyn
		       before System.Control.markabsyn := false
	val svCore as STRvar{access=PATH[lvCore],...} =
	         lookSTR (env,[Symbol.strSymbol "Core"],err)
	val _ = CoreInfo.setCore(env,[Symbol.strSymbol "Core"]);
  	val env = load env "boot/perv.sig";
	val env = load env "boot/system.sig";
	val env = loader env "boot/math.sml";
	val env = loader env "boot/perv.sml";
	val env = load env "boot/overloads.sml";
	val _ = System.Control.Print.signatures := signatures
	val _ = System.Control.markabsyn := markabs;
	val STRvar{access=PATH[lvMath],...} =
		     lookSTR (env,[Symbol.strSymbol "Math"],err)
	and svInitial as STRvar{access=PATH[lvInitial],
			         binding=strInitial,...} =
		     lookSTR (env,[Symbol.strSymbol "Initial"],err)
        and overLoads = lookSTR (env,[Symbol.strSymbol "Overloads"],err)
	val env' = openStructureVar (openStructureVar(Env.empty,svInitial),
	                             overLoads)
	val env' = fold (fn (name,e) => Env.bind(name,Env.look(env,name),e))
                 sigSymbols env'
	val env' = Env.bind
			(NJsymbol, 
			 STRbind(STRvar{name=NJsymbol,access=PATH[lvInitial],
				        binding=strInitial}),
			 env')
     in  (env',(lvCore,lvInitial,lvMath))
     end handle Cascade s => (print("Compiler Bug: "^s^"\n");
				 raise Stop)
end (* structure ProcessFile *)
