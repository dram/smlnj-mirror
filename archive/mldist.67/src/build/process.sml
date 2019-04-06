(* Copyright 1989 by AT&T Bell Laboratories *)
signature PROCESSFILE =
sig
  exception Stop
  val dumpMap : unit -> unit
  val prLambda : unit -> unit
  val prFun : int -> unit
  val printslots : Basics.env -> string -> unit
  val process : Basics.env * string * (CPS.function * string -> unit) option ->
			Basics.env
  val load : Basics.env -> string -> Basics.env
  val primeEnv : Basics.env
  val getCore : unit -> int list
  val bootEnv : (Basics.env -> string -> Basics.env) -> (Basics.env * (int * int * int))
end

structure ProcessFile : PROCESSFILE =

struct
 open Access Basics PrintUtil EnvAccess NewParse
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
  val _ = System.Control.prLambda := fn () => (MCprint.printLexp (!lambda); newline())
  fun prLambda() = (MCprint.printLexp(!lambda); newline())
  fun prFun lv = (MCprint.printFun(!lambda) lv; newline())


 (* debugging aid--print the slots of a structure
    -- this belongs somewhere else *)

  fun printslot {name,access=SLOT s} =
	  (print "Slot "; print s; print " : ";
	   print(Symbol.name name);
	   print "\n")
    | printslot {name,access=INLINE s} =
	  (print "Inline "; print(Prim.inLineName s); print " : ";
	   print(Symbol.name name);
	   print "\n")
    | printslot {name,access=PATH _} =
	  (print "Path?? :";
	   print(Symbol.name name);
	   print "\n")

  val usl : {name:Symbol.symbol,access:access} list ref = ref nil

  fun buildlist (_,VARbind(VALvar{name=[n],access,...})) =
		  usl := {name=n,access=access} :: !usl
    | buildlist (_,STRbind(STRvar{name=[n],access,...})) =
		  usl := {name=n,access=access} :: !usl
    | buildlist (_,CONbind(DATACON{name,rep=(VARIABLE access),...})) =
		  usl := {name=name,access=access} :: !usl
    | buildlist _ = ()

  fun slotgt ({access=SLOT s1,name},{access=SLOT s2,name=_}) = s1 > s2
    | slotgt ({access=SLOT _,...},_) = true
    | slotgt ({access=PATH[v1],...},{access=PATH[v2],...}) = v1 > v2
    | slotgt ({access=PATH _,...},_) = true
    | slotgt ({access=INLINE i1,...},{access=INLINE i2,...}) =
        ErrorMsg.impossible "why do you sort slots" (* i1 > i2 *)
    | slotgt ({access=INLINE _,...},_) = true
    | slotgt _ = ErrorMsg.impossible "Path access in printslots"

  fun symPath s =
    let fun f nil = (nil,nil)
	  | f ("."::m) =
		  let val (s,syms) = f m
		  in  (nil,Symbol.strSymbol(implode s)::syms)
		  end
	  | f (a::m) =
		  let val (s,syms) = f m
		  in  (a::s,syms)
		  end
        val (s,syms) = f(explode s)
    in  Symbol.strSymbol(implode s)::syms
    end

  fun qid env symlist =
    let fun getStr([],str) = str
	  | getStr(id::rest,STRstr{table,env,...}) =
	      let val STRvar{access=SLOT n,binding,...} = 
		      lookSTR table id
		      handle Env.Unbound =>
		      (print ("unbound intermediate structure in path: "
				^ Symbol.name id ^ "\n"); raise Stop)
		  val str = case (binding,env)
			     of (INDstr i,REL{s,...}) => s sub i
			      | (SHRstr(i::r),REL{s,...}) =>
				   TypesUtil.getEpath(r,s sub i)
			      | (STRstr _, _) => binding
			      | _ => ErrorMsg.impossible "Process.qid.getStr"
	       in getStr(rest,str)
	      end
	val firstId::rest = symPath symlist
	val STRvar{binding,...} = lookSTR env firstId
	      handle Unbound => (print("unbound structure at head of path: "
					^ Symbol.name firstId ^ "\n"); raise Stop)
    in  getStr(rest,binding)
    end

  fun printslots env s =
      let val STRstr{table,...} = qid env s
	  val unsortedlist = (usl := nil; Env.app buildlist table; !usl)
	  val sortedlist = Sort.sort slotgt unsortedlist
      in  print "module "; print s; print "\n";
	  app printslot sortedlist
      end
      handle Bind => ErrorMsg.impossible "Weird structure in printslots"


  open ErrorMsg BareAbsyn Lambda System.Timer

  fun for l f = app f l
  val update = System.Stats.update
  val printDepth = System.Control.Print.printDepth

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
		  fn (SEQdec decs) => app f decs
		   | (absyn as SIGdec _) => 
		       (PrintAbsyn.printDec (!env') (absyn,0,!printDepth);
			newline())
		   | (absyn as OPENdec _) =>
		       (PrintAbsyn.printDec (!env') (absyn,0,!printDepth);
			newline())
		   | (STRdec sbs) =>
			for sbs
			  (fn sb as
			      STRB{strvar=STRvar{name=[n],access=PATH[v],...},...} =>
			     (print "structure "; printSym n; newline();
			      let val mkLam = fn () => transStrb (!env') sb
			      in  proc(Symbol.name n, v, mkLam)
			      end))
		   | (ABSdec sbs) =>
			for sbs
			  (fn sb as
			      STRB{strvar=STRvar{name=[n],access=PATH[v],...},...} =>
			     (print "abstraction "; printSym n; newline();
			      let val mkLam = fn () => transStrb (!env') sb
			      in  proc(Symbol.name n, v, mkLam)
			      end))
		   | (FCTdec fbs) =>
			for fbs
			  (fn fb as
			      FCTB{fctvar=FCTvar{name,access=PATH[v],...},...} =>
			     (print "functor "; printSym name; newline();
			      let val mkLam = fn () => transFctb (!env') fb
			      in  proc(Symbol.name name, v, mkLam)
			      end))
		   | (MARKdec(d,_,_)) => f d
                   | _ => ErrorMsg.complain "signature, functor, or structure expected"
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
  val varofPT = STRvar{name=[nameofPT],access=PATH[0],binding=Prim.primTypes}
  val nameofIL = Symbol.strSymbol "InLine"
  val varofIL = STRvar{name=[nameofIL],access=PATH[0],binding=Prim.inLine}

  val primeEnv = Env.bind(nameofIL,STRbind varofIL, 
			  Env.bind(nameofPT,STRbind varofPT,
				   openStructureVar Env.empty varofPT))

  fun bootEnv (loader:env -> string -> env) =
    let val env = ref primeEnv
    in
      (CoreInfo.resetCore();
       env := load (!env) "boot/assembly.sig";
       is_core := true;
       (env := loader (!env) "boot/core.sml" 
				handle e => (is_core := false; raise e));
       is_core := false;
       env := load (!env) "boot/dummy.sml";
       let val markabs = !System.Control.markabsyn
			before System.Control.markabsyn := false
	   val svCore as STRvar{access=PATH[lvCore],...} =
	         lookSTR (!env) (Symbol.strSymbol "Core")
       in
	   CoreInfo.setCore(svCore);
  
	  env := load (!env) "boot/perv.sig";
	  env := load (!env) "boot/system.sig";
	  env := loader (!env) "boot/math.sml";
	  env := loader (!env) "boot/perv.sml";
	  env := load (!env) "boot/overloads.sml";
	  System.Control.markabsyn := markabs;
	  let val STRvar{access=PATH[lvMath],...} =
		     lookSTR (!env) (Symbol.strSymbol "Math")
	      and svInitial as STRvar{access=PATH[lvInitial],
			 	      binding=strInitial as STRstr{table,...},...} =
		     lookSTR (!env) (Symbol.strSymbol "Initial")
	      and STRvar{binding=STRstr{table=otable,...},...} =
		     lookSTR (!env) (Symbol.strSymbol "Overloads")
	      val sigs = map (fn s => lookSIG (!env) (Symbol.sigSymbol s))
			      ["REF","LIST","ARRAY","BYTEARRAY","IO","BOOL",
			       "STRING","INTEGER","REAL","GENERAL"]
	      val NJsymbol = Symbol.strSymbol "NewJersey"
	  in (* merge overload bindings into Initial's symtable *)
	    (* IntStrMap.app (IntStrMap.add table) otable;
	     $$$ this has been replaced by the following atop construction,
	     which is probably less efficient *)
	     env := Env.atop(otable,openStructureVar Env.empty svInitial);
	     (* Rewrite this as a "fold", without side-effects *)
	     app (fn (sgn as SIGvar{name,...}) => 
		   env := Env.bind(name,SIGbind sgn, !env)) sigs;
	     env := Env.bind
			(NJsymbol, 
			 STRbind(STRvar{name=[NJsymbol],access=PATH[lvInitial],
				        binding=strInitial}),
			 !env);
	     (!env,(lvCore,lvInitial,lvMath))
	  end
       end) handle Cascade s => (print("Compiler Bug: "^s^"\n");
				 raise Stop)
     end

end (* structure ProcessFile *)
