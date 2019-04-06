(* Copyright 1989 by AT&T Bell Laboratories *)
signature PROCESSFILE =
sig
  exception Stop
  val dumpMap : unit -> unit
  val prLambda : unit -> unit
  val prFun : int -> unit
  val printslots : string -> unit
  val timemsg : string -> bool
  val process : string * (Lambda.lexp * string -> unit) option -> unit
  val load : string -> unit
  val reset : unit -> unit
  val primeEnv : unit -> unit
  val getCore : unit -> int list
  val bootEnv : (string -> unit) -> int * int * int
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
    | printslot {name,access=LVAR s} =
	  (print "Lvar "; print s; print " : ";
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

  fun buildlist (_,_,VARbind(VALvar{name=[n],access,...})) =
		  usl := {name=n,access=access} :: !usl
    | buildlist (_,_,STRbind(STRvar{name=[n],access,...})) =
		  usl := {name=n,access=access} :: !usl
    | buildlist (_,_,CONbind(DATACON{name,rep=(VARIABLE access),...})) =
		  usl := {name=name,access=access} :: !usl
    | buildlist _ = ()

  fun slotgt ({access=SLOT s1,name},{access=SLOT s2,name=_}) = s1 > s2
    | slotgt ({access=SLOT _,...},_) = true
    | slotgt ({access=LVAR v1,...},{access=LVAR v2,...}) = v1 > v2
    | slotgt ({access=LVAR _,...},_) = true
    | slotgt ({access=INLINE i1,...},{access=INLINE i2,...}) =
        ErrorMsg.impossible "why do you sort slots" (* i1 > i2 *)
    | slotgt ({access=INLINE _,...},_) = true
    | slotgt _ = ErrorMsg.impossible "Path access in printslots"

  fun symPath s =
    let fun f nil = (nil,nil)
	  | f ("."::m) =
		  let val (s,syms) = f m
		  in  (nil,Symbol.symbol(implode s)::syms)
		  end
	  | f (a::m) =
		  let val (s,syms) = f m
		  in  (a::s,syms)
		  end
        val (s,syms) = f(explode s)
    in  Symbol.symbol(implode s)::syms
    end

  fun qid symlist =
    let fun getStr([],str) = str
	  | getStr(id::rest,STRstr{table,env,...}) =
	      let val STRvar{access=SLOT n,binding,...} = 
		      lookSTRinTable(table,id)
		      handle Env.UnboundTable =>
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
	val STRvar{binding,...} = lookSTR firstId
	      handle Unbound => (print("unbound structure at head of path: "
					^ Symbol.name firstId ^ "\n"); raise Stop)
    in  getStr(rest,binding)
    end

  fun printslots s =
      let val STRstr{table,...} = qid s
	  val unsortedlist = (usl := nil; IntStrMap.app buildlist table; !usl)
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

  fun process(fname, gencode) =
      let 
          val stream = open_in fname
	  val inputSource as {anyErrors,...} =
			 ErrorMsg.newSource(fname,stream,false,std_out)
	  val parser = NewParse.parse inputSource

	  fun opt lam =
		let val timer = start_timer()
	     	    val lam = if !CGoptions.reduce then Opt.reduce lam else lam
	     	    val lam = if !CGoptions.hoist then Opt.hoist lam else lam
	     	    val time = check_timer timer
	     	in  update(System.Stats.codeopt,time);
	     	    timemsg("codeopt, " ^ makestring time ^ "s")
	     		  orelse debugmsg "codeopt";
	     	    lam
	     	end
       
         exception Eof
       
         fun transStrb sb =
	     	let val timer = start_timer()
	     	    val (sb,profil) = Prof.instrumStrb sb
	     	    val Absyn.STRB{strvar=STRvar{access=LVAR v,...},...} = sb
	     	    val lam = Translate.transDec inputSource
					(Absyn.STRdec[sb]) (Lambda.VAR v)
	     	    val lam = Prof.bindLambda(lam,profil)
	     	    val time = check_timer timer
	     	 in update(System.Stats.translate,time);
	     	    timemsg("translate, " ^ makestring time ^ "s")
	     		  orelse debugmsg "translate";
	     	    if !anyErrors then raise Stop else ();
	     	    lam
	     	end
	       
         fun transFctb fb =
	     	let val timer = start_timer()
	     	    val (fb,profil) = Prof.instrumFctb fb
	     	    val Absyn.FCTB{fctvar=FCTvar{access=LVAR v,...},...} = fb
	     	    val lam = Translate.transDec inputSource
				(Absyn.FCTdec[fb]) (Lambda.VAR v)
	     	    val lam = Prof.bindLambda(lam,profil)
	     	    val time = check_timer timer
	     	 in update(System.Stats.translate,time);
	     	    timemsg("translate, " ^ makestring time ^ "s")
	     		  orelse debugmsg "translate";
	     	    if !anyErrors then raise Stop else ();
	     	    lam
	     	end

	  val _ = Env.commit()
	  fun cleanup() = (print("[closing " ^ fname ^ "]\n");
			   close_in stream)
	  fun proc(name,lvar,mkLam) =
	      (enterName(lvar, name);
	       case gencode of
		 NONE => ()
	       | SOME gencode =>
		 let val lam = Opt.closestr(lookupName,opt(mkLam()), getCore())
		 in debugmsg "closed";
		    if !saveLambda then lambda := lam else ();
		    gencode(lam, name);
		    if !anyErrors then raise Stop else ()
		 end)
	  fun loop() =
	    let val rec f = 
		  fn EOF => raise Eof
		   | ABORT => raise Stop
		   | ERROR => raise Stop
		   | PARSE(SEQdec decs) => app (f o PARSE) decs
		   | PARSE(absyn as SIGdec _) =>
			(PrintAbsyn.printDec(absyn,0,!printDepth);
			 newline())
		   | PARSE(absyn as OPENdec _) =>
			(PrintAbsyn.printDec(absyn,0,!printDepth);
			 newline())
		   | PARSE(STRdec sbs) =>
			for sbs
			  (fn sb as
			      STRB{strvar=STRvar{name=[n],access=LVAR v,...},...} =>
			     (print "structure "; printSym n; newline();
			      let val mkLam = fn () => transStrb sb
			      in  proc(Symbol.name n, v, mkLam)
			      end))
		   | PARSE(ABSdec sbs) =>
			for sbs
			  (fn sb as
			      STRB{strvar=STRvar{name=[n],access=LVAR v,...},...} =>
			     (print "abstraction "; printSym n; newline();
			      let val mkLam = fn () => transStrb sb
			      in  proc(Symbol.name n, v, mkLam)
			      end))
		   | PARSE(FCTdec fbs) =>
			for fbs
			  (fn fb as
			      FCTB{fctvar=FCTvar{name,access=LVAR v,...},...} =>
			     (print "functor "; printSym name; newline();
			      let val mkLam = fn () => transFctb fb
			      in  proc(Symbol.name name, v, mkLam)
			      end))
		   | PARSE(MARKdec(d,_,_)) => f(PARSE d)
                   | _ => ErrorMsg.complain "signature, functor, or structure expected";
	     in f(parser());
		loop()
	    end
      in  loop() 
	    handle Eof =>
		     (cleanup();
		      if !anyErrors
		      then (Env.restore(); raise Stop)
		      else Env.consolidate())
		 | e => (Env.restore(); cleanup(); raise e)
      end

  fun load fname = process(fname,NONE)

 (* initializing static environment *)

 (* priming structures: PrimTypes and InLine *)
  val nameofPT = Symbol.symbol "PrimTypes"
  val varofPT = STRvar{name=[nameofPT],access=LVAR 0,binding=Prim.primTypes}
  val varofPT' = STRvar{name=[nameofPT],access=PATH[0],binding=Prim.primTypes}
  val nameofIL = Symbol.symbol "InLine"
  val varofIL = STRvar{name=[nameofIL],access=LVAR 0,binding=Prim.inLine}

  fun reset() = Env.reset()

  fun primeEnv() =
      (reset();
       openStructureVar varofPT';
       bindSTR(nameofPT,varofPT);
       bindSTR(nameofIL,varofIL);
       ())

  fun bootEnv (loader:string -> unit) =
      (primeEnv();
       CoreInfo.resetCore();
       load "boot/assembly.sig";
       is_core := true;
       (loader "boot/core.sml" handle e => (is_core := false; raise e));
       is_core := false;
       load "boot/dummy.sml";
       let val markabs = !System.Control.markabsyn
			before System.Control.markabsyn := false
	   val svCore as STRvar{access=PATH[lvCore],...} =
	         lookSTR (Symbol.symbol "Core")
        in CoreInfo.setCore(svCore);
	   load "boot/perv.sig";
	   load "boot/system.sig";
	   loader "boot/math.sml";
	   loader "boot/perv.sml";
	   load "boot/overloads.sml";
	   System.Control.markabsyn := markabs;
	   let val STRvar{access=PATH[lvMath],...} =
		     lookSTR (Symbol.symbol "Math")
	       and svInitial as STRvar{access=PATH[lvInitial],
				       binding=strInitial as STRstr{table,...},...} =
		     lookSTR (Symbol.symbol "Initial")
	       and STRvar{binding=STRstr{table=otable,...},...} =
		     lookSTR (Symbol.symbol "Overloads")
	       val sigs = map (fn s => lookSIG(Symbol.symbol s))
			      ["REF","LIST","ARRAY","BYTEARRAY","BASICIO",
			       "IO","BOOL","STRING","INTEGER","REAL","GENERAL"]
	       val NJsymbol = Symbol.symbol "NewJersey"
	    in Env.reset();
	        (* merge overload bindings into Initial's symtable *)
	       IntStrMap.app (IntStrMap.add table) otable;
	       openStructureVar(svInitial);
	       app (fn (sgn as SIGvar{name,...}) => bindSIG(name,sgn))
		   sigs;
	       bindSTR(NJsymbol, STRvar{name=[NJsymbol],access=LVAR(lvInitial),
				        binding=strInitial});
	       (lvCore,lvInitial,lvMath)
	   end
       end) handle Cascade s => (print("Compiler Bug: "^s^"\n");
				 raise Stop)


end (* structure ProcessFile *)
