signature PREBATCH =
sig
  exception Stop
  val dumpMap : unit -> unit
  val prLambda : unit -> unit
  val prFun : int -> unit
  val reset : unit -> unit
  val reprime : unit -> unit
  val printslots : string -> unit
  val timemsg : string -> bool
  val process : string * (Lambda.lexp * string -> unit) option -> unit
  val load : string -> unit
end


structure PreBatch : PREBATCH =
struct

open Access Basics PrintUtil

exception Stop
fun timemsg (s : string) =
  if !System.Control.timings then (print s; newline(); true) else false
val saveLambda = System.Control.saveLambda
val lambda = ref (Lambda.RECORD [])
(* really needed only for interactive version *)
val _ = System.prLambda := fn () => (MCprint.printLexp (!lambda); newline())
fun prLambda() = (MCprint.printLexp(!lambda); newline())
fun prFun lv = (MCprint.printFun(!lambda) lv; newline())

local
    val nameofPT = Symbols.stringToSymbol "PrimTypes"
    val varofPT = STRvar{name=nameofPT,access=LVAR 0,binding=Prim.primTypes}
    val varofPT' = STRvar{name=nameofPT,access=PATH[0],binding=Prim.primTypes}
    val nameofIL = Symbols.stringToSymbol "InLine"
    val varofIL = STRvar{name=nameofIL,access=LVAR 0,binding=Prim.inLine}
in  fun reset() =
      (Env.reset();
       EnvAccess.reset();
       Typecheck.reset();
       Parse.reset())
    fun reprime() =
      (reset();
       EnvAccess.setPervasives [varofPT'];
       EnvAccess.openPervasives();
       Env.add(nameofPT,STRbind varofPT);
       Env.add(nameofIL,STRbind varofIL))
end

(* debugging aid--print the slots of a structure *)
local
  fun printslot {name,access=SLOT s} =
	  (print "Slot "; print s; print " : ";
	   print(Symbol.name name);
	   print "\n"; ())
    | printslot {name,access=LVAR s} =
	  (print "Lvar "; print s; print " : ";
	   print(Symbol.name name);
	   print "\n"; ())
    | printslot {name,access=INLINE s} =
	  (print "Inline "; print s; print " : ";
	   print(Symbol.name name);
	   print "\n"; ())
    | printslot {name,access=PATH _} =
	  (print "Path?? :";
	   print(Symbol.name name);
	   print "\n"; ())
  val usl : {name:Symbol.symbol,access:access} list ref = ref nil
  fun buildlist (_,VARbind(VALvar{name,access,...})) =
		  usl := {name=name,access=access} :: !usl
    | buildlist (_,STRbind(STRvar{name,access,...})) =
		  usl := {name=name,access=access} :: !usl
    | buildlist (_,CONbind(DATACON{name,rep=(VARIABLE access),...})) =
		  usl := {name=name,access=access} :: !usl
    | buildlist _ = ()
  fun slotgt ({access=SLOT s1,name},{access=SLOT s2,name=_}) = s1 > s2
    | slotgt ({access=SLOT _,...},_) = true
    | slotgt ({access=LVAR v1,...},{access=LVAR v2,...}) = v1 > v2
    | slotgt ({access=LVAR _,...},_) = true
    | slotgt ({access=INLINE i1,...},{access=INLINE i2,...}) = i1 > i2
    | slotgt ({access=INLINE _,...},_) = true
    | slotgt _ = ErrorMsg.impossible "Path access in printslots"
  fun symPath s =
    let fun f nil = (nil,nil)
	  | f ("."::m) =
		  let val (s,syms) = f m
		  in  (nil,Symbols.stringToSymbol(implode s)::syms)
		  end
	  | f (a::m) =
		  let val (s,syms) = f m
		  in  (a::s,syms)
		  end
        val (s,syms) = f(explode s)
    in  Symbols.stringToSymbol(implode s)::syms
    end
  fun qid symlist =
    let fun getStr([],str) = str
	  | getStr(id::rest,STRstr{table,env={s,...},...}) =
	      let val STRvar{access=SLOT n,binding,...} = 
		      EnvAccess.lookSTRinTable(table,id)
		      handle Table.Notfound_Table =>
		      (print ("unbound intermediate structure in path: "
				^ Symbol.name id ^ "\n"); raise Stop)
	      in getStr(rest,case binding of INDstr i => s sub i | _ => binding)		      end
	val firstId::rest = symPath symlist
	val STRvar{binding,...} = EnvAccess.lookSTR firstId
	      handle Unbound => (print("unbound structure at head of path: "
					^ Symbol.name firstId ^ "\n"); raise Stop)
    in  getStr(rest,binding)
    end
in
fun printslots s =
    let val STRstr{table,...} = qid s
	val unsortedlist = (usl := nil; Table.app(table,buildlist); !usl)
	val sortedlist = Sort.sort slotgt unsortedlist
    in  print "module "; print s; print "\n";
	app printslot sortedlist
    end
    handle Bind => ErrorMsg.impossible "Weird structure in printslots"
end (* local *)


(* lvar -> string environment used by batch compiler to map module
   lvars to names of modules *)
local
    exception Modname
    val m : string Intmap.intmap = Intmap.new Modname
    val lookup = Intmap.map m
in  val enterName = Intmap.add m
    fun lookupName v =
	  lookup v 
	  handle Modname => 
	    let val s = Access.lvarName v
	    in  ErrorMsg.complain ("Bad free variable: " ^ Access.lvarName v);
		s
	    end
    fun dumpMap() =
      let fun p(i:int,s:string) =
		  (print i; print " -> "; print s; print "\n"; ())
      in  print "lvar -> structure mapping:\n"; Intmap.app p m
      end
end

open ErrorMsg BareAbsyn Lambda System.Timer
local
    fun for l f = app f l
    val update = System.Stats.update
    val printDepth = System.Control.Print.printDepth
    fun opt lam =
      let val timer = start_timer()
	  val lam = if !CGoptions.reduce then Opt.reduce lam else lam
	  val _ = if !anyErrors then raise Stop else ()
	  val lam = if !CGoptions.hoist then Opt.hoist lam else lam
	  val time = check_timer timer
      in  update(System.Stats.codeopt,time);
	  timemsg("codeopt, " ^ makestring time ^ "s")
		orelse debugmsg "codeopt";
	  if !anyErrors then raise Stop else ();
	  lam
      end
    fun codegen(gencode,lam,name) =
      let val _ = gencode(lam, name)
      in  if !anyErrors then raise Stop else ()
      end
    fun parse() =
      let val ref linenum = ErrorMsg.lineNum
	  val timer = start_timer()
	  val absyn = (anyErrors := false; Parse.tdec())
	  val time = check_timer timer
	  val lines = !ErrorMsg.lineNum - linenum
      in  update(System.Stats.parse,time);
	  System.Stats.lines := !System.Stats.lines + lines;
	  timemsg("parse, " ^ Integer.makestring lines
			^ " lines, " ^ makestring time ^ "s")
		orelse debugmsg "parse";
	  if !anyErrors then raise Stop else ();
	  absyn
      end
    fun transStrb sb =
      let val timer = start_timer()
	  val lam = Translate.transStrb sb
	  val time = check_timer timer
      in  update(System.Stats.translate,time);
	  timemsg("translate, " ^ makestring time ^ "s")
		orelse debugmsg "translate";
	  if !anyErrors then raise Stop else ();
	  lam
      end
    fun transFctb fb =
      let val timer = start_timer()
	  val lam = Translate.transFctb fb
	  val time = check_timer timer
      in  update(System.Stats.translate,time);
	  timemsg("translate, " ^ makestring time ^ "s")
		orelse debugmsg "translate";
	  if !anyErrors then raise Stop else ();
	  lam
      end
in  fun process(fname, gencode) =
      let val stream = open_in fname
	  val _ = Lex.pushSource(stream, fname)
	  val _ = Env.commit()
	  fun cleanup() = (print("[closing " ^ fname ^ "]\n");
			   close_in stream;
			   Lex.popSource())
	  fun proc(name,lvar,mkLam) =
	      (enterName(lvar, name);
	       case gencode
		 of NONE => Env.commit()
		  | SOME gencode =>
		      let val lam = Opt.closestr(lookupName,opt(mkLam()))
					before debugmsg "closed"
		      in  if !saveLambda then lambda := lam else ();
			  codegen(Env.commit o gencode,lam,name)
		      end)
	  fun loop() =
	    let val absyn = parse()
	    in  case absyn
		  of SIGdec _ =>
			(PrintAbsyn.printDec(absyn,0,!printDepth);
			 newline())
		   | OPENdec _ =>
			(PrintAbsyn.printDec(absyn,0,!printDepth);
			 newline())
		   | STRdec sbs =>
			for sbs
			  (fn sb as
			      STRB{strvar=STRvar{name,access=LVAR v,...},...} =>
			     (print "structure "; printSym name; newline();
			      let val mkLam = fn () => transStrb sb
			      in  proc(Symbol.name name, v, mkLam)
			      end))
		   | ABSdec sbs =>
			for sbs
			  (fn sb as
			      STRB{strvar=STRvar{name,access=LVAR v,...},...} =>
			     (print "abstraction "; printSym name; newline();
			      let val mkLam = fn () => transStrb sb
			      in  proc(Symbol.name name, v, mkLam)
			      end))
		   | FCTdec fbs =>
			for fbs
			  (fn fb as
			      FCTB{fctvar=FCTvar{name,access=LVAR v,...},...} =>
			     (print "functor "; printSym name; newline();
			      let val mkLam = fn () => transFctb fb
			      in  proc(Symbol.name name, v, mkLam)
			      end));
		loop()
	    end
      in  loop() 
	    handle Parse.Eof =>
		     (cleanup();
		      if !anyErrors
		      then (Env.restore(); raise Stop)
		      else ())
		 | e => (Env.restore(); cleanup(); raise e)
      end
    fun load fname = (Env.commit();process(fname,NONE))

end (* local *)
end (* structure PreBatch *)
