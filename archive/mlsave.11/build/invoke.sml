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
    val saveLambda : bool ref
    exception Stop
    val load : string -> unit
    val assemble : string -> unit
    val check : string -> unit
    val compile : string -> unit
    val reset : unit -> unit
    val bootEnv : unit -> unit
    val bootEnv1 : unit -> unit
    val compileIn : unit -> unit
    val dumpMap : unit -> unit
  end

functor Invoke(Codes : CODES) : INVOKE =
struct

  structure BareAbsyn = BareAbsyn
  structure Lambda = Lambda

  structure Machine = Codegen(Codes.Machm.M)
  structure Amachine = Codegen(Codes.Macha.M)

  local open Intmap
	val m : string intmap = new()
   in val enterName = add m
      val lookupName = map m
      val lookupName = fn v => lookupName v 
            handle Intmap => let val s = Access.lvarName v
			      in ErrorMsg.complain ("Bad free variable: "
						 ^ Access.lvarName v);
				 s
			     end
      fun dumpMap() =
	   let fun p(i:int,s:string) =
		        (print i; print " -> "; print s; print "\n"; ())
	   in  print "lvar -> structure mapping:\n"; app p m
	   end
  end

  local open ErrorMsg Access Basics BareAbsyn Lambda
  in

    fun freevars e =
        let val t = Intset.new()
	    val set = Intset.add t
	    val unset = Intset.rem t
	    val done = Intset.mem t
	    val free : int list ref = ref nil
	    fun root [v] = v | root (_::p) = root p
	    val rec mak =
	     fn VAR w => if done w then () else (set w; free := w :: !free)
	      | FN (w,b) => (set w; mak b; unset w)
	      | FIX (vl,el,b) => (app set vl; app mak (b::el); app unset vl)
	      | APP (f,a) => (mak f; mak a)
	      | SWITCH(e,l,d) => 
		  (mak e;
		   app (fn (DATAcon(DATACON{rep=ref(VARIABLE(PATH p)),...}),e) =>
			     (mak(VAR(root p)); mak e)
			 | (c,e) => mak e)
		       l;
		   case d of NONE => () | SOME a => mak a)
	      | RECORD l => app mak l
	      | SELECT (i,e) => mak e
	      | HANDLE (a,h) => (mak a; mak h)
	      | RAISE e => mak e
	      | INT _ => ()
	      | REAL _ => ()
	      | STRING _ => ()
         in set 0; mak e; !free
        end
   
    fun closestr e =
        let val fv = freevars e
	    val names = map lookupName fv
         in if !ErrorMsg.debugging
		then app (fn s => (print s; print " ")) names
		else ();
	    FN(mkLvar(),
	       RECORD
	         [fold (fn (v,f) =>
		          let val w = mkLvar()
			   in FN(w,APP(FN(v,APP(f,SELECT(1,(VAR w)))),
				       SELECT(0,(VAR w))))
			  end)
		       fv
		       (FN(mkLvar(),e)),
	          fold (fn (s,f) => RECORD[STRING s, f])
		       names
		       (RECORD nil)])
        end

    fun closetop e =
	let val looker = Access.mkLvar()
	    val fv = freevars e
	 in FN(looker,
	       fold (fn (v,f) =>
			 APP(FN(v,f),APP(VAR looker, INT v)))
		    fv e)
        end

    val lambda = ref (Lambda.RECORD [])
    val vars = ref (nil : int list)
    fun prLambda () = (MCprint.printLexp(!lambda); print "\n"; ())
    val saveLambda = ref false

    exception Stop

    fun process (fname, gencode) =
	let val (_,source) = Lex.openSource(open_in fname, fname)
	    fun cleanup() = (print("[closing "^ fname ^ "]\n");
			     Lex.closeSource source)
	    fun proc(name,lvar,absyn) =
		let val lam = Translate.topdec absyn before debugmsg "translated"
		    val _ = if !anyErrors then raise Stop else ()
		    val lam = closestr lam before debugmsg "closed"
		    val lam = Opt.reduce lam before debugmsg "reduced"
		    val lam = if !CGoptions.hoist
				 then Opt.hoist lam before debugmsg "hoisted"
				 else lam
		    val _ = if !anyErrors then raise Stop else ()
		in  enterName(lvar, name);
		    gencode(lam, name);
		    if !anyErrors then raise Stop else ();
		    Env.commit();
		    if !saveLambda then lambda := lam else ()
	       end
	    fun loop() =
		let val absyn = (anyErrors := false; Parse.tdec())
					before debugmsg "parsed"
		    val _ = if !anyErrors then raise Stop else ()
		in  case absyn of
			SIGdec _ => (PrintAbsyn.printDec absyn;PrintUtil.newline())
		      | STRdec(STRB{strvar=STRvar{name,access=LVAR v,...},...})
			      => (print "structure ";
				  PrintUtil.printSym name;
				  print "\n";
				  proc(Symbol.name name, v, absyn))
		      | FCTdec(FCTB{fctvar=FCTvar{name,access=LVAR v,...},...})
			      => (print "functor ";
				  PrintUtil.printSym name;
				  print "\n";
				  proc(Symbol.name name, v, absyn));
		    loop()
	       end
	in  loop() handle Parse.Eof => cleanup()
			| e => (Env.restore(); cleanup(); raise e)
	end

    fun load fname =
        let val (_,source) = Lex.openSource(open_in fname, fname)
	    fun cleanup() = (print("[closing "^ fname ^ "]\n");
	    		     Lex.closeSource source)
	    fun load0() =
   	     let val absyn = Parse.tdec()
	     in  case absyn of
		    SIGdec _ => (PrintAbsyn.printDec absyn; PrintUtil.newline())
		  | STRdec(STRB{strvar=STRvar{name,access=LVAR v,...},...})
			      => (print "structure ";
				  PrintUtil.printSym name;
				  print "\n";
				  enterName(v,Symbol.name name);
				  Env.commit())
	          | FCTdec(FCTB{fctvar=FCTvar{name,access=LVAR v,...},...})
			      => (print "functor ";
				  PrintUtil.printSym name;
				  print "\n";
				  enterName(v,Symbol.name name);
				  Env.commit());
		 load0()
	     end
	in  load0() handle Parse.Eof => cleanup()
			 | e => (Env.restore(); cleanup(); raise e)
	end

    fun assemble(fname : string) =
	 process(fname,
	         (fn (lexp,s) => 
		    let val outfile = open_out( s ^ ".s")
		     in Codes.Macha.outfile := outfile;
(* spacing and parens critical in this string -- don't change *)
			output(outfile,"\
\.text\n\
\base:	.byte (end-base-4)/16777216\n\
\	.byte (end-base-4)/65336- ((end-base-4)/65336/256*256)\n\
\	.byte (end-base-4)/256- ((end-base-4)/256/256*256)\n\
\	.byte (end-base-4)- ((end-base-4)/256*256)\n\
\	.long 0\n");
		        Amachine.generate lexp
		        handle Amachine.Notfound_Codenv v
			 => (MCprint.printLexp (Lambda.VAR v);
			     raise Amachine.Notfound_Codenv v);
			output(outfile,"end:\n");
			close_out outfile
		    end))

    fun check(fname : string) =
	process(fname, (fn (lexp,s) => ()))

    fun compile(fname : string) =
	process(fname,
	        (fn (lexp,s) => 
		 let val (size,f) = (Machine.generate lexp;
		                     Codes.Machm.assemble())
		     val _ = if !anyErrors then raise Stop else ()
		     val outfile = open_out( s ^ ".mo")
		     fun writebyte i = output(outfile,chr(i))
		     fun writeint i = (writebyte(i div 16777216);
		     		  writebyte((i div 65536) mod 256);
		     		  writebyte((i div 256) mod 256);
		     		  writebyte(i mod 256))
		  in writeint(size+4);
		     writeint 0; (* scratch location for gc forwarding *)
		     f writebyte;
		     close_out outfile
		 end))

val _ = debugmsg "invoke1"

    val _ = let val STRstr{table,env,...} = Prim.primTypes
	    in EnvAccess.pervasives := [(([0],env),table)]
	   end

val _ = debugmsg "invoke2"

    val nameofPT = SymbolTable.stringToSymbol "PrimTypes"
    val varofPT = STRvar{name=nameofPT,access=LVAR(0),binding=Prim.primTypes}
    val nameofILP = SymbolTable.stringToSymbol "InLinePrim"
    val varofILP = STRvar{name=nameofILP,access=LVAR(0),binding=Prim.inLinePrim}

    fun reset() =
        (Env.reset();
	 EnvAccess.reset();
	 Typecheck.reset();
	 Parse.reset())

    fun fetchStr str =
        let val STRvar{binding=STRstr{table,env,...},access=PATH p,...} =
                  EnvAccess.lookSTR(SymbolTable.stringToSymbol str)
	 in ((p,env),table)
	end

    fun bootEnv () =
	(reset();
	 Env.add(nameofPT,STRbind(varofPT));
	 Env.add(nameofILP,STRbind(varofILP));
	 compile "boot/perv.sig";
	 compile "boot/perv.sml";
	 load "boot/assembly.sml";
	 load "boot/overloads.sml";
	 EnvAccess.pervasives := map fetchStr ["Initial","Overloads"];
	 reset();
	 app Env.openOld (!EnvAccess.pervasives))

    fun bootEnv1 () =
	(reset();
	 Env.add(nameofPT,STRbind(varofPT));
	 Env.add(nameofILP,STRbind(varofILP));
	 compile "boot/perv.sig";
	 load "boot/perv.sml";
	 load "boot/assembly.sml";
	 load "boot/overloads.sml";
	 EnvAccess.pervasives := map fetchStr ["Initial","Overloads"];
	 reset();
	 app Env.openOld (!EnvAccess.pervasives))


    fun compileIn() =
        let (* initialize static environment by loading boot files *)
	    val _ = (bootEnv1(); print "Go for it\n")
	    (* set up top-level dynamic environment, represented as intmap *)
	    val t = Intmap.new() : System.object Intmap.intmap
	    val set = Intmap.add t
	    val lookup = Intmap.map t
	    (* bind runtime structure Initial *)
	    val (([lvarInitial],_),_)::_ = !EnvAccess.pervasives
	    val _ = set(lvarInitial, !System.pstruct);
	    val _ = Env.commit()
	    fun toploop() =
		let val _ = anyErrors := false
		    val _ = Lex.toplevel := true
		    val absyn = Parse.interdec()
		    val _ = debugmsg "parsed"
		    val _ = if !debugging
				then (PrintAbsyn.printDec absyn; print "\n"; ())
				else ()
		    val _ = if !anyErrors then raise Stop else ()
		    val vars = Translate.getvars absyn
		    val lambda = Translate.makedec absyn
		     	       (Lambda.RECORD(map Lambda.VAR vars))
		    val _ = debugmsg "translated"
		    val _ = if !anyErrors then raise Stop else ()
		    val lambda = Opt.reduce (closetop lambda)
		    val _ = if !anyErrors then raise Stop else ()
		    val (size,emit) = (Machine.generate lambda;
				       Codes.Machm.assemble())
		    val _ = if !anyErrors then raise Stop else ()
		    val a = System.create_s(size+4)
		    val pos = ref 0
		    fun writebyte i = (System.store_s(a,!pos,i); inc pos)
		    (* scratch word for garbage collector *)
		    val _ = (writebyte 0; writebyte 0; writebyte 0; writebyte 0)
		    val result = (emit writebyte; debugmsg "about to boot";
		     	          System.boot1 a lookup)
		    fun set1 (i,v::r) = (set(v,result sub i); set1 (i+1,r))
		      | set1 (_,nil) = ()
		 in set1(0,vars);
		    PrintDec.printDec lookup absyn;
		    Env.commit();  (* accept bindings *)
		    toploop()
		end
	    fun usefile fname =
		  let val _ = print("[opening "^fname^"]\n")
		      val (prev,new) = Lex.openSource(open_in fname, fname)
		   in toploop()
		      handle exn =>
			(print("[closing "^fname^"]\n");
			 Lex.closeSource new;
			 Lex.resumeSource prev;
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
			     | f => (print("uncaught exception " 
					   ^ System.exn_name f ^ "\n");
				     cleanup();
				     interact())
         in System.useref := usefile;
	    Lex.openSource(std_in,"std_in"); (* redundant *)
	    interact()
	end

  end (* local open ... *)

end (* structure invoke *)
