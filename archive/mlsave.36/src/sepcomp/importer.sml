(* Importer: Isolation of Mads' original code from Interact() into a separate
   functor. *)

functor Importer(structure ModuleComp: MODULE_COMPILER
		    sharing ModuleComp.Lambda = Lambda
		        and ModuleComp.Absyn = BareAbsyn
		        and type ModuleComp.lvar = Access.lvar
		 val statPrinter: Env.statModule -> string
			(* Make this a fn _ => "" for no effect. *)
		): IMPORTER =
   struct
      val BIN_VERSION = 1
		(* This is stored as the first element of the tuple in the
		   binary file. Be sure to increment it whenever the structure
		   of any of the stored data objects changes. *)

      type statModule = Env.statModule
      type LambDynModule = ModuleComp.LambDynModule
      type CodeDynModule = ModuleComp.CodeDynModule
      type lvar = Access.lvar

      exception Import of string
			  	(* A single exception for any failure to
				   import (barring compiler bugs). compileModule
				   requires a protective coating so that it
				   doesn't leave the global static environment
				   in a funny state. *)

      datatype ToplevelFns =
         TOPLEVEL_FNS of {bind: lvar * System.Unsafe.object -> unit,
			  lookup: lvar -> System.Unsafe.object,
			  parse: unit -> BareAbsyn.dec,
			  getvars: BareAbsyn.dec -> lvar list,
			  opt: Lambda.lexp -> Lambda.lexp
			 }

      fun opening file    = print("[opening " ^ file ^ "]\n")
      fun openingIn file  = print("[opening " ^ file ^ " for input]\n")
      fun openingOut file = print("[opening " ^ file ^ " for output]\n")
      fun closing file    = print("[closing " ^ file ^ "]\n")

      fun fail(msg, verdict) =
         (print("import: " ^ msg ^ "\n"); raise Import verdict)

      fun addAndExecModule(statModule as Env.STATmodule{table, ...},
			   compDynModule,
			   TOPLEVEL_FNS{bind, lookup, ...}):unit =
	  let val newlvars=
		 Env.importModule statModule 
		  (*adds the static bindings of the module to the *)
		  (*static environment*)

	      val Nick_result =
		 ModuleComp.executeDynModule compDynModule lookup
		 handle exn =>	(* Local handle for module execution (NICK). *)
		    fail("execution of module raised exception "
			  ^ System.exn_name exn
			  ^ "\n\t(static bindings of module take no effect)\n",
			 "uncaught exception"
			)

	      fun bindlvars (i,v::r) = (bind(v,Nick_result sub i);
					bindlvars (i+1,r))
		| bindlvars (_,nil) = ()
	   in  
	      bindlvars(0,newlvars);		(* add new runtime bindings *)
	      Env.commit();			(* accept static bindings *)
	      PrintDec.printBindingTbl table
	   end
	   handle	(* Exceptions other than ones raised through
			   module execution. *)
	        Interrupt => raise Interrupt
	      | exn => ErrorMsg.impossible(
		          "addAndExecModule: exn (" ^ System.exn_name exn ^ ")??"
		       )


     (* New code (NICK) - I store the static information (StatModule) and
        dynamic information (CodeDynModule) in one object, so that I can blast
	out the entire thing as a single object into a file. Foo.sml now gets
	compiled into Foo.bin (sounds like a good name to me), which contains
	both. The object stored in the file is a pair: the first element is a
	"version number" for the data structures, the second is whatever needs
	storing (currently a record of {statModule, dynModule}).
	If this version number changes, I have to recompile. *)

      fun tryOpenIn filename: instream option =
         SOME(open_in filename) handle Io _ => NONE

      fun createBinary(filename,
	  	       statModule: statModule,
		       codeDynModule: CodeDynModule
		      ): unit =
         let
	    val fullName = filename ^ ".bin"
	    val outstream =
	       (open_out fullName
		handle Io _ => fail("couldn't open " ^ fullName ^ " for output",
				    "open"
				   )
	       )
	    val _ = openingOut fullName
	 in
	    System.Unsafe.blast_write(
	       outstream,
	       (BIN_VERSION, {statModule=statModule, codeDynModule=codeDynModule})
	    );
	    close_out outstream;
	    closing fullName
	 end

      fun createTextual(filename, statModule): unit =
        let
	  val outputText = statPrinter statModule
	in
	  case outputText
	    of "" => ()		(* Do NOTHING if the print function is a dummy *)
	     | _ =>
	         let
		   val fullName = filename ^ ".lstat"
		   val _ = openingOut fullName
		   val os = open_out fullName
			    handle Io _ =>
			      fail("couldn't open " ^ fullName ^ " for output",
				    "open"
				   )
		 in
		   output os outputText;
		   closing fullName;
		   close_out os
		 end
        end

     (* We must do a syntactic check that the source declarations in a module are
        just functor and signature declarations (or sequences thereof), otherwise
	the renaming routines will fall over later. Importer is the place to do
	it, where we still have a fighting chance of a putting out a decent
	diagnostic message. We don't allow IMPORT - that should have been dealt
	with earlier. *)

      local
         open BareAbsyn
      in
         fun kosherModuleDecl dec =
	    case dec
	      of FCTdec _ => true
	       | SIGdec _ => true
	       | SEQdec decs =>			(* ALL must be kosher. *)
	            fold (fn (dec, result) => kosherModuleDecl dec andalso result)
		         decs true		(* Is there a "forall" fn, Daddy? *)
	       | _ => false
      end

      fun badModuleDecl() =
         ErrorMsg.condemn "expecting SIGNATURE/FUNCTOR/IMPORT"
         

      fun getModule(filename, pervasives, toplevelFns): statModule*CodeDynModule =
             getModule'(filename, pervasives,
			{smlStream=tryOpenIn(filename ^ ".sml"),
			 binStream=tryOpenIn(filename ^ ".bin")
			},
			toplevelFns
		       )

      and getModule'(filename, pervasives, {smlStream, binStream}, toplevelFns)
             : statModule*CodeDynModule =
         case (smlStream, binStream)
	   of (SOME source, NONE) =>		(* Source only: Compile! *)
	         let
		    val fullName = filename ^ ".sml"
		    val _ = opening fullName;
		    val _ = Lex.pushSource(source, fullName)

		    val (statModule, codeDynModule) =
		       compileModule(pervasives, toplevelFns)
		       handle exn =>
		          (closing fullName; close_in source; Lex.popSource();
			   raise exn
			  )
                 in
		    closing fullName;
		    close_in source;
		    Lex.popSource();
		    createBinary(filename, statModule, codeDynModule);
		    createTextual(filename, statModule);
		    (statModule, codeDynModule)
		 end

	    | (SOME source, SOME binary) =>	(* One day: check dates! *)
	         (close_in source;
	          getModule'(filename, pervasives,
			     {smlStream=NONE, binStream=binStream},
			     toplevelFns
			    )
		 )				(* Just ignore the source. *)

	    | (NONE, SOME binary) =>
	         let val fullName = filename ^ ".bin"
		     val _ = openingIn fullName
		     val (binVersion, {statModule, codeDynModule}) =
		        System.Unsafe.blast_read binary
		 in
		    if binVersion = BIN_VERSION then
		       (closing fullName;
		        close_in binary;
			(statModule, codeDynModule)
		       )
		    else
		       (print(filename ^ ".bin is an old format; recompiling\n");
		        closing fullName;
			close_in binary;
			case tryOpenIn(filename ^ ".sml")
			  of SOME source =>
				getModule'(filename, pervasives,
					   {smlStream=SOME source, binStream=NONE},
					   toplevelFns
					  )
			   | NONE =>
				fail(fullName ^ " is out of date, and I can't\
				     \ open " ^ filename ^ ".sml",
				     "open"
				    )
		       )
		 end

	    | (NONE, NONE) => fail("cannot open " ^ filename ^ ".sml", "open")


      and compileModule(pervasives,
	  		toplevelFns as TOPLEVEL_FNS{parse, getvars, opt, ...}
		       ): statModule * CodeDynModule =
        let val frominfo = Basics.currentInfo()

            fun loop(dynModule, lvars): LambDynModule * lvar list =
	       (case (Lex.toplevel := true; parse())
		  of BareAbsyn.IMPORTdec fnames => 
                        let
		           fun loop'([], dynMod, lvars) = (dynMod, lvars)
                             | loop'(fname::rest, dynMod, lvars)=
                                  let
				     val (stat, codeDyn) =
				        getModule(fname, pervasives, toplevelFns)

                                     val newLvars = Env.importModule stat

				     val lambDyn =
				        ModuleComp.abstractDynModule(codeDyn, newLvars)

                                     val dynMod' =
                                        ModuleComp.importDynModule(lambDyn, dynMod)
                                  in
                                     loop'(rest, dynMod', lvars @ newLvars)
                                  end
                        in
			   loop(loop'(fnames,dynModule,lvars))
			end

                   | absyn => (* normal program *)
		        if kosherModuleDecl absyn then
			  let
			    val newLvars = getvars absyn
			    val newMod =
			      ModuleComp.addDeclaration(absyn, newLvars, dynModule)
			      handle ModuleComp.AddDeclaration =>
			        fail("error during translate", "translate")
			  in
			    loop(newMod, lvars @ newLvars)
			  end
                        else
		           badModuleDecl()
		) handle Parse.Eof => (dynModule, lvars)
                       | Stop      => raise Import "compile-time error"

	    val savedEnv = Env.current()
	    val _ = Env.resetEnv pervasives

            val (lambDynModule, lvars) =
	       loop(ModuleComp.emptyDynModule, [])
	       handle exn => (Env.resetEnv savedEnv; raise exn)

            val toinfo = Basics.currentInfo()

            val statModule=
	       Env.STATmodule{table=Env.popModule(pervasives,savedEnv),
		              from=frominfo, to=toinfo,
                              lvars=lvars
			     }

	    val dynModule =
	       ModuleComp.compileDynModule opt lambDynModule
	       handle ModuleComp.CompileDynModule =>
		 fail("code generation failed", "codegen")
         in
            (statModule, dynModule)
         end

      fun getAndExecModule(filename, pervasives, toplevelFns): unit = 
         let
	    val (stat, dyn) = getModule(filename, pervasives, toplevelFns)
	 in
	    addAndExecModule(stat, dyn, toplevelFns)
	 end
   end;
