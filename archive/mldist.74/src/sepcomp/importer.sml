(* Copyright 1989 by AT&T Bell Laboratories *)
functor Importer(structure FilePaths: FILEPATHS
		 val fileExtension: string
		 structure ModuleComp: MODULE_COMPILER)
		: IMPORTER =
struct
  open PrintUtil Access Modules NewParse

  fun all pred list = fold (fn (this, res) => pred this andalso res) list true

  val DEBUG = false
  val debug = if DEBUG then fn str => outputc std_out ("<" ^ str ^ ">\n")
                       else fn _ => ()

  exception Import
     (* A single exception for any failure to import (barring compiler bugs).
        compSource requires a protective coating so that it doesn't leave
	the global static environment in a funny state. *)

  (* Feedback messages. If anybody's interested, files which may
     cause failures, or may cause nested reads, are done as:
       [reading fred.sml]
       [closing fred.sml]

     Ones which shouldn't (eg. reading from a binary) produce:
       [reading fred.bin... done]
   *)

  fun reading(file, indent) =
      (tab indent; print("[reading " ^ file ^ "]\n"))
  fun reading1(file, indent) =
      (tab indent; print("[reading " ^ file ^ "... "); flush_out std_out)
  fun writing(file, indent) =
      (tab indent; print("[writing " ^ file ^ "]\n"))
  fun writing1(file, indent) =
      (tab indent; print("[writing " ^ file ^ "... "); flush_out std_out)
  fun closing(file, indent) =
      (tab indent; print("[closing " ^ file ^ "]\n"))
  fun done() = print "done]\n"

  fun fail msg = (print("import: " ^ msg ^ "\n"); raise Import)

 (* impliedPath: derived from FilePaths.impliedPath, but catches
    ImpliedPath if a "~"-filename fails to translate. *)

  fun impliedPath(oldPath, oldName) =
      FilePaths.impliedPath(oldPath, oldName)
      handle FilePaths.ImpliedPath =>
	fail("couldn't translate path in: " ^ oldName)

  type LambDynModule = ModuleComp.LambDynModule
  type CodeDynModule = ModuleComp.CodeDynModule
  type lvar = Access.lvar

  datatype statModule =
      STATmodule of {env: Modules.env, lvars: lvar list}

    (* Rename the lvars of the static module.
       Only signature and functor bindings are accepted.
       For each functor binding, a fresh lvar will be chosen; hence
       at run-time, several imports of the same functor will presumably
       lead to a new copy of the code of that functor *)

  fun importModule(STATmodule{env,lvars}) : (env * lvar list) = 
      let val newlvars = map (fn _ => mkLvar()) lvars
	  fun lookup x =
	      let fun f(a::ar, b::br) = if a=x then b else f(ar,br)
		    | f _ = ErrorMsg.impossible "importModule 1"
	       in f(lvars,newlvars)
	      end
	  val newenv = ref(Env.empty: Modules.env)
	  fun adjustBinding(_,FCTbind(FCTvar{name,access=PATH[lvar], binding})) =
	       newenv := Env.bind
	        (name,
 	         FCTbind(FCTvar{name = name, access= PATH[lookup lvar],
				binding = binding}),
		!newenv)
            | adjustBinding(n,b as SIGbind _) = newenv := Env.bind(n,b,!newenv)
	    | adjustBinding _ = ErrorMsg.impossible "importModule 2"
      in Env.app adjustBinding env;
	 (!newenv,newlvars)
      end

 (* We store the static information (StatModule) and
    dynamic information (CodeDynModule) in one object, so that I can blast
    out the entire thing as a single object into a file. Foo.sml now gets
    compiled into Foo.bin. The file contains 1 line of Ascii describing
    the ML version and the target architecture, then has a BinFormat object.
    If this version string changes, we have to recompile. *)

  type BinFormat = {statModule: statModule,
		    dynModule: CodeDynModule,
		    imports: string list}

  val blastRead: instream -> BinFormat = System.Unsafe.blast_read
  val blastWrite: (outstream * BinFormat) -> unit = System.Unsafe.blast_write

  val blastWrite =    (* Silent version. *)
      fn (stream, obj) =>
	 let val gcmessages = System.Control.Runtime.gcmessages 
	     val oldmsgs = !gcmessages
	  in gcmessages := 0;
	     (blastWrite(stream, obj); gcmessages := oldmsgs)
	     handle e => (gcmessages := oldmsgs; raise e)
	 end

  val BIN_VERSION = System.version ^ " " ^ fileExtension^ "\n"
       (* This is stored as the first line of the binary file. *)

  structure SysIO = System.Unsafe.SysIO
  fun timeFile filename =
      let val System.Timer.TIME{sec, ...} = SysIO.mtime(SysIO.PATH filename)
       in SOME sec
      end
      handle _ => NONE

 (* We must do a syntactic check that the source declarations in a module
    are just functor and signature declarations (or sequences thereof),
    otherwise the renaming routines will fall over later. Importer is the
    place to do it, where we still have a fighting chance of a putting
    out a decent diagnostic message. We don't allow IMPORT - that should
    have been dealt with earlier. *)

  val rec kosherModuleDecl =
	fn BareAbsyn.FCTdec _ => ()
	 | BareAbsyn.SIGdec _ => ()
	 | BareAbsyn.MARKdec(dec,_,_) => kosherModuleDecl dec
	 | BareAbsyn.SEQdec decs => app kosherModuleDecl decs
	 | _ => fail "expecting SIGNATURE/FUNCTOR/IMPORTS"

 (* uptodate should be memo'd sometime, since it's quite expensive. *)
  fun uptodate (path, myBinTime) name =
       let val {newPath, validName} = impliedPath(path, name)
	in case (timeFile(validName ^ ".sml"), timeFile(validName ^ ".bin"))
	    of (SOME srcTime, SOME binTime) => 
	      ( srcTime < binTime	(* binary out of date *)
		andalso binTime < myBinTime  (* Some other branch of the Make
					  task compiled this under me...? *)
		andalso let (* source is older; check imports *)
		    	   val binary = open_in (validName ^ ".bin")
		    	   val binVersion = input_line binary
		        in if binVersion <> BIN_VERSION
			   then (close_in binary; false)
			        (* can't trust "imports" : chicken out *)
			   else let val {imports, ...} = (blastRead binary)
			         in close_in binary;
			            all (uptodate(newPath,myBinTime)) imports
			        end
		       end
		handle (Io s) => (fail s handle Import => (); false))
	    | (SOME _, NONE) => false  (* No bin: force recompile *)
	    | (NONE, SOME _) => true  (* No source: trust for now... *)
	    | (NONE, NONE) => fail("cannot find source or binary of module "
				   ^ validName)
       end (* uptodate *)

 fun getModule(pervasives, parents, indent, path, name) =
     (* "parents" is a depth-first list of filenames used for
	a circularity check. "indent" is for cosmetic purposes. *)
    let val {validName as filename, newPath as path} = impliedPath(path, name)
	val _ = if exists (fn x  => x = filename) parents
		then fail("self-referential import of " ^ validName)
		else ()
	val parents = filename :: parents
	val _ = debug("getModule(name=" ^ name ^ ")")
	val srcName = filename ^ ".sml" and binName = filename ^ ".bin"

	fun compSource0(source: instream, name:string) : statModule * CodeDynModule =
	  let val inputSource = ErrorMsg.newSource(name,source,false,std_out,
						   Index.openIndexFile name)
	      val parser = NewParse.parse inputSource
	      val emptyDynModule = ModuleComp.mkLambda(Env.empty,inputSource,
					  Absyn.SEQdec nil, nil)

	      fun loop(EOF, env, dynMod, lvars, imports) = 
				(env, dynMod, lvars, imports)
		| loop(ABORT, _,_,_,_) = fail "syntax error"
		| loop(ERROR, _,_,_,_) = fail "syntax or semantic error"
		| loop(PARSE(BareAbsyn.IMPORTdec(name::rest),_),
		       env, dynMod, lvars, imports) =
		      let val (stat, codeDyn) =
			      getModule(pervasives, parents, indent+2,
					path, name)
			  val (newEnv,newLvars) = importModule stat
			  val lambDyn = ModuleComp.abstractDynModule
					  	(codeDyn, newLvars)
			  val dynMod' = ModuleComp.importDynModule
					  	(lambDyn, dynMod)
		       in loop(PARSE(BareAbsyn.IMPORTdec rest,Env.empty), 
			       Env.atop(newEnv,env), dynMod', 
			       newLvars @ lvars, name :: imports)
		      end
		| loop(PARSE(BareAbsyn.IMPORTdec nil,_), env, d, l, i) =
			loop(parser(Env.atop(env,pervasives)), env, d, l, i)
		| loop(PARSE (absyn,env'), env, dynModule, lvars, imports) = 
		      (* normal program *)
		      let val _ = kosherModuleDecl absyn
			  val newLvars = Linkage.getvars absyn
			  val env'' = Env.atop(env',env)
			  val newMod =ModuleComp.importDynModule
			      (ModuleComp.mkLambda(Env.atop(env'',pervasives),
						   inputSource,absyn,newLvars),
			       dynModule)
					handle ModuleComp.MkLambda =>
					  fail "error during translate"
		       in loop(parser(Env.atop(env'',pervasives)),
			       env'', newMod, newLvars @ lvars, imports)
		      end

              fun clumsyCleanup() = case inputSource 
		                     of {indexStream=SOME s,...} => close_out s
				      | _ => ()
	      val (env, lambDynModule, lvars, imports) =
		    loop(parser pervasives, Env.empty, emptyDynModule, [], [])
		    before clumsyCleanup()
		    handle Import  => (clumsyCleanup(); raise Import)
		       | Io x => (clumsyCleanup(); 
				  fail("unexpected: Io("^x^")"))
		       | exn => (clumsyCleanup();
				 fail("compile-time exception: "
					     ^ System.exn_name exn))

	      val statModule= STATmodule{env=Env.consolidate env,
					 lvars=lvars}
	      val dynModule = ModuleComp.compileDynModule lambDynModule
			      handle ModuleComp.CompileDynModule =>
				fail "code generation failed"

      	   in let val outstream = open_out binName handle Io s => fail s
               in writing1(binName, indent);
	          outputc outstream BIN_VERSION;
	          blastWrite(outstream,
	  	             {statModule=statModule, dynModule=dynModule,
		      	      imports=imports});
	          close_out outstream;
	          done()
              end handle Import => (); (* make failed writes nonfatal *)
	      (statModule, dynModule)
	  end  (* fun compSource0 *)

	fun compSource () = let
	      val _ = debug(filename ^ ": source only")
	      val source = open_in srcName
	      fun cleanup () = (closing(srcName, indent);
				close_in source)
	       in reading(srcName, indent);
		  (compSource0(source, srcName) before cleanup())
		  handle exp => (cleanup(); raise exp)
	      end

     in case (timeFile srcName, timeFile binName)
	 of (SOME _, NONE) =>  compSource() (* Source only: Compile! *)
	  | (SOME srcTime, SOME binTime) => (
	      debug(filename ^ ": src dated " ^ makestring srcTime
		^ ", bin dated " ^ makestring binTime);
	      if srcTime >= binTime   (* (">=" for safety) *)
		then (  (* binary out of date? *)
		  tab indent;
		  print("[" ^ filename ^ ".bin is out of date; recompiling]\n");
		  compSource())
		else let (* bin is newer: what about the things imported? *)
		  val _ = debug(filename ^ ": checking imports")
		  val _ = reading1(binName, indent)
		  val binary = open_in binName
		  val binVersion = input_line binary
		  in
		    if (binVersion <> BIN_VERSION)
		      then (
			print "]\n";
			tab indent;
			print("[" ^ binName
			  ^ " is the wrong format; recompiling\n");
			closing(binName, indent);
			close_in binary;
			compSource())
		      else let
			val {statModule, dynModule, imports} = blastRead binary
			fun allOk imports = (
			      all (uptodate (path, binTime)) imports)
				handle exn => (
				  print "]\n";
				  closing(binName, indent);
				  close_in binary;
				  raise exn)
			in
			  if not(allOk imports)
			    then (
			      print "]\n"; tab indent;
			      print("[import(s) of " ^ filename
				^ " are out of date; recompiling]\n");
			      closing(binName, indent);
			      close_in binary;
			      compSource())
			    else ( (* All OK: use the binary. *)
			      debug(filename ^ ": all up to date");
			      close_in binary;
			      done();
			      (statModule, dynModule))
			end
		  end)

	  | (NONE, SOME _) =>
	      let val _ = debug(filename ^ ": binary only")
	          val binary = (reading1(binName, indent); open_in binName)
	          val binVersion = input_line binary
	       in if (binVersion = BIN_VERSION)
		  then let val {statModule, dynModule, ...} = blastRead binary
		        in close_in binary;  done();
		           (statModule, dynModule)
		       end
		  else (print "]\n";	(* Outstanding message... *)
		        closing(binName, indent);
		        close_in binary;
		        fail(binName ^ "is the wrong format and can't open "
		             ^ srcName))
	      end

	  | (NONE, NONE) => fail("cannot open " ^ srcName)
    end (* getModule *)

  fun getAndExecModule pervasives filename : Modules.env =
    if !System.Control.interp then fail("interpreter cannot import") else
    let val (statModule as STATmodule{env, ...}, compDynModule) =
	      getModule(pervasives,[],0,FilePaths.defaultPath,filename)
	val (newenv,newlvars) = importModule statModule 
	  (* adds the static bindings of the module to the static environment*)
	val result = ModuleComp.executeDynModule Linkage.lookup compDynModule
		handle exn =>	(* Local handle for module execution (NICK). *)
		  fail("uncaught exception "^ System.exn_name exn)
     in Linkage.bindLvars(newlvars,result);	(* add new runtime bindings *)
	PrintDec.printBindingTbl env;
	newenv
    end

(* From here down is the new code to implement the compiler primitives
   for the pervasive structure Compile, which supports the SourceGroup separate
   compilation system. *)

  fun modtime filename =
    let val System.Timer.TIME{sec, ...} = SysIO.mtime(SysIO.PATH filename)
    in sec end handle _ => 0

  val compiling'env :env ref = ref (Env.empty :env)

  val targetVersion = System.version ^ " SOURCEGROUP " ^ fileExtension^ "\n"

  type targetFormat = {statModule: statModule, dynModule: CodeDynModule}

  val targetRead: instream -> targetFormat = System.Unsafe.blast_read
  val targetWrite': (outstream * targetFormat) -> unit =
        System.Unsafe.blast_write

  val targetWrite =    (* Silent version. *)
      fn (stream, obj) =>
	 let val gcmessages = System.Control.Runtime.gcmessages 
	     val oldmsgs = !gcmessages
	  in gcmessages := 0;
	     (targetWrite'(stream, obj); gcmessages := oldmsgs)
	     handle e => (gcmessages := oldmsgs; raise e)
	 end

  fun getTargetModules targetName indent =
    let val _ = reading1(targetName, indent)
        val target = open_in targetName
        val target'version = input_line target
    in
      if (target'version <> targetVersion)
        then
          (print "]\n";
           print("[" ^ targetName ^ " is the wrong format; quitting\n");
           closing(targetName, 0);
           close_in target;
           raise Import)
        else
          let val {statModule, dynModule,...} = targetRead target
          in
            close_in target;
            done();
            (statModule, dynModule)
          end
    end

  fun load'modules (statModule as STATmodule{env,...}, compDynModule) :unit =
    let val (newenv, newlvars) = importModule statModule 
	val _ = (compiling'env := Env.consolidate(Env.atop(newenv,!compiling'env)))
        val result = ModuleComp.executeDynModule Linkage.lookup compDynModule
              handle exn =>   (* Local handle for module execution (NICK). *)
                fail("uncaught exception "^ System.exn_name exn)
     in Linkage.bindLvars(newlvars,result);   (* add new runtime bindings *)
        PrintDec.printBindingTbl env;
        ()
    end

  fun load_target_file (filename:string) :unit =
    load'modules (getTargetModules filename 0)

  fun valid_target_file (filename:string) :bool =
    let val target = open_in filename
        val target'version = input_line target
        val result = (target'version = targetVersion)
    in
      close_in target;
      result
    end handle _ => false

  fun delete'file (filename :string) =
    (System.Unsafe.SysIO.unlink filename) handle _ => ()

  fun handleInterrupt (operation : unit -> unit) =
    let exception Done
        val old'handler = System.Signals.inqHandler(System.Signals.SIGINT)
        fun reset'handler () =
          System.Signals.setHandler(System.Signals.SIGINT, old'handler)
     in ( callcc (fn k =>
           ( System.Signals.setHandler(System.Signals.SIGINT,SOME(fn _ => k)) ;
             operation ();
             raise Done )) ;
          print ("\n--- interrupt ---\n") ;
          raise Interrupt )
        handle Done => (reset'handler ())
             | exn  => (reset'handler (); raise exn)
    end

  fun compile_file (loadTarget:bool) (srcName:string) (targetName :string) =
    let val _ = debug(srcName ^ ": compile_file")
        val source = open_in srcName
        val inputSource = ErrorMsg.newSource
              (srcName, source, false, std_out, Index.openIndexFile srcName)
        fun cleanup () =
          (closing(srcName, 0); close_in source;
           case inputSource of {indexStream=SOME s,...} => close_out s |_=>())
        val parser = NewParse.parse inputSource
        val emptyDynModule = ModuleComp.mkLambda
              (Env.empty, inputSource, Absyn.SEQdec nil, nil)
        fun compSource1 () : statModule * CodeDynModule =
          let fun loop(EOF, env, dynMod, lvars, imports) =
                    (env, dynMod, lvars, imports)
                | loop(ABORT, _,_,_,_) = fail "syntax error"
                | loop(ERROR, _,_,_,_) = fail "syntax or semantic error"
                | loop(PARSE(BareAbsyn.IMPORTdec _,_), env, d, l, i) =
                      loop(parser(Env.atop(env,!compiling'env)), env, d, l, i)
                | loop(PARSE (absyn,env'), env, dynModule, lvars, imports) =
                      let val _ = kosherModuleDecl absyn
                          val newLvars = Linkage.getvars absyn
			  val env'' = Env.atop(env',env)
                          val full'env = Env.atop(env'',!compiling'env)
                          val newMod =ModuleComp.importDynModule
			      (ModuleComp.mkLambda(full'env,
                                                   inputSource,absyn,newLvars),
			       dynModule)
                                        handle ModuleComp.MkLambda =>
                                          fail "error during translate"
                      in loop(parser full'env,
                              env'', newMod, newLvars@lvars, imports)
                      end
              val (env, lambDynModule, lvars, imports) =
                    loop(parser(!compiling'env),Env.empty,emptyDynModule,[],[])
                  handle Io x => (fail("unexpected: Io("^x^")"))
                       | exn => (fail("compile-time exception: "
                                             ^ System.exn_name exn))
	      val statModule= STATmodule{env=Env.consolidate env,
					 lvars=lvars}
	      val dynModule = ModuleComp.compileDynModule lambDynModule
			      handle ModuleComp.CompileDynModule =>
				fail "code generation failed"
          in
            let val outstream = open_out targetName handle Io s => fail s in
              writing1(targetName, 0);
              outputc outstream targetVersion;
              targetWrite (outstream,
                           {statModule=statModule, dynModule=dynModule});
              close_out outstream;
              done()
            end handle Import => (delete'file targetName); 
            (statModule, dynModule)
          end  (* fun compSource1 *)

      fun process'file () =
           (reading(srcName, 0);
            let val (statmod, dynmod) = compSource1 () in
              cleanup();
              if loadTarget then load'modules (statmod, dynmod) else ()
            end; ())
    in
      (handleInterrupt process'file)
        handle e => (cleanup(); delete'file targetName; raise e);
      ()
    end

  val compile_source = compile_file false
  val compile_and_load = compile_file true

  val _ =
    (System.Unsafe.compile_ref := compile_source;
     System.Unsafe.compile_and_load_ref := compile_and_load;
     System.Unsafe.load_target_ref := load_target_file;
     System.Unsafe.validTarget_ref := valid_target_file)

end (* functor Importer *)

