functor LinkCM (structure HostMachDepVC : MACHDEP_VC) = struct

  local
      structure YaccTool = YaccTool
      structure LexTool = LexTool
      structure BurgTool = BurgTool

      structure E = GenericVC.Environment
      structure SE = GenericVC.StaticEnv
      structure ER = GenericVC.EnvRef
      structure BE = GenericVC.BareEnvironment
      structure CMSE = GenericVC.CMStaticEnv
      structure S = GenericVC.Symbol
      structure CoerceEnv = GenericVC.CoerceEnv
      structure EM = GenericVC.ErrorMsg

      val os = SMLofNJ.SysInfo.getOSKind ()

      structure SSV =
	  SpecificSymValFn (structure MachDepVC = HostMachDepVC
			    val os = os)

      val warmup_hook = ref (NONE: E.dynenv option)

      (* Instantiate the persistent state functor; this includes
       * the binfile cache and the dynamic value cache *)
      structure FullPersstate =
	  FullPersstateFn (structure MachDepVC = HostMachDepVC
			   val warmup_hook = warmup_hook)

      (* Create two arguments appropriate for being passed to
       * CompileGenericFn. One instantiation of that functor
       * is responsible for "recompile" traversals, the other one
       * does "link" traversals. Notice how the two share the same
       * underlying state. *)
      structure Recomp = RecompFn (structure PS = FullPersstate)
      structure Exec = ExecFn (structure PS = FullPersstate)

      (* make the two traversals *)
      structure RT = CompileGenericFn (structure CT = Recomp)
      structure ET = CompileGenericFn (structure CT = Exec)

      (* The StabilizeFn functor needs a way of converting bnodes to
       * dependency-analysis environments.  This can be achieved quite
       * conveniently by a "recompile" traversal for bnodes. *)
      fun bn2statenv gp i = #1 (#stat (valOf (RT.bnode gp i)))
	  handle Option => raise Fail "bn2statenv"

      (* exec_group is basically the same as ET.group with
       * two additional actions to be taken:
       *   1. Before executing the code, we announce the priviliges
       *      that are being invoked.  (For the time being, we assume
       *      that everybody has every conceivable privilege, but at the
       *      very least we announce which ones are being made use of.)
       *   2. After we are done we must make the values of "shared"
       *      compilation units permanent. *)
      fun exec_group gp (g as GroupGraph.GROUP { required = rq, ... }) =
	  (if StringSet.isEmpty rq then ()
	   else Say.say ("$Execute: required privileges are:\n" ::
		     map (fn s => ("  " ^ s ^ "\n")) (StringSet.listItems rq));
	   ET.group gp g
	   before FullPersstate.rememberShared ())

      fun recomp_runner gp g = isSome (RT.group gp g)

      (* This function combines the actions of "recompile" and "exec".
       * When successful, it combines the results (thus forming a full
       * environment) and adds it to the toplevel environment. *)
      fun make_runner gp g =
	  case RT.group gp g of
	      NONE => false
	    | SOME { stat, sym} =>
		  (case exec_group gp g of
		       NONE => false
		     | SOME dyn => let
			   val delta = E.mkenv { static = stat, symbolic = sym,
						 dynamic = dyn }
			   val base = #get ER.topLevel ()
			   val new = BE.concatEnv (CoerceEnv.e2b delta, base)
		       in
			   #set ER.topLevel new;
			   Say.vsay ["[New bindings added.]\n"];
			   true
		       end)

      fun al_loadit gp m =
	  case RT.impexpmap gp m of
	      NONE => NONE
	    | SOME { stat, sym } => let
		  fun exec () =
		      ET.impexpmap gp m
		      before FullPersstate.rememberShared ()
	      in
		  case exec () of
		      NONE => NONE
		    | SOME dyn => let
			  val e = E.mkenv { static = stat, symbolic = sym,
					    dynamic =dyn }
			  val be = GenericVC.CoerceEnv.e2b e
		      in
			  SOME be
		      end
	      end

      val al_greg = GroupReg.new ()

      (* Instantiate the stabilization mechanism. *)
      structure Stabilize =
	  StabilizeFn (val bn2statenv = bn2statenv
		       val getPid = FullPersstate.pid_fetch_sml
		       val warmup = FullPersstate.new_bininfo
		       val recomp = recomp_runner)

      (* Access to the stabilization mechanism is integrated into the
       * parser. I'm not sure if this is the cleanest way, but it works
       * well enough. *)
      structure Parse = ParseFn (structure Stabilize = Stabilize)

      local
	  type kernelValues =
	      { primconf : Primitive.configuration,
	        pervasive : E.environment,
		corenv : BE.staticEnv,
		pervcorepids : PidSet.set }

	  val fnpolicy = FilenamePolicy.colocate
	      { os = os, arch = HostMachDepVC.architecture }

	  val pcmodeRef = ref (PathConfig.hardwire [])

	  val theValues = ref (NONE: kernelValues option)

      in
	  fun param () = let
	      val v = valOf (!theValues)
		  handle Option =>
		      raise Fail "CMBoot: theParam not initialized"
	  in
	      { primconf = #primconf v,
	        fnpolicy = fnpolicy,
		pcmode = !pcmodeRef,
		symenv = SSV.env,
		keep_going = EnvConfig.getSet StdConfig.keep_going NONE,
		pervasive = #pervasive v,
		corenv = #corenv v,
		pervcorepids = #pervcorepids v }
	  end

	  fun autoload s = let
	      val c = SrcPath.cwdContext ()
	      val pcmode = !pcmodeRef
	      val p = SrcPath.standard pcmode { context = c, spec = s }
	  in
	      case Parse.parse (SOME al_greg) (param ()) NONE p of
		  NONE => false
		| SOME (g, _) =>
		      (AutoLoad.register (GenericVC.EnvRef.topLevel, g);
		       true)
	  end

	  fun al_ginfo () = { param = param (),
			      groupreg = al_greg,
			      errcons = EM.defaultConsumer () }

	  val al_manager =
	      AutoLoad.mkManager (fn m => al_loadit (al_ginfo ()) m)

	  fun al_manager' (ast, _, ter) = al_manager (ast, ter)

	  fun initTheValues bootdir = let
	      val pcmode = PathConfig.bootcfg bootdir
	      val _ = pcmodeRef := pcmode
	      val initgspec =
		  SrcPath.standard pcmode { context = SrcPath.cwdContext (),
					    spec = BtNames.initgspec }
	      val ginfo = { param = { primconf = Primitive.primEnvConf,
				      fnpolicy = fnpolicy,
				      pcmode = pcmode,
				      symenv = SSV.env,
				      keep_going = false,
				      pervasive = E.emptyEnv,
				      corenv = BE.staticPart BE.emptyEnv,
				      pervcorepids = PidSet.empty },
			    groupreg = GroupReg.new (),
			    errcons = EM.defaultConsumer () }
	  in
	      case BuildInitDG.build ginfo initgspec of
		  NONE => raise Fail "CMBoot: BuiltInitDG.build"
		| SOME { rts, core, pervasive, primitives, ... } => let
		      fun get n = let
			  val { stat = (s, sp), sym = (sy, syp), ctxt } =
			      valOf (RT.sbnode ginfo n)
			  val d = Exec.env2result (valOf (ET.sbnode ginfo n))
			  val env = E.mkenv { static = s, symbolic = sy,
					      dynamic = d }
			  val pidInfo = { statpid = sp, sympid = syp,
					  ctxt = ctxt }
		      in
			  (env, pidInfo)
		      end
		      fun getPspec (name, n) = let
			  val (env, pidInfo) = get n
		      in
			  { name = name, env = env, pidInfo = pidInfo }
		      end

		      val (core, corePidInfo) = get core
		      val corenv = CoerceEnv.es2bs (E.staticPart core)
		      val (rts, _) = get rts
		      val (pervasive0, pervPidInfo) = get pervasive
		      val pspecs = map getPspec primitives
		      val core_symdyn =
			  E.mkenv { static = E.staticPart E.emptyEnv,
				    dynamic = E.dynamicPart core,
				    symbolic = E.symbolicPart core }
		      val pervasive = E.layerEnv (pervasive0, core_symdyn)
		      val pervcorepids =
			  PidSet.addList (PidSet.empty,
					  [#statpid corePidInfo,
					   #statpid pervPidInfo,
					   #sympid pervPidInfo])
		  in
		      #set ER.core corenv;
		      #set ER.pervasive pervasive;
		      #set ER.topLevel BE.emptyEnv;
		      theValues :=
		        SOME { primconf = Primitive.configuration pspecs,
			       pervasive = pervasive,
			       corenv = corenv,
			       pervcorepids = pervcorepids };
		      HostMachDepVC.Interact.installCompManager
		           (SOME al_manager');
		      autoload "basis.cm";
		      ()
		  end
	  end
      end

      fun stabilize_runner gp g = true
  in
    structure CM = struct

	fun run sflag f s = let
	    val c = SrcPath.cwdContext ()
	    val p = SrcPath.native { context = c, spec = s }
	in
	    case Parse.parse NONE (param ()) sflag p of
		NONE => false
	      | SOME (g, gp) => f gp g
	end

	fun stabilize recursively = run (SOME recursively) stabilize_runner
	val recomp = run NONE recomp_runner
	val make = run NONE make_runner
	val autoload = autoload
    end

    structure CMB = struct
	structure BootstrapCompile =
	    BootstrapCompileFn (structure MachDepVC = HostMachDepVC
				val os = os)
	val make' = BootstrapCompile.compile
	fun make () = make' NONE
	fun setRetargetPervStatEnv x = ()
	fun wipeOut () = ()
    end

    fun init (bootdir, de) =
	(warmup_hook := SOME de;
	 initTheValues bootdir;
	 warmup_hook := NONE)
  end
end

signature CMTOOLS = sig end
signature COMPILATION_MANAGER = sig end
