(*
 * sched/recompile.sml: selective recompilation
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
local
    val filterPhase = Stats.newPhase "Enviroment filtering"
    val concatPhase = Stats.newPhase "Environment concatenation"
    fun inFilterPhase f = Stats.inPhase filterPhase f
    fun inConcatPhase f = Stats.inPhase concatPhase f

    datatype 'a fulfillment =
	NOTYET of unit -> 'a
      | DONE of 'a

    fun memoize thunk = let
	val promise = ref (NOTYET thunk)
	fun deliver () =
	    case !promise of
		DONE x => x
	      | NOTYET thunk => let
		    val result = thunk ()
		in
		    promise := DONE result;
		    result
		end
    in
	deliver
    end
    val delay = memoize
    fun force x = x ()
in
  functor RecompileFun (structure DagTraversal: DAG_TRAVERSAL
			structure Driver: DRIVER
			structure SysEnv:SYS_ENV
			structure Control: CONTROL
			structure Prune: PRUNE
			sharing
			    DagTraversal.SysDag.ModuleName.Compiler =
			    SysEnv.Compiler =
			    Driver.Compiler
			and
			    Prune.SD =
			    DagTraversal.SysDag =
			    Driver.SysDag): RECOMPILE = struct

    structure SysDag = DagTraversal.SysDag
    structure ModuleName = SysDag.ModuleName
    structure GroupDag = SysDag.GroupDag
    structure SmlSource = GroupDag.SmlSource
    structure Compiler = ModuleName.Compiler
    structure CUnit = SmlSource.CUnit
    structure Iid = CUnit.Iid
    structure Env = Compiler.Environment
    structure Pid = Compiler.PersStamps

    exception RecompileInternalError
    and WrongConfiguration of string * string
    and CompilationErrors of exn list

    type iid = Iid.t
    type senv = Env.staticEnv

    type iinfo = SmlSource.cu Futures.future
    datatype 'env enviid =
	ENVIID of (unit -> 'env) * iid
      | ENVIID_FAIL of exn
    datatype 'env cinfo =
	CI of { iinfo: iinfo,
	        env_iid: 'env enviid Futures.future }

    fun recomp
	{ getenv, filterBy, senvOf, runEnvOf, symenvOf,
	  genEnvD, emptyEnv, layerEnv, final }
	(SysDag.AE { roots, namemap, ... }, baseEnv) = let

	val layerEnv = inConcatPhase layerEnv

	fun same_dagno (x: int, y: int) = x = y
	val contexts: (int, unit -> senv) Table.table =
	    Table.create same_dagno

	fun d2sml (GroupDag.DAG { smlsource, ... }) = smlsource

	fun fetch cuf () =
	    case Futures.get cuf of
		SmlSource.SUCCESS cu =>
		    ENVIID (fn () => getenv cu, CUnit.iid cu)
	      | SmlSource.FAILURE exn => ENVIID_FAIL exn

	fun seen_before d = SmlSource.cunit_again (d2sml d)

	fun pre _ = ()

	fun extern mn = ModuleName.symbolOf mn

	fun cross (SysDag.CE { dag, filter = NONE, ... }, iinfo) =
	    CI { iinfo = iinfo,
		 env_iid = Futures.future (fetch iinfo) }
	  | cross (SysDag.CE { dag, filter = SOME filter, lib, pidpair },
		   iinfo) = let
		val fsyms =
		    map ModuleName.symbolOf (ModuleName.makelist filter)
		fun filterenv () =
		    case fetch iinfo () of
			ENVIID (e, iid) => let
			    val f_e =
				delay (fn () =>
				       inFilterPhase (filterBy fsyms) (e ()))
			    fun recalc () = let
				val GroupDag.DAG { seq_no = dagno, ... } = dag
			    in
				case Table.find (contexts, dagno) of
				    NONE => raise RecompileInternalError
				  | SOME mkctxt => let
					val context = mkctxt ()
					val f_iid =
					    Iid.aug (iid, context,
						     senvOf (f_e ()))
					val orig = Iid.staticPid iid
					val filtered = Iid.staticPid f_iid
				    in
					pidpair :=
					SOME { orig = orig,
					       filtered = filtered };
					ENVIID (f_e, f_iid)
				    end
			    end
			in
			    case !pidpair of
				NONE => recalc ()
			      | SOME { orig, filtered } => let
				    val pid = Iid.staticPid iid
				    val lpid = Iid.lambdaPid iid
				in
				    if Pid.compare (orig, pid) = EQUAL then
					ENVIID (f_e,
						Iid.new { senv = filtered,
							  lambda = lpid })
				    else
					recalc ()
				end
			end
		      | fail => fail
	    in
		CI { iinfo = iinfo,
		     env_iid = Futures.future filterenv }
	    end

	fun intern (_, ii) = ii

	fun subcombine (e, c, i) = let

	    fun c_combine (CI { env_iid, ... }, (e, pl, exl)) =
		case Futures.get env_iid of
		    ENVIID (e', p') =>
			(delay (fn () => layerEnv (e' (), e ())),
			 p' :: pl, exl)
		  | ENVIID_FAIL exn => (e, [], exn :: exl)

	    val (c_env, c_iids, c_exns) =
		foldr c_combine (fn () => baseEnv, [], []) c

	    fun i_combine (iinfo, (e, pl, exl)) =
		case fetch iinfo () of
		    ENVIID (e', p') =>
			(delay (fn () => layerEnv (e' (), e ())),
			 p' :: pl, exl)
		  | ENVIID_FAIL exn => (e, [], exn :: exl)

	    val (ic_env, ic_iids, ic_exns) =
		foldr i_combine (c_env, c_iids, c_exns) i

	    val _ =
		case ic_exns of
		    [] => ()
		  | _ => raise CompilationErrors ic_exns

	    val iids = Iid.makeset ic_iids
	    
	    (* val iids = Iid.makeset (basePid :: ic_iids) *)

	in
	    (ic_env, iids)
	end

	fun combine (_, e, c, i, dag) = let
	    val sml = d2sml dag
	    val GroupDag.DAG { seq_no = dagno, ... } = dag
	    fun compenv'n'iids () = subcombine (e, c, i)
	    val getContext = senvOf o force o #1 o compenv'n'iids
	    val _ = Table.enter (contexts, dagno, getContext)
	    val cuf = SmlSource.cunit { smlsource = sml,
				        mkcompenv = compenv'n'iids,
					senvOf = senvOf,
					runEnvOf = runEnvOf,
					symenvOf = symenvOf }
	in
	    if Control.keep_going NONE then
		cuf
	    else
		case Futures.get cuf of
		    SmlSource.SUCCESS _ => cuf
		  | SmlSource.FAILURE exn => raise exn
	end

	val tr = DagTraversal.traversal
	    { seen_before = seen_before, pre = pre, extern = extern,
	      cross = cross, intern = intern, combine = combine }

	fun ce2dag (SysDag.CE { dag, ... }) = dag

	fun check ([], []) = ()
	  | check ([], exns) = raise CompilationErrors exns
	  | check (iinfo :: t, exns) =
	    case Futures.get iinfo of
		SmlSource.SUCCESS _ => check (t, exns)
	      | SmlSource.FAILURE exn => check (t, exn :: exns)
    in
	check (tr roots, []);
	final (getenv, filterBy, emptyEnv, layerEnv, namemap)
    end

    fun make_topenv (getenv, filterBy, emptyEnv, layerEnv, namemap) = let	    

	fun mk_top_env ((nl, SysDag.CE { dag = GroupDag.DAG { smlsource,
							      ... },
					 filter,
					 ... }), e0) = let
	    val cu = Futures.get (SmlSource.cunit_again smlsource)
	    val env =
		case cu of
		    SmlSource.SUCCESS u => getenv u
		  | SmlSource.FAILURE exn => raise RecompileInternalError
	    val env = case filter of
		NONE => env
	      | SOME f => let
		    val f = map ModuleName.symbolOf (ModuleName.makelist f)
		in
		    inFilterPhase (filterBy f) env
		end
	in
	    layerEnv (env, e0)
	end

    in
	foldl mk_top_env emptyEnv namemap
    end

    fun cvtFilter f = fn s => fn e => f (e, s)

    fun statsymLayer ((st, sy), (st', sy')) = let
	val dyn0 = Env.dynamicPart Env.emptyEnv
	val e = Env.mkenv { static = st, dynamic = dyn0, symbolic = sy }
	val e' = Env.mkenv { static = st', dynamic = dyn0, symbolic = sy' }
	val ce = Env.concatEnv (e, e')
    in
	(Env.staticPart ce, Env.symbolicPart ce)
    end

    val recomp_only = recomp {
			      getenv = fn u => (CUnit.senv u, CUnit.symenv u),
			      filterBy =
			          fn s => fn (st, sy) =>
			              (Env.filterStaticEnv (st, s), sy),
			      senvOf = fn (st, _) => st,
			      symenvOf = fn (_, sy) => sy,
			      runEnvOf = NONE,
			      genEnvD = (fn (cu, _) =>
					 (CUnit.senv cu, CUnit.symenv cu)),
			      emptyEnv =
			        (Env.staticPart Env.emptyEnv,
				 Env.symbolicPart Env.emptyEnv),
			      layerEnv = statsymLayer,
			      final = make_topenv
			     }

    fun getStatSymBaseEnv () = let
	val e = SysEnv.getBaseEnv ()
    in
	(Env.staticPart e, Env.symbolicPart e)
    end

    fun only descfile = let
	val e as (senv, _) = getStatSymBaseEnv ()
    in
	Driver.driver (fn ae => (recomp_only (ae, e); ()), senv) descfile
    end

    fun and'stabilize recursive descfile = let
	val e as (senv, _) = getStatSymBaseEnv ()
    in
	Driver.driver (fn ae as SysDag.AE { stabilizer, ... } =>
		       (recomp_only (ae, e);
			stabilizer only recursive),
		       senv)
	  descfile
    end

    fun exec_genEnvD (cu, compenv) =
	CUnit.execute (cu, Env.dynamicPart compenv)

    fun getEnv cu =
	case CUnit.env cu of
	    NONE => raise RecompileInternalError
	  | SOME e => e

    val recomp_and_run = recomp {
				 getenv = getEnv,
				 filterBy = cvtFilter Env.filterEnv,
				 senvOf = Env.staticPart,
				 symenvOf = Env.symbolicPart,
				 runEnvOf = SOME (fn x => x),
				 genEnvD = exec_genEnvD,
				 emptyEnv = Env.emptyEnv,
				 layerEnv = Env.concatEnv,
				 final = make_topenv
				}

    fun and'run'regardless (pruning, cpusym) descfile = let
	val env = SysEnv.getBaseEnv ()
	val senv = Env.staticPart env
	val delta =
	    Driver.driver
	      (fn ae => let
		  val ae = case pruning of
		      NONE => ae
		    | SOME files => let
			  val smn = ModuleName.structMN
			  val fmn = ModuleName.functMN
			  val syms = [smn (cpusym ^ "VisComp"), fmn "IntShare"]
			  val syms = ModuleName.makeset syms
			  val ae = case SysDag.select_roots (ae, syms) of
			      { ae = SOME ae, ... } => ae
			    | _ => raise RecompileInternalError
			  val files = map AbsPath.rigidcur files
			  fun apriori (GroupDag.DAG { smlsource, ... }) = let
			      val f = SmlSource.name smlsource
			      fun sameFile (f, f') =
				  AbsPath.compare (f, f') = EQUAL
			  in
			      List.exists (fn f' => sameFile (f, f')) files
			  end
		      in
			  Prune.prune apriori ae
		      end

	      in
		  recomp_and_run (ae, env)
	      end,
	       senv)
	      descfile
    in
	Control.vsay
	  "[introducing new bindings into toplevel environment...]\n";
	SysEnv.addToInteractiveEnv delta
    end

    fun and'run (hostconf, targetconf, pruning) = let
	fun don't _ =
	    raise WrongConfiguration (Arch.confname hostconf,
				      Arch.confname targetconf)
    in
	if hostconf = targetconf then
	    and'run'regardless (pruning, Arch.cpusym (#cpu hostconf))
	else
	    don't
    end
	    
    fun exec'once_genEnvD (cu, compenv) =
	case CUnit.env cu of
	    SOME e => e
	  | NONE => 
		CUnit.execute (cu, Env.dynamicPart compenv)

    val and'run'once = 
	recomp {
		getenv = getEnv,
		filterBy = cvtFilter Env.filterEnv,
		senvOf = Env.staticPart,
		symenvOf = Env.symbolicPart,
		runEnvOf = SOME (fn x => x),
		genEnvD = exec'once_genEnvD,
		emptyEnv = Env.emptyEnv,
		layerEnv = Env.concatEnv,
		final = make_topenv
	       }

    fun withAe f = let
	val se = Env.staticPart (SysEnv.getBaseEnv ())
    in
	fn d => Driver.driver (fn ae => f (d, ae), se) d
    end
  end
end
