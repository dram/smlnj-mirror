(*
 * srctypes/smlsource.sml:
 *   Data structure which associates SML source files with everything
 *   you ever wanted to know about them.
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
local
    val declParsePhase = Stats.newPhase "Parsing (DA)"
    val compParsePhase = Stats.newPhase "Parsing (comp)"
    fun inDeclParsePhase f = Stats.inPhase declParsePhase f
    fun inCompParsePhase f = Stats.inPhase compParsePhase f
in
  functor SmlSourceFun (structure Decl: DECL
			structure FnameRules: FNAME_RULES
			structure CUnit: CUNIT
			structure Control: CONTROL
			sharing
			    Decl.Compiler =
			    CUnit.Compiler): SML_SOURCE =
  struct

    structure MD = Decl.MD
    structure Compiler = CUnit.Compiler
    structure CUnit = CUnit
    structure Iid = CUnit.Iid
    structure Env = Compiler.Environment

    exception SourceFileDoesNotExist of string
    and UserCodeExn of string * exn
    and SmlSourceInternalError

    type dec = Compiler.Ast.dec
    type source = Compiler.Source.inputSource
    type senv = Env.staticEnv
    type symenv = Env.symenv
    type env = Env.environment

    datatype cu =
	SUCCESS of CUnit.t
      | FAILURE of exn

    datatype t = S of {
		       name: AbsPath.t,
		       creator: string option,
		       timestamp: TStamp.t ref,
		       declfile: AbsPath.t,
		       binfile: AbsPath.t,
		       decl: MD.decl option ref,
		       (*
			* meaning of `cu' field ({} indicates a future):
			* NONE: no compilation was started
			* SOME {...}: compilation was started
			* SOME {FAILURE exn}: compilation failed with exn
			* SOME {SUCCESS cu}: compilation succeeded
			*)
		       cu: cu Futures.future option ref,
		       stable: bool,
		       parsed: { ast: dec,  source: source } option ref
		      }

    fun eq (S { timestamp = t1, ... }, S { timestamp = t2, ... }) = t1 = t2

    fun sameFile (f, f') = AbsPath.compare (f, f') = EQUAL

    (* a table of sources and the number of parsetrees kept within *)
    val cache: { table: (AbsPath.t, t) Table.table, trees: int ref } =
	{ table = Table.create sameFile, trees = ref 0 }

    fun describe (pn, NONE) = AbsPath.elab pn
      | describe (pn, SOME c) = concat [AbsPath.elab pn, " (", c, ")"]

    fun makestring (S { name, creator, ... }) = describe (name, creator)

    val nonexistent = SourceFileDoesNotExist o makestring

    fun get { file, creator } = let
	fun update (s as S { timestamp, decl, cu, ... }, modtime_ts) =
	    (timestamp := modtime_ts; decl := NONE; cu := NONE)
    in
	case Table.find (#table cache, file) of
	    NONE => let
		val s = S {
			   name = file,
			   creator = creator,
			   timestamp = ref (TStamp.modtime file),
			   declfile = FnameRules.declFileFor file,
			   binfile = FnameRules.binFileFor file,
			   decl = ref NONE,
			   cu = ref NONE,
			   stable = false,
			   parsed = ref NONE
			  }
	    in
		Table.enter (#table cache, file, s); s
	    end
	  | SOME (s as S { timestamp, stable = false, ... }) => let
		val new_ts = TStamp.modtime file
	    in
		if TStamp.earlier (!timestamp, new_ts) then
		    (update (s, new_ts); s)
		else s
	    end
	  | SOME s => s
    end

    fun mkstable (name, decl) =
	case Table.find (#table cache, name) of
	    SOME (S { stable = true, ... }) => ()
	  | _ => let
		val ts = TStamp.stabletime name
		val s = S {
			   name = name,
			   creator = SOME "stable",
			   timestamp = ref ts,
			   declfile = FnameRules.errorTextFile,
			   binfile = FnameRules.binFileFor name,
			   decl = ref (SOME decl),
			   cu = ref NONE,
			   stable = true,
			   parsed = ref NONE
			  }
	    in
		Table.enter (#table cache, name, s)
	    end

    fun name (S { name, ... }) = name
    fun binfile (S { binfile, ... }) = binfile

    fun parse (S { parsed = ref (SOME p), ... }, true) = p
      | parse (s as S { name, parsed, ... }, quiet) = let
	    val desc = makestring s
	    val _ = if quiet then ()
		    else Control.vsay (concat ["[parsing ", desc, "]\n"])
	    val p = CUnit.parse { file = name, desc = desc }
	    val ntrees = ! (#trees cache)
	in
	    if ntrees < (Control.parse_caching NONE) then
		(parsed := SOME p;
		 (#trees cache) := ntrees + 1)
	    else ();
	    p
	end

    fun forget_parse (S { parsed = p as ref (SOME _), ... }) =
	(p := NONE; (#trees cache) := (!(#trees cache)) - 1)
      | forget_parse _ = ()

    fun warn_stable (TStamp.STABLETSTAMP _, name, action) =
	Control.say (concat ["!% CM Warning: need to ", action, " ",
			     AbsPath.elab name,
			     " (even though it is `stable')\n"])
      | warn_stable _ = ()

    fun decl (s as S { name, timestamp, declfile, decl, ... }) =
	case !decl of
	    SOME d => d
	  | NONE => let
		val df = declfile
	    in
		case Decl.recover (df, !timestamp) of
		    SOME d => (decl := SOME d; d)
		  | NONE => 
			(case !timestamp of
			     TStamp.NOTSTAMP => raise (nonexistent s)
			   | t => let
				 val _ = warn_stable (t, name, "reconsult")
				 val { ast, source } =
				     inDeclParsePhase parse (s, false)
				 val d =
				     Decl.create (ast, df, source)
				     handle exn =>
					 (Compiler.Source.closeSource source;
					  raise exn)
				 val _ = Compiler.Source.closeSource source
			     in
				 decl := SOME d; d
			     end)
	    end

    fun cunit { smlsource, mkcompenv, senvOf, symenvOf, runEnvOf } = let
	val S { timestamp, name, binfile, cu, ... } = smlsource
	val run_code = case runEnvOf of NONE => false | _ => true
	fun changed desc = Control.say
	    (concat
	     ["!* WARNING: ", desc,
	      " was modified after being analyzed.\n",
	      "!* -------  (It might be necessary to re-run the analysis.)\n"])

	fun userCodeExn exn = UserCodeExn (AbsPath.elab name, exn)

	fun work old_cu () = let

	    val (delayed'environ, provided) = mkcompenv ()

	    fun mayrun u =
		case runEnvOf of
		    NONE => SUCCESS u
		  | SOME cvt => let
			val e = cvt (delayed'environ ())
			val de = Env.dynamicPart e
		    in
			(CUnit.execute (u, de);
			 SUCCESS u)
			handle exn => FAILURE (userCodeExn exn)
		    end

	    fun fromfile () = let
		val e = delayed'environ ()
		val senv = senvOf e
		val symenv = symenvOf e
	    in
		case CUnit.recover { srcfile = name,
				     binfile = binfile,
				     se = senv,
				     sourcetime = !timestamp,
				     provided = provided,
				     keep_code = run_code } of
		    SOME { u, ... } => (forget_parse smlsource; mayrun u)
		  | NONE =>
			(case !timestamp of
			     TStamp.NOTSTAMP => raise (nonexistent smlsource)
			   | t => let
				 val _ = warn_stable (t, name, "recompile")
				 val desc = makestring smlsource
				 val _ =
				     Control.vsay
				       (concat ["[compiling ", desc,
						" -> ",
						AbsPath.elab binfile,
						"]\n"])
				 val S { name, ... } = smlsource
				 val _ =
				     if TStamp.earlier
					     (t, TStamp.modtime name) then
					 changed desc
				     else ()
				 val { ast, source } =
				     inCompParsePhase parse (smlsource, true)
			     in
				 forget_parse smlsource;
				 mayrun
				   (CUnit.create {
						  ast = ast,
						  source = source,
						  name = name,
						  binfile = binfile,
						  senv = senv,
						  symenv = symenv,
						  provided = provided,
						  keep_code = run_code
						 })
				 before Compiler.Source.closeSource source
			     end)
	    end handle exn => FAILURE exn
	      
	in
	    case old_cu of
		NONE => fromfile ()
	      | SOME uf =>
		    (case Futures.get uf of
			 FAILURE _ => fromfile ()
		       | SUCCESS u =>	(* so let's check it *)
			     if CUnit.isValid (u, provided, run_code) then
				 mayrun u
			     else
				 fromfile ())
	end

	val new_cu = Futures.future (work (!cu))

    in
	cu := SOME new_cu; new_cu
    end

    fun cunit_again (S { cu = ref (SOME cuf), ... }) = cuf
      | cunit_again _ = raise SmlSourceInternalError

    fun clearcache () = (Table.clear (#table cache); (#trees cache) := 0)

    fun sweepcache () = let
	fun is_bad (S { name, timestamp, stable, ... }) =
	    (not stable) andalso
	    TStamp.earlier (!timestamp, TStamp.modtime name)
	fun keep_good (name, s, (l, t)) =
	    if is_bad s then (l, t) else
		((name, s) :: l,
		 case s of
		     S { parsed = ref NONE, ... } => t
		   | _ => t + 1)
	val (good_list, good_trees) =
	    Table.fold keep_good (# table cache) ([], 0)
    in
	Table.clear (#table cache);
	app (fn (n, s) => Table.enter (#table cache, n, s)) good_list;
	(#trees cache) := good_trees
    end

  end
end
