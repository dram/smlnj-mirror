(*
 * compile/cunit.sml: CM's representation of `compiled units'
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
local
    val compilePhase = Stats.newPhase "Compilation (no parsing)"
    val recoveryPhase = Stats.newPhase "Binfile recovery"
    val savingPhase = Stats.newPhase "Binfile writing"
    val executePhase = Stats.newPhase "Linking (exec)"
    fun inCompilePhase f = Stats.inPhase compilePhase f
    fun inRecoveryPhase f = Stats.inPhase recoveryPhase f
    fun inSavingPhase f = Stats.inPhase savingPhase f
    fun inExecutePhase f = Stats.inPhase executePhase f
in
  functor CUnitFun (structure Iid: IID
		  structure Control: CONTROL): CUNIT = struct

    structure Iid = Iid
    structure Compiler = Iid.Compiler
    structure CUU = Compiler.CUnitUtil

    structure Pid = Compiler.PersStamps
    structure Env = Compiler.Environment
    structure EnvRef = Compiler.EnvRef
    structure Err = Compiler.ErrorMsg
    structure Comp = Compiler.Compile
    structure Print = Compiler.Control.Print
    structure Source = Compiler.Source

    exception FormatError = CUU.FormatError
    exception NoCodeBug = CUU.NoCodeBug
    exception Outdated
    exception Compile of string

    type pid = Pid.persstamp
    type senv = Env.staticEnv
    type denv = Env.dynenv
    type symenv = Env.symenv
    type env = Env.environment
    type iid = Iid.t
    type iidset = Iid.set

    type t = iidset CUU.cunit

    val openBinOut = AbsPath.openBinOut Control.vsay

    fun iid u =
	Iid.new { senv = CUU.staticPidCU u, lambda = CUU.lambdaPidCU u }
    val senv = CUU.senvCU
    val symenv = CUU.symenvCU
    fun env u = !(CUU.envCU u)

    val readUnit = CUU.readUnit
    val writeUnit = CUU.writeUnit
    val makeUnit = CUU.makeUnit

    val codeClosure = CUU.codeClosure
    val discardCode = CUU.discardCode

    val senvCU = CUU.senvCU
    val symenvCU = CUU.symenvCU
    val envCU = CUU.envCU
    val importsCU = CUU.importsCU
    val exportCU = CUU.exportCU
    val referencesCU = CUU.referencesCU
    val nocodeCU = CUU.nocodeCU

    fun sel_imp_exp c = let
	val imports = importsCU c
	val exportPid = exportCU c
    in
	(imports,
	 case exportPid of
	     NONE => []
	   | SOME pid => [pid])
    end

    fun iids_ok provided requested = Iid.isSubset (requested, provided)

    fun check_iids p r = if iids_ok p r then () else raise Outdated

(*
    fun check_iids p r =
	if iids_ok p r then ()
	else let
	    val s = "\np: " ::
		Iid.toHex (p, "\nr: " :: Iid.toHex (r, ["\n"]))
	in
	    Control.vsay (concat s);
	    raise Outdated
	end
*)

    fun dont_bother _ = ()

    fun pids2iids check pids = let
	fun pairup ([], iids) = iids
	  | pairup (p1 :: p2 :: pn, iids) =
	    pairup (pn, Iid.new { senv = p1, lambda = p2 } :: iids)
	  | pairup _ = raise FormatError
	val iids = Iid.makeset (pairup (pids, []))
    in
	check iids;
	iids
    end

    (*
     * recover: binfile * senv * sourcetime option * pids provided -> t option
     *  - recover a compiled unit from the binfile specified (if
     *    possible) unless the source was newer or the unit
     *    is incompatible with the pids provided.
     *  - if everything is ok then return SOME (unit, bintime), otherwise NONE.
     *)
    fun recover { srcfile, binfile, se, sourcetime, provided, keep_code } = let
	val bintime = TStamp.modtime binfile
	val _ =
	    if TStamp.earlier (bintime, sourcetime) then raise Outdated else ()
	val _ = Control.vsay (concat ["[recovering ",
				      AbsPath.elab binfile, "..."])
	val s = AbsPath.openBinIn binfile
	val u = readUnit { stream = s,
			   name = AbsPath.elab srcfile,
			   pids2iid = pids2iids (check_iids provided),
			   senv = se,
			   keep_code = keep_code }
	    handle exn => (BinIO.closeIn s;
			   Control.vsay " failed]\n"; raise exn)
    in
	Control.vsay " done]\n";
(*
	Control.vsay (concat ["staticPid: ",
			      Pid.toHex (CUU.staticPidCU u),
			      "\n"]);
*)
	BinIO.closeIn s;
	SOME { u = u, bintime = bintime }
    end handle _ => NONE

    val recover = inRecoveryPhase recover

    fun deleteFile name = OS.FileSys.remove name handle _ => ()

    fun saveUnit (binfile, u, keep_code) = let
	val s = openBinOut binfile
	fun iids2pids iids = let
	    fun add (iid, r) = (Iid.staticPid iid) :: (Iid.lambdaPid iid) :: r
	in
	    foldr add [] (Set.makelist iids)
	end
	fun writer () =
	    writeUnit { stream = s, cunit = u, keep_code = keep_code,
		        iid2pids = iids2pids }
	val _ = Interrupt.guarded writer
	    handle exn => (BinIO.closeOut s; raise exn)
    in
	BinIO.closeOut s;
	Control.vsay (concat ["[wrote ", AbsPath.elab binfile, "]\n"]);
	()
    end handle exn => let
	val binstring = AbsPath.elab binfile
    in
	deleteFile binstring;
	Control.vsay (concat ["[writing ", binstring, " failed]\n"]);
	raise exn
    end

    val saveUnit = inSavingPhase saveUnit

    (*
     * make our own `coreEnvRef' so `Batch' can set it...
     *)
    val coreEnvRef = let
	val r = ref (#get EnvRef.core ())
    in
	{ get = fn () => !r, set = fn x => (r := x) }
    end

    fun check errs phase =
	if Err.anyErrors errs then raise Compile (phase ^ " failed") else ()

    (*
     * create: ast * source * binfile * senv * pids provided -> t
     *  - create a compiled unit from an abtract syntax tree and
     *    a static environment by compiling the source file
     *  - cache the unit as a binary file in the file system
     *)
    fun create_pid (pidopt, splitting) args = let
	val { ast, source, name, binfile,
	      senv, symenv, provided, keep_code } = args
	val errors = Err.errors source
	val corenv = #get coreEnvRef ()

	val cinfo = Comp.mkCompInfo (source, corenv, fn x => x)

	val { absyn, newenv, exportLvars, staticPid, exportPid,
	      pickle = newenvPickle } =
	    Comp.elaborate { compInfo = cinfo, compenv = senv, ast = ast }
	    before check errors "elaboration"

	val absyn =
	    Comp.instrument { compenv = senv, source = source,
			      compInfo = cinfo }
	                    absyn

	val { genLambda, imports } =
	    Comp.translate { compInfo = cinfo, absyn = absyn,
			     exportLvars = exportLvars,
			     newstatenv = newenv,
			     oldstatenv = senv,
			     exportPid = exportPid }
	    before check errors "translation"

	val imports =
	    case pidopt of
		NONE => imports
	      | SOME s => let
		    val _ = Control.vsay ("fake import pid = \"" ^ s ^ "\"\n")
		    val p = Pid.fromBytes (Byte.stringToBytes s)
		in
		    case imports of
			[_] => [p]
		      | _ => raise Compile "core compilation failed"
		end

	val lambda = Comp.inline { genLambda = genLambda,
				   imports = imports,
				   symenv = symenv }

	val { lambda_e, lambda_i } =
	    Comp.split { lambda = lambda, enable = splitting }

	val code = Comp.codegen { compInfo = cinfo, lambda = lambda_e }
	    before check errors "codegen"

	val u = makeUnit { imports = imports,
			   exportPid = exportPid,
			   references = provided,

			   staticPid = staticPid,
			   newenv = newenv,
			   newenvPickle = newenvPickle,

			   lambda_i = lambda_i,
			   code = code }
    in
	saveUnit (binfile, u, keep_code);
	u
    end

    val create_pid = fn x => fn y => inCompilePhase (create_pid x) y

    fun create x = create_pid (NONE, true) x

    fun isValid (u, provided, flag) =
	if nocodeCU u andalso flag then false
	else iids_ok provided (referencesCU u)

    fun parse { file, desc } = let
	val makeSource = Source.newSource
	val cparse = Comp.parse
	val s = AbsPath.openTextIn file
	val source = makeSource (desc, 1, s, false,
				 { linewidth = !Print.linewidth,
				   flush = Print.flush,
				   consumer = Print.say })
	val ast = cparse source
	    handle Comp.Compile msg => (TextIO.closeIn s; raise Compile msg)
		 | exn => (TextIO.closeIn s; raise exn)
    in
	TextIO.closeIn s;
	{ ast = ast, source = source }
    end

    fun execute (u, denv) = let
	val env = envCU u
	val clos = codeClosure u
	val de = Comp.execute { executable = codeClosure u,
			        imports = importsCU u,
			        exportPid = exportCU u,
				dynenv = denv }
	val e = Env.mkenv { static = senvCU u, dynamic = de,
			    symbolic = symenv u }
    in
	env := SOME e;
	e
    end

    val execute = fn x => inExecutePhase execute x

    fun compileBootFile (pidopt, splitting) (sf, bf, senv, sye) = let
	val _ = Control.vsay (concat ["[compiling (boot) ",
				      AbsPath.elab sf,
				      " -> ", AbsPath.elab bf, "]\n"])
	val sfs = AbsPath.elab sf
	val { ast, source } = parse { file = sf, desc = sfs }
	val u = create_pid (pidopt, splitting)
	         { ast = ast, source = source, name = sf,
		   binfile = bf, senv = senv, symenv = sye,
		   provided = Set.empty, keep_code = false }
	val _ = Source.closeSource source
    in
	(senvCU u, symenv u)
    end

    fun fetchUnit (bf, senv) = let
	val s = AbsPath.openBinIn bf
	val cu = readUnit { stream = s,
			    name = AbsPath.elab bf,
			    pids2iid = pids2iids dont_bother,
			    senv = senv,
			    keep_code = true }
	    handle exn => (BinIO.closeIn s; raise exn)
    in
	BinIO.closeIn s; cu
    end

    fun fetchObjectEnv (bf, se) = let
	val u = fetchUnit (bf, se)
    in
	discardCode u;
	(senvCU u, symenv u)
    end

  end
end
