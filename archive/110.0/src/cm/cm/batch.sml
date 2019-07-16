(*
 * cm/batch.sml:
 *  Construct a batch-compiler for (cross-)compiling SML/NJ's compiler.
 *  (adapted from an earlier version in SC)
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
functor CMBFun (structure Compiler: COMPILER
		val version: string
		val hostcpun: string
		val hostosn: string
		val targetosn: string): BATCH =
struct

    structure P = OS.Path

    val hostconf = { cpu = Arch.cpu hostcpun, os = Arch.os hostosn }
    val targetos = Arch.os targetosn

    val targetcpun = Compiler.Compile.architecture
    val targetcpu = Arch.cpu targetcpun
    val targetconf = { cpu = targetcpu, os = targetos }

    val archsuffix = concat [targetcpun, "-", targetosn]

    fun addarchsuffix (name, confn) =
	P.joinBaseExt { base = name, ext = SOME confn }

    val targetDirStr = addarchsuffix ("bin", archsuffix)
    val targetDir = AbsPath.rigidcur targetDirStr

    val version = version

    structure CM = CompilationManagerFun
	(structure Compiler = Compiler
	 val version = version
	 val singlebindir = SOME targetDir
	 val hostconf = hostconf
	 val targetos = targetos)

    structure Tools = CM.Tools
    structure CU = CM.CUnit
    structure Control = CM.Control
    structure Driver = CM.Driver
    structure Recompile = CM.Recompile
    structure Lists = CM.Lists
    structure SysDag = CM.SysDag
    structure MN = CM.ModuleName
    structure ED = CM.EntityDesc

    structure Compiler = CU.Compiler
    structure Env = Compiler.Environment
    structure BareEnv = Compiler.BareEnvironment

    structure FS = OS.FileSys

    val openTextOut = AbsPath.openTextOut Control.vsay

    val allFiles = "all-files.cm"
    val pervasives = "pervasives.cm"

    val cpuNames =
	[
	 "alpha32", "alpha32x",
	 "mipsel", "mipseb",
	 "hppa",
	 "rs6000",
	 "sparc",
	 "x86"
	]

    val runtimePid = "runtimePidxxxxxx"

    exception BadBootList and BootMatch

    val cpusym = Arch.cpusym

    local
	fun win32FileName s =
	    case P.fromString s of
		{ isAbs = false, vol = "", arcs = arc1 :: arcn } =>
		    concat (arc1 ::
			    foldr (fn (a, r) => "\\" :: a :: r) [] arcn)
	      | _ => raise Fail "batch:win32FileName: bogus"

	val osFileName =
	    case targetos of
		SMLofNJ.SysInfo.WIN32 => win32FileName
	      | _ => (fn s => s)
    in
	fun writeFileName (s, n) =
	    TextIO.output (s, osFileName n ^ "\n")
    end

    fun readBootFileList name = let
	fun no _ = false
	val ep = ED.EP { path = [],
			 lparm = { strdef = no, sigdef = no, fctdef = no,
				   fsigdef = no, symval = CM.SymVal.lookup }}
	val ed = ED.read ep name
    in
	case ed of
	    ED.ENTITY { lib = false, exports = [], members = eml, ... } =>
		let
		    fun fetch (ED.M { name, history,
				      classification = Tools.SMLSOURCE }) =
			name
		      | fetch _ = raise BadBootList
		in
		    map fetch eml
		end
	  | _ => raise BadBootList
    end

    infix //
    infix %

    fun (st, sy) // (st', sy') = (Env.layerStatic (st, st'),
				  Env.layerSymbolic (sy, sy'))

    fun (f % g) e = let
	val e' = f e
	val e'' = g (e' // e)
    in
	e'' // e'
    end

    val add_bin_ext =
	if CmConfig.namelength_limited then
	    fn f => f
	else
	    fn f => AbsPath.extendExt { path = f, ext = "bin", sep = "." }

    fun targetname name = AbsPath.joinDirFile { dir = targetDir, file = name }

    fun barebin f = AbsPath.file (add_bin_ext f)
    fun bin f = targetname (barebin f)

    (*
     * choices for the `process'-parameter to `boot'driver':
     *)

    fun compile mkobj sf (senv, symenv) = mkobj (sf, bin sf, senv, symenv)

    fun getbin mkobj sf (senv, _) =
	(Control.vsay (concat [AbsPath.elab sf, "... "]);
	 CU.fetchObjectEnv (bin sf, senv))

    fun listbootbins (corefile, files, pervfiles, usingDefaultPerv) = let
	fun onebin s file = writeFileName (s, barebin file)
	fun onesrc s file = writeFileName (s, AbsPath.elab file)
	fun dolist (one, l, file) = let
	    val s = openTextOut (targetname file)
	in
	    app (one s) l handle exn => (TextIO.closeOut s; raise exn);
	    TextIO.closeOut s
	end
    in
	if usingDefaultPerv then
	    (dolist (onebin, corefile :: (files @ pervfiles), "BOOTLIST");
	     dolist (onesrc, files, "BOOTSRC"))
	else
	    (dolist (onebin, corefile :: files, "BOOTLIST");
	     dolist (onebin, pervfiles, "PERVLIST");
	     dolist (onesrc, files, "BOOTSRC");
	     dolist (onesrc, pervfiles, "PERVSRC"))
    end

    val modtime = AbsPath.modTime
    fun older (a, b) = Time.< (modtime a, modtime b)
    fun outdated f = older (bin f, f)

    fun makeTargetDir () =
	if FS.access (targetDirStr, []) then
	    Control.vsay (concat ["Directory ", targetDirStr,
				  " already existed\n"])
	else
	    FS.mkDir targetDirStr

    fun cleanTargetDir () = let

	val ds = FS.openDir targetDirStr

	fun deleteFile f =
	    if f = P.currentArc orelse f = P.parentArc then
		()
	    else let
		val p = P.joinDirFile { dir = targetDirStr, file = f }
	    in
		if FS.isDir p then
		    ()
		else
		    FS.remove p
	    end

	fun loop () =
	    case FS.readDir ds of
		"" => FS.closeDir ds
	      | f => (deleteFile f; loop ())

    in
	loop () handle exn => FS.closeDir ds
    end

    val pervEnv = ref (NONE: Env.environment option)

    fun boot'if'necessary () = let

	val bootdir = AbsPath.rigidcur "boot"
	fun bootfile name =
	    AbsPath.native { context = bootdir, spec = name, rigid = true }

	val boot'dummy = bootfile "dummy.sml"
	val boot'core = bootfile "core.sml"
	val boot'assembly = bootfile "assembly.sig"

	val bootAllFiles = bootfile allFiles
	val specialBootFiles = [boot'dummy, boot'core, boot'assembly]

	fun boot'driver (msg, process, files, pervfiles) = let

	    val _ = Control.vsay (msg ^ "...\n")
	    val empty = Env.emptyEnv
	    val bempty = BareEnv.emptyEnv
	    val bsempty = BareEnv.staticPart bempty
	    val dempty = Env.dynamicPart empty
	    val symempty = Env.symbolicPart empty

	    val _ = #set CU.coreEnvRef bsempty
	    val pre_core = process (CU.compileBootFile (NONE, false))
	    val core = process (CU.compileBootFile (SOME runtimePid, true))
	    val normal = process (CU.compileBootFile (NONE, true))

	    fun many [] = raise BootMatch
	      | many [s] = normal s
	      | many (first :: more) = (normal first) % (many more)

	    val prim_env = (Env.primEnv, symempty)

	    val asig_env = pre_core boot'assembly prim_env
	    val asig_prim_env = asig_env // prim_env

	    val dummy_env = pre_core boot'dummy asig_prim_env
	    val dummy_asig_prim_env = dummy_env // asig_prim_env

	    val core_env = core boot'core dummy_asig_prim_env

	    val _ = #set CU.coreEnvRef
		          (Compiler.EnvRef.unSCstaticEnv (#1 core_env))

	    val core_asig_prim_env = core_env // asig_prim_env

	    val many_env = many files core_asig_prim_env

	    val (result_st, result_sy) = many pervfiles many_env

	    val cons_st = Env.consolidateStatic result_st
	    val cons_sy = Env.consolidateSymbolic result_sy

	    val p_env = Env.mkenv { static = cons_st,
				    dynamic = dempty,
				    symbolic = cons_sy }

	in
	    p_env
	end

	val files = readBootFileList bootAllFiles
	val (pervasivesFiles, usingDefaultPerv) =
	    (readBootFileList (bootfile pervasives), false)
	    handle _ => let
		val default'boot'pervasives = bootfile "pervasives.sml"
	    in
		Control.say (concat ["!% trouble reading ", pervasives,
				     " in boot directory -- using \"",
				     AbsPath.elab default'boot'pervasives,
				     "\"\n"]);
		([default'boot'pervasives], true)
	    end
	val allbootfiles = specialBootFiles @ files @ pervasivesFiles
	val _ = makeTargetDir ()
	val isOutdated = List.exists outdated allbootfiles
    in
	case (!pervEnv, isOutdated) of
	    (SOME pe, false) =>
		(Control.vsay "[Using existing pervasive environment]\n"; pe)
	  | (_, true) => let
		val _ = Control.vsay "[Cleaning out binfile directory..."
		val _ = cleanTargetDir ()
		val _ = CM.clear ()	(* clear in-core caches *)
		val _ = Control.vsay "]\n"
		val newpe =
		    boot'driver ("Recompiling boot directory",
				 compile, files, pervasivesFiles)
	    in
		listbootbins (boot'core, files, pervasivesFiles,
			      usingDefaultPerv);
		pervEnv := SOME newpe;
		newpe
	    end
	  | (NONE, false) => let
		val newpe = boot'driver ("Reloading boot environment",
					 getbin, files, pervasivesFiles)
	    in
		Control.vsay "\n";
		pervEnv := SOME newpe;
		newpe
	    end
    end

    fun thinAE (ae, sname) = SysDag.pick_root (ae, MN.structMN sname)

    fun genlists fullAE = let

	fun stripDir f = P.file f
	fun binlist ae = map stripDir (Lists.binfiles ae)

	fun writeList (f, l) = let
	    val os = openTextOut f
	    val _ = app (fn n => writeFileName (os, n)) l
		handle exn => (TextIO.closeOut os; raise exn)
	in
	    TextIO.closeOut os
	end

	fun viscomplist cpun = let
	    val cpu = Arch.cpu cpun
	    val cpus = Arch.cpusym cpu
	    val thisAE = thinAE (fullAE, cpus ^ "VisComp")
	    val binfiles = binlist thisAE
	in
	    writeList (targetname (addarchsuffix ("BINLIST", cpun)), binfiles)
	end

	val viscomplist = CM.Complain.warning viscomplist

	val myAE = thinAE (fullAE, "Int" ^ (Arch.cpusym targetcpu))

	val binfiles = binlist myAE
	val names = Lists.names myAE

    in
	writeList (targetname "SRCLIST", names);
	writeList (targetname "BINLIST", binfiles);
	app viscomplist cpuNames
    end

    fun make' rootname = let
	val e = boot'if'necessary ()
	val st_pe = Env.staticPart e
	val sy_pe = Env.symbolicPart e
	fun action ae = let
	    val compAE =
		case rootname of
		    NONE => ae
		  | SOME "" => thinAE (ae, "All_Pro_Forma")
		  | SOME s => thinAE (ae, s)
	in
	    Control.vsay "Compiling...\n";
	    Recompile.recomp_only (compAE, (st_pe, sy_pe));
	    Control.vsay "Finished Compiling -- creating list files...\n";
	    genlists ae;
	    Control.vsay "Done.\n"
	end

    in
	Driver.driver (action, st_pe)
	              (SysDag.CMFILE (AbsPath.rigidcur allFiles))
    end

    val make' = CM.Complain.complaining make'

    val make = fn () => make' NONE

end
