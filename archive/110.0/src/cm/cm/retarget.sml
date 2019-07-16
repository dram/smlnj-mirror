(*
 * cm/retarget.sml: Constructing cross-batch-compilers
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
structure CMR: sig

    val retarget: { bindir: string, cpu: string, os: string } -> unit
    functor CMBFun: CMB_FUN

end = struct

    functor CMBFun = CMBFun

    structure Env = Compiler.Environment
    structure BareEnv = Compiler.BareEnvironment
    structure EnvRef = Compiler.EnvRef
    structure Symbol = Compiler.Symbol

    structure CM = CMB.CM
    structure CU = CM.CUnit

    fun addarchsuffix (name, cpun) = concat [name, ".", cpun]

    val cpusym = Arch.cpusym

    fun load_binfiles (bindir, listfile) = let

	val layer = Env.layerEnv
	fun binname name = AbsPath.joinDirFile { dir = bindir, file = name }
	val is = TextIO.openIn (AbsPath.elab (binname listfile))

	fun all_files () = let

	    val perv = #get EnvRef.pervasive ()

	    fun one_file env = let
		val line = TextIO.inputLine is
		val binfile = substring (line, 0, size line - 1)
		val binpath = binname binfile
		val _ = CM.Control.vsay (concat ["[reading ",
						 AbsPath.elab binpath,
						 "]\n"])
		val full = layer (env, perv)
		val cu = CU.fetchUnit (binpath, Env.staticPart full)
	    in
		CU.execute (cu, Env.dynamicPart full)
	    end

	    fun loop env =
		if TextIO.endOfStream is then
		    env
		else
		    loop (layer (one_file env, env))
	in
	    loop (Env.emptyEnv)
	end

	val env = all_files () handle exn => (TextIO.closeIn is; raise exn)
    in
	TextIO.closeIn is; env
    end

    fun filtered_load (bindir, listfile, sym) = let
	val lenv = load_binfiles (bindir, listfile)
	val fenv = Env.filterEnv (lenv, [sym]) 
	val nenv =
	    BareEnv.concatEnv (EnvRef.unSCenv fenv, #get EnvRef.topLevel ())
    in
	#set EnvRef.topLevel nenv
    end

    fun load_compiler (bindir, cpun, compname) = let
	val listfile = addarchsuffix ("BINLIST", cpun)
	val sym = Symbol.strSymbol compname
    in
	filtered_load (bindir, listfile, sym)
    end

    val hostcpu = Arch.cpu Compiler.architecture
    val hostos = SMLofNJ.SysInfo.getOSKind ()
    val hostconf = { cpu = hostcpu, os = hostos }
    val hostcpun = Arch.cpuname hostcpu
    val hostosn = Arch.osname hostos

    fun new_compiler (compname, targetosn) =
	concat [
		"structure CMB = CMR.CMBFun (structure Compiler = ", compname,
		" val version = \"", CMB.version,
		" (retarget)\" val hostcpun = \"", hostcpun,
		"\" val hostosn = \"", hostosn, "\"\n",
		" val targetosn = \"", targetosn, "\") \n"
		]
	
    fun retarget { bindir, cpu = targetcpun, os = targetosn } = let
	val bindir = AbsPath.rigidcur bindir
	val targetcpus = Arch.cpusym (Arch.cpu targetcpun)
	val compname = targetcpus ^ "VisComp"
    in
	load_compiler (bindir, targetcpun, compname);
	Compiler.Interact.useStream 
	  (TextIO.openString (new_compiler (compname, targetosn)))
    end

    val retarget = CM.Complain.complaining retarget

end




