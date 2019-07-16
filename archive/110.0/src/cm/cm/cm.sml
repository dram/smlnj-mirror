(*
 * cm/cm.sml: `CM' Compilation Manager (constructing the main structure)
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)

local
  structure CmGlobals = struct
    val path = ref (map AbsPath.rigidcur CmConfig.path)
    val rootfile = ref "sources.cm"
  end
in
  functor CompilationManagerFun
    (structure Compiler: COMPILER
     val version: string
     val singlebindir: AbsPath.t option
     val hostconf: Arch.conf
     val targetos: Arch.os): FULL_CM =

  struct

    structure Compiler = Compiler

    val targetconf = { cpu = Arch.cpu Compiler.architecture,
		       os = targetos }

    structure SymVal = SymValFun
	(val conf = targetconf
	 val version = #version_id Compiler.version)

    structure Control = ControlFun (Compiler)

    structure Tools = ToolsFun
	(structure Control = Control)

    structure SysEnv = SysEnvFun (Compiler)

    structure FnameRules = FnameRulesFun
	(structure Compiler = Compiler
	 structure Control = Control
	 val singlebindir = singlebindir
	 val namelength_limited = CmConfig.namelength_limited
	 val targetos = targetos)

    structure ModuleName = ModuleNameFun (Compiler)

    structure ModDecl = ModDeclFun (ModuleName)

    structure Convert = ConvertFun (ModDecl)

    structure Decl = DeclFun
	(structure Convert = Convert
	 structure Control = Control
	 structure Tools = Tools)

    structure ImpExp = ImpExpFun
	(structure MD = ModDecl
	 structure Control = Control)

    structure Iid = IIDFun
	(structure Compiler = Compiler)

    structure CUnit = CUnitFun
	(structure Compiler = Compiler
	 structure Control = Control
	 structure Iid = Iid)

    structure SmlSource = SmlSourceFun
	(structure Decl = Decl
	 structure FnameRules = FnameRules
	 structure CUnit = CUnit
	 structure Control = Control)

    structure YaccSource = YaccSourceFun
	(structure Tools = Tools
	 val command = CmConfig.yacc)

    structure LexSource = LexSourceFun
	(structure Tools = Tools
	 val command = CmConfig.lex)

    structure BurgSource = BurgSourceFun
	(structure Tools = Tools
	 val command = CmConfig.burg)

    structure RcsSource = RcsSourceFun
	(structure Control = Control
	 structure Tools = Tools
	 val command = CmConfig.rcsco)

    structure GroupDag = GroupDagFun
	(structure IE = ImpExp
	 structure SmlSource = SmlSource)

    structure EntityDesc = EntityDescFun
	(structure Decl = Decl
	 structure FnameRules = FnameRules
	 structure SmlSource = SmlSource
	 structure Control = Control
	 structure Tools = Tools)

    structure SysDag = SysDagFun
	(structure GroupDag = GroupDag
	 structure EntityDesc = EntityDesc
	 structure Decl = Decl
	 structure FnameRules = FnameRules
	 structure Control = Control)

    structure DagTraversal = DagTraversalFun (SysDag)

    fun clear () = (SmlSource.clearcache (); EntityDesc.clear ())

    structure Driver = DriverFun
	(structure SysDag = SysDag
	 structure SysEnv = SysEnv
	 structure Control = Control
	 val path = CmGlobals.path
	 val symval = SymVal.lookup)

    structure Dot = DotFun (DagTraversal)

    structure GenDot = GenDotFun
	(structure Dot = Dot
	 structure Driver = Driver)

    structure SAlone = SAloneFun (DagTraversal)

    structure GenSAlone = GenSAloneFun
	(structure SAlone = SAlone
	 structure Driver = Driver)

    structure LinearSched = LinearSchedFun (DagTraversal)
    structure Lists = ListsFun (LinearSched)

    structure GenLists = GenListsFun
	(structure Lists = Lists
	 structure Driver = Driver)

    structure Prune = PruneFun (DagTraversal)

    structure Recompile = RecompileFun
	(structure DagTraversal = DagTraversal
	 structure Driver = Driver
	 structure SysEnv = SysEnv
	 structure Control = Control
	 structure Prune = Prune)

    structure AutoLoad = AutoLoadFun
	(structure Control = Control
	 structure Convert = Convert
	 structure Recompile = Recompile)

    val rootfile = CmGlobals.rootfile
    val verbose = Control.verbose
    val debug = Control.debug
    val keep_going = Control.keep_going
    val parse_caching = Control.parse_caching
    val show_exports = Control.show_exports
    val path = CmGlobals.path

    fun set_path NONE = !path
      | set_path (SOME p) = (EntityDesc.clear ();
			     !path before path := map AbsPath.rigidcur p)

    val set_path = (map AbsPath.elab) o set_path

    val path_separator =
	(case SMLofNJ.SysInfo.getOSKind () of
	     SMLofNJ.SysInfo.UNIX => #":"
	   | _ => #";")
	handle SMLofNJ.SysInfo.UNKNOWN => #";"

    structure Cleanup = CleanupFun
	(val slists = [("CM_PATH", path_separator, set_path)]
	 val bools = [("CM_VERBOSE", verbose),
		      ("CM_DEBUG", debug),
		      ("CM_KEEP_GOING", keep_going),
		      ("CM_SHOW_EXPORTS", show_exports)]
	 val strings = [("CM_ROOT", rootfile)])

    structure Complain = ComplainFun
	(structure GroupDag = GroupDag
	 structure ImpExp = ImpExp
	 structure SysDag = SysDag
	 structure CUnit = CUnit
	 structure Decl = Decl
	 structure Driver = Driver
	 structure ED = EntityDesc
	 structure Recompile = Recompile
	 structure SmlSource = SmlSource
	 structure Control = Control
	 structure Tools = Tools)

    type desc = SysDag.desc

    val version = version

    val canon = AbsPath.rigidcur

    val cmfile = SysDag.CMFILE o canon
    val scfile = SysDag.SCGROUP o canon

    fun autodetect n =
	case Tools.classify (canon n, NONE) of
	    Tools.SCGROUP => cmfile n
	  | Tools.SCLIBRARY => scfile n
	  | _ => cmfile n

    fun default1 f = fn () => f (cmfile (!rootfile))
    fun default2 f = fn arg => f (cmfile (!rootfile), arg)

    fun auto1 f = f o autodetect
    fun auto2 f = fn (s, arg) => f (autodetect s, arg)

    fun set_root r = rootfile := r

    (*
     * '-versions (root description file given explicitly):
     *)
    val dot' = Complain.complaining GenDot.genDot
    val sa' = Complain.complaining GenSAlone.genStandAlone
    val names' = Complain.complaining GenLists.names
    val binfiles' = Complain.complaining GenLists.binfiles
    val strings' = Complain.complaining GenLists.strings
    val recompile' = Complain.complaining Recompile.only
    val make' =
	Complain.complaining (Recompile.and'run (hostconf, targetconf, NONE))
    val mkusefile' = Complain.complaining GenLists.mkusefile
    val autoload' =
	Complain.complaining (Recompile.withAe AutoLoad.register)

    fun testbed' (desc, files) =
	Complain.complaining (Recompile.and'run (hostconf,
						 targetconf, SOME files))
	desc

    fun stabilize' (desc, recursive) =
	Complain.complaining (Recompile.and'stabilize recursive) desc
	before clear ()

    fun destabilize' desc = let

	fun deleteFile name = OS.FileSys.remove name handle _ => ()

	fun d _ = false
	fun v _ = NONE
	    
	val ep = EntityDesc.EP { path = !path,
				 lparm = { strdef = d, sigdef = d, fctdef = d,
					   fsigdef = d, symval = v } }

	fun get_entity (SysDag.CMFILE f) = EntityDesc.read ep f
	  | get_entity (SysDag.SCGROUP f) = EntityDesc.readSCGroup ep f
	  | get_entity (SysDag.SCLIBRARY f) = EntityDesc.readSCLibrary ep f

	val EntityDesc.ENTITY { location, stable, ... } = get_entity desc

    in
	(if stable then
	    deleteFile (AbsPath.elab
			(FnameRules.stableFileFor location))
	 else ());
	clear ()
    end

    (*
     * convenient versions: root description file taken from !rootfile:
     *)
    val dot = default2 dot'
    val sa = default2 sa'
    val names = default1 names'
    val binfiles = default1 binfiles'
    val strings = default1 strings'
    val recompile = default1 recompile'
    val make = default1 make'
    val mkusefile = default2 mkusefile'
    val stabilize = default2 stabilize'
    val destabilize = default1 destabilize'
    val autoload = default1 autoload'

    fun testbed files = testbed' (cmfile "all-files.cm", files)

    (*
     * convenient versions: root description file taken from string argument
     * cmfile/scfile detection is automatic
     *)
    val dot' = auto2 dot'
    val sa' = auto2 sa'
    val names' = auto1 names'
    val binfiles' = auto1 binfiles'
    val strings' = auto1 strings'
    val recompile' = auto1 recompile'
    val make' = auto1 make'
    val mkusefile' = auto2 mkusefile'
    val stabilize' = auto2 stabilize'
    val destabilize' = auto1 destabilize'
    val autoload' = auto1 autoload'

    val testbed' = auto2 testbed'

    val autoloading = let
	val state = ref false
	fun al arg = let
	    val oldstate = !state
	in
	    case arg of
		NONE => ()
	      | SOME newstate =>
		    (state := newstate;
		     if newstate then
			 Compiler.Interact.installCompManager
			      (SOME AutoLoad.manager)
		     else
			 (Compiler.Interact.installCompManager NONE;
			  AutoLoad.clear ()));
	    oldstate
	end
    in
	al
    end

    val clearAutoList = AutoLoad.clear

    local
	fun strip (SysDag.CMFILE s) = AbsPath.elab s
	  | strip (SysDag.SCGROUP s) = AbsPath.elab s
	  | strip (SysDag.SCLIBRARY s) = "-" ^ AbsPath.elab s
    in
	val autoList = (map strip) o AutoLoad.autolist
    end

    type env = Compiler.Environment.environment

    val initCleanup = Cleanup.init
    val uninitCleanup = Cleanup.uninit

    val sweep = SmlSource.sweepcache

    (* statistics *)
    fun withTiming f = Stats.withTiming Control.say f

    (* cmd line *)
    fun procCmdLine () = let
	fun p (f, "sml") = Compiler.Interact.useFile f
	  | p (f, "sig") = Compiler.Interact.useFile f
	  | p (f, "cm") = make' f
	  | p (f, e) =
		(print (concat ["!* unable to process `", f,
				"' (unknown extension `", e, "'\n"]))
	fun c f = (f, String.map Char.toLower
		          (getOpt (OS.Path.ext f, "(none")))
    in
	app (p o c) (SMLofNJ.getArgs ())
    end
  end
end (* local *)
