(*
 * cm/complain.sml: Guard functions with exception handling/error reporting
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
functor ComplainFun (structure GroupDag: GROUP_DAG
		     structure ImpExp: IMP_EXP
		     structure SysDag: SYS_DAG
		     structure CUnit: CUNIT
		     structure Decl: DECL
		     structure Driver: DRIVER
		     structure ED: ENTITY_DESC
		     structure Recompile: RECOMPILE
		     structure SmlSource: SML_SOURCE
		     structure Control: CONTROL
		     structure Tools: TOOLS
		     sharing SmlSource = GroupDag.SmlSource
		     and GroupDag.ModuleName = ImpExp.ModuleName): COMPLAIN =
struct

    structure MN = ImpExp.ModuleName

    val say = Control.say
    structure Compile = ImpExp.Compiler.Compile

    fun msg lab l = Control.say (concat ["\n!* ", lab, concat l, "\n"])

    val err = msg "CM error: "
    val bug = msg "CM bug: "
    val warnerr = msg "CM warning: "
    val warnbug = msg "CM bug warning: "

    fun printExn (Compile.TopLevelException e) = printExn e
      | printExn Compile.SilentException = ()
      | printExn exn = let
	fun showhist [s] = say (concat ["  raised at: ", s, "\n"])
	  | showhist (s :: r) =
	    (showhist r; say (concat["             ", s, "\n"]))
	  | showhist [] = ()
	val exnMsg = (case exn
			  of (Compile.Compile s) =>
			      concat ["Compile: \"", s, "\""]
			| _ => General.exnMessage exn)
    in
	say (concat ["\nuncaught exception ", exnMsg, "\n"]);
	showhist (SMLofNJ.exnHistory exn)
    end

    fun names [] = ""
      | names [n] = n
      | names [n1, n2] = concat [n1, " and ", n2]
      | names [n1, n2, n3] = concat [n1, ", ", n2, ", and ", n3]
      | names (hd :: tl) = concat [hd, ", ", names tl]

    fun waswere [_] = "was"
      | waswere _ = "were"

    fun moan (err, bug) = let
      fun moan' exn =
	case exn of
	    GroupDag.MultipleDefinitions (n, f1, f2) =>
		err [f1, ", ", f2, ": multiple definitions for ", n]
	  | GroupDag.Cycle (s, l) => let
		fun track ([], _) = []
		  | track ((sd, n) :: l, su) = let
			val r = track (l, sd)
		    in
			"    in " :: SmlSource.makestring su ::
			" a reference to " :: MN.makestring n ::
			" (defined in " :: SmlSource.makestring sd :: ")\n" ::
			r
		    end
	    in
		err ("A cycle has been detected among SML definitions.\n" ::
		     "  While processing " ::
		     SmlSource.makestring s ::
		     " the following cycle was discovered:\n" ::
		     track (l, s))
	    end
	  | GroupDag.IllegalToplevelOpen s =>
		err [s, ": toplevel `open'"]
	  | GroupDag.GroupDagInternalError =>
		bug ["internal error in GroupDag"]
	  | ImpExp.Undefined n =>
		bug ["undefined ", MN.makestring n, " not handled in ImpExp"]
	  | ImpExp.IllegalToplevelOpen =>
		bug ["IllegalToplevelOpen from ImpExp"]
	  | ImpExp.InternalError s =>
		bug ["internal error in ImpExp: ", s]
	  | SysDag.EntityCycle l => let
		fun f (p, r) =
		    "    " :: AbsPath.elab p :: "\n" :: r
	    in
		err ("A cycle has been detected among groups and libraries.\n"
		     :: "  The entity description files involved are:\n" ::
		     foldr f [] l)
	    end
	  | SysDag.MultipleDefinitions (nl, e, sf1, sf2) =>
		err [e, ": ", names nl, " ", waswere nl,
		     " imported from both ", sf1, " and ", sf2]
	  | SysDag.ExportedNamesNotDefined (nl, e) =>
		err [e, ": ", names nl, " ", waswere nl,
		     " not defined anywhere"]
	  | SysDag.Stabilize f =>
		err ["missing prerequisite for stabilization: ", f]
	  | SysDag.MultiMember { m, d, d' = NONE } =>
		err ["ML file ", m, " occurs more than once in ", d]
	  | SysDag.MultiMember { m, d, d' = SOME d' } =>
		err ["ML file ", m, " occurs in more than one group: ",
		     d, ", ", d']
	  | SysDag.SysDagInternalError =>
		bug ["internal error in SysDag"]
	  | CUnit.FormatError =>
		err ["binfile doesn't have the required format"]
	  | CUnit.Outdated =>
		bug ["cannot recover from an outdated binary file"]
	  | CUnit.Compile s =>
		err ["compile: ", s]
	  | CUnit.NoCodeBug =>
		bug ["executable code not available"]
	  | Decl.InternalError =>
		bug ["internal error in Decl"]
	  | Decl.FormatError =>
		err ["declfile doesn't have the required format"]
	  | Tools.UnknownClass s =>
		err ["don't know source class name ", s]
	  | Tools.ToolError { tool, msg } =>
		err [tool, " failed: ", msg]
	  | ED.BadEntityDescription (f, s) =>
		err [f, ": syntax error: ", s]
	  | ED.FileNotFound f =>
		err [f, ": description file not found"]
	  | ED.AliasNestingTooDeep f =>
		err [f, ": too many nested aliases (cycle?)"]
	  | MN.ModuleNameError =>
		bug ["internal error in ModuleName"]
	  | MN.PathError =>
		bug ["internal error in ModuleName (PathError)"]
	  | Recompile.RecompileInternalError =>
		bug ["internal error in Recompile"]
	  | Recompile.WrongConfiguration (comp, host) =>
		err ["code compiled for ", comp,
		     " cannot run on ", host]
	  | Recompile.CompilationErrors exnlist =>
		app moan' exnlist
	  | SmlSource.SourceFileDoesNotExist s =>
		err [s, ": SML source file not found"]
	  | SmlSource.UserCodeExn (f, e) =>
		(err [f, ": exception raised in user code"]; moan' e)
	  | SmlSource.SmlSourceInternalError =>
		bug ["internal error in SmlSource"]
	  | Lexer.LexicalError (f, s) =>
		err [f, ": lexical error: ", s]
	  | Lexer.SyntaxError (f, s) =>
		err [f, ": syntax error (preprocessor): ", s]
	  | Lexer.UserError (f, msg) =>
		err [f, ": ", msg]
	  | Arch.BadConf s =>
		err ["unrecognized target configuration name: ", s]
	  | Arch.BadCpu s =>
		err ["unrecognized cpu type: ", s]
	  | Arch.BadOS s =>
		err ["unrecognized os type: ", s]
	  | _ => printExn exn
    in
	moan'
    end

    val complain = moan (err, bug)
    val warn = moan (warnerr, warnbug)

    fun complaining f arg =
	f arg handle exn => (complain exn; raise Compile.SilentException)
    fun warning f arg = f arg handle exn => warn exn

end
