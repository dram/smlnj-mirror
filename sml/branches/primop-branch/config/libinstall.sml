(* SML/NJ installer script -- written in SML.
 *   This script runs after the runtime system has been built and
 *   the interactive system has been booted from bootfiles.
 *
 * The remainder of the build process compiles additional libraries
 * and installs certain standalone programs such as ml-yacc and ml-lex.
 * This is the bulk of what used to be done by install.sh.
 *
 * The script is written in such a way that it can be used portably
 * on both *nix- and win32-systems.
 *
 * (C) 2003 The Fellowship of SML/NJ
 * 
 * Author: Matthias Blume (blume@tti-c.org)
 *)
structure LibInstall : sig

    (* all filenames that are passed as arguments use native syntax: *)
    val proc :
	{ smlnjroot: string,
	  buildcmd: string,
	  instcmd : string -> unit,
	  unpackcmd: string option } -> unit

end = struct

    structure P = OS.Path
    structure F = OS.FileSys
    structure SI = SMLofNJ.SysInfo
    structure SM = RedBlackMapFn (type ord_key = string
				  val compare = String.compare)

    structure SCC = GraphSCCFn (type ord_key = string
				val compare = String.compare)

    fun say l = TextIO.output (TextIO.stdErr, concat l)
    fun fail l = (say ("FAILURE: " :: l);
		  OS.Process.exit OS.Process.failure)

    (* figure out who and what we are *)
    val arch = String.map Char.toLower (SMLofNJ.SysInfo.getHostArch ())
    val (isUnix, oskind) = case SI.getOSKind () of
			       SI.UNIX => (true, "unix")
			     | SI.WIN32 => (false, "win32")
			     | _ => fail ["os kind not supported\n"]

    val arch_oskind = concat [arch, "-", oskind]
    val heap_suffix = SMLofNJ.SysInfo.getHeapSuffix ()

    (* File names in configuration files are used across all platforms
     * and for that reason are written using CM's standard syntax
     * which is like Unix pathname syntax. *)

    (* standard arc separator is / *)
    fun usep #"/" = true
      | usep _ = false

    (* convert standard syntax to native syntax *)
    fun native f =
	case String.fields usep f of
	    "" :: arcs => P.toString { vol = "", isAbs = true, arcs = arcs }
	  | arcs => P.toString { vol = "", isAbs = false, arcs = arcs }

    fun fexists f = F.access (f, []) handle _ => false

    (* several worklists for delayed execution *)
    val stablist : (unit -> bool) list ref = ref []
    val movlist  : (unit -> unit) list ref = ref []
    val salist : (unit -> unit) list ref = ref []

    (* make a directory (including parent, parent's parent, ...) *)
    fun mkdir "" = ()
      | mkdir d = if fexists d then () else (mkdir (P.dir d); F.mkDir d)

    (* move a stable library file to its final location *)
    fun movelib src dst () =
	(mkdir (P.dir dst); F.rename { old = src, new = dst })

    (* register a temporary anchor-value binding *)
    fun localanchor { anchor, path } =
	#set (CM.Anchor.anchor anchor) (SOME (native path))

    fun getInputTokens s =
	Option.map (String.tokens Char.isSpace) (TextIO.inputLine s)

    (* Take a list of modules and dependencies (in depfile) and
     * build the transitive closure of those modules.
     * We do this by considering the dependency graph and construct
     * a topological order for it. *)
    fun resolve (modules, depfile) = let
	val s = TextIO.openIn depfile
	fun rd m =
	    case getInputTokens s of
		NONE => (TextIO.closeIn s; fn x => getOpt (SM.find (m, x), []))
	      | SOME (x :: xs) => rd (SM.insert (m, x, xs))
	      | SOME [] => rd m
	fun strip (SCC.SIMPLE c) = c
	  | strip _ = fail ["cyclic dependencies in ", depfile, "\n"]
    in
	rev (map strip (SCC.topOrder' { roots = modules,
					follow = rd SM.empty }))
    end

    (* do all the delayed stuff: *)

    (* stabilization of libraries... *)
    fun dostabs () =
	foldr (fn (f, true) => f () | (_, false) => false) true (!stablist)

    (* move stable library files to their final locations... *)
    fun domoves () =
	(app (fn f => f ()) (rev (!movlist)); true)
	handle _ => false

    (* build those standalone programs that require libraries
     * and, therefore, must be compiled "late"... *)
    fun dolatesas () =
	(app (fn f => f ()) (rev (!salist)); true)
	handle _ => false

    (* our main routine *)
    fun proc { smlnjroot, buildcmd, instcmd, unpackcmd } = let
	val smlnjroot = F.fullPath smlnjroot
	val configdir = P.concat (smlnjroot, "config")

	(* dependency file: config/dependencies *)
	val depfile = P.concat (configdir, "dependencies")

	(* find and open first usable targetsfiles *)
	val targetsfiles =
	    [P.concat (configdir, "targets.customized"),
	     P.concat (configdir, "targets")]

	val s =
	    case List.find fexists targetsfiles of
		SOME f => TextIO.openIn f
	      | NONE => fail ["no targetsfiles\n"]

	(* parse the targets file *)
	fun loop (mv, ml) =
	    case TextIO.inputLine s of
		NONE => (TextIO.closeIn s; (mv, ml))
	      | SOME l =>
		  if String.sub (l, 0) = #"#" then loop (mv, ml)
		  else (case String.tokens Char.isSpace l of
			    ["dont_move_libraries"] => loop (false, ml)
			  | ["move_libraries"] => loop (true, ml)
			  | ["request", module] => loop (mv, module :: ml)
			  | [] => loop (mv, ml)
			  | _ => fail ["ill-formed targets line: ", l])

	val (move_libraries, modules) = loop (true, [])

	(* now resolve dependencies; get full list of modules
	 * in correct build order: *)
	val modules = resolve (modules, depfile)

	(* fetch and unpack source trees, using auxiliary helper command
	 * which takes the root directory as its first and the module
	 * names to be fetched as subsequent arguments. *)
	val _ = case unpackcmd of
		    NONE => ()		(* archives must exist *)
		  | SOME cmd => let
			val cmdline =
			    concat (cmd :: " " :: smlnjroot :: " " ::
				    foldr (fn (f, l) => " " :: f :: l)
					  [] modules)
		    in
			if OS.Process.system cmdline = OS.Process.success
			then ()
			else fail ["unpacking failed\n"]
		    end

        val libdir = P.concat (smlnjroot, "lib")
        val srcdir = P.concat (smlnjroot, "src")
	val bindir = P.concat (smlnjroot, "bin")
	val heapdir = P.concat (bindir, ".heap")
	val cm_pathconfig = P.concat (libdir, "pathconfig")

	(* add an entry to lib/pathconfig *)
	fun write_cm_pathconfig (a, p) = let
	    val s = TextIO.openAppend cm_pathconfig
	in
	    TextIO.output (s, concat [a, " ", p, "\n"])
	    before TextIO.closeOut s
	end

	(* at the end, read lib/pathconfig and eliminate duplicate entries *)
	fun uniqconfig () = let
	    fun finish m = let
		val s = TextIO.openOut cm_pathconfig
		fun one (key, value) =
		    TextIO.output (s, concat [key, " ", value, "\n"])
	    in
		SM.appi one m; TextIO.closeOut s
	    end
	    val s = TextIO.openIn cm_pathconfig
	    fun loop m =
		case getInputTokens s of
		    NONE => (TextIO.closeIn s; finish m)
		  | SOME [key, value] => loop (SM.insert (m, key, value))
		  | SOME l => (say ("funny line in " :: cm_pathconfig :: ":" ::
				    foldr (fn (x, l) => " " :: x :: l)
					  ["\n"] l);
			       loop m)
	in
	    loop SM.empty
	end

	(* register library to be built *)
	fun reglib { anchor, altanchor, relname, dir } = let
	    (* anchor: the anchor name currently used by the library
	     *   to be registered for compilation
	     * altanchor: optional alternative anchor name which is
	     *   to be used once the library is in its final location
	     *   (this must be used if "anchor" is already bound
	     *   and used for other libraries which come from the
	     *   bootfile bundle),
	     * relname: path to library's .cm file relative to anchor
	     *   (standard syntax)
	     * dir: directory name that anchor should be bound to,
	     *   name is relative to srcdir and in standard syntax *)
	    val nrelname = native relname
	    val ndir = native dir
	    val libname = concat ["$", anchor, "/", relname]
	    val adir = P.concat (srcdir, ndir)
	    val finalanchor = getOpt (altanchor, anchor)
	    val { dir = nreldir, file = relbase } = P.splitDirFile nrelname
	    val relloc =
		P.concat (nreldir, P.concat ("CM",
					    P.concat (arch_oskind, relbase)))
	    val srcfinalloc = P.concat (adir, relloc)
	    val (finalloc, finalconfigpath) =
		if move_libraries then
		    (P.concat (libdir,
			       P.concat (finalanchor, relloc)),
		     finalanchor)
		else
		    (srcfinalloc, adir)
	in
	    if fexists finalloc then
		say ["Library ", libname, " already exists in ",
		     finalloc, "\n"]
	    else if not (fexists (P.concat (adir, nrelname))) then
		fail ["Source tree for ", libname, " at ",
		      P.concat (adir, nreldir), "(", relbase,
		      ") does not exist.\n"]
	    else
		(say ["Scheduling library ", libname, " to be built as ",
		      finalloc, "\n"];
		 stablist := (fn () => CM.stabilize false libname)
			     :: !stablist;
		 #set (CM.Anchor.anchor anchor) (SOME adir);
		 if move_libraries then
		     movlist := movelib srcfinalloc finalloc :: !movlist
		 else ();
		 write_cm_pathconfig (finalanchor, finalconfigpath))
	end

	(* build a standalone program, using auxiliary build script *)
	fun standalone { target, optheapdir, optsrcdir } = let
	    (* target: name of program; this is the same as the basename
	     *   of the heap image to be generated as well as the
	     *   final arc of the source tree's directory name
	     * optheapdir: optional subdirectory where the build command
	     *   drops the heap image
	     * optsrcdir:
	     *   The source tree for target is located in a directory
	     *   named the same as the target itself.  Normally it is
	     *   a subdirectory of srcdir.  With optsrcdir one can specify
	     *   an alternative for srcdir by giving a path relative to
	     *   the original srcdir. *)
	    val heapname = concat [target, ".", heap_suffix]
	    val targetheaploc =
		case optheapdir of
		    NONE => heapname
		  | SOME hd => P.concat (native hd, heapname)
	    val mysrcdir =
		case optsrcdir of
		    NONE => srcdir
		  | SOME sd => P.concat (srcdir, native sd)
	    val finalheaploc = P.concat (heapdir, heapname)
	    val treedir = P.concat (mysrcdir, target)
	in
	    if fexists finalheaploc then
		say ["Target ", target, " already exists.\n"]
	    else if not (fexists treedir) then
		fail ["Source tree for ", target, " at ", treedir,
		      " does not exist.\n"]
	    else
		(say ["Building ", target, ".\n"];
		 F.chDir treedir;
		 if OS.Process.system buildcmd = OS.Process.success then
		     if fexists targetheaploc then
			 (F.rename { old = targetheaploc,
				     new = finalheaploc };
			  instcmd target;
			  #set (CM.Anchor.anchor target) (SOME bindir))
		     else
			 fail ["Built ", target, "; ", heapname,
			       " still missing.\n"]
		 else
		     fail ["Building ", target, " failed.\n"];
		 write_cm_pathconfig (target, P.concat (P.parentArc, "bin"));
		 F.chDir smlnjroot)
	end

	(* ------------------------------ *)

	(* abbreviations *)
	fun r (a, r, d) = reglib { anchor = a, relname = r, dir = d,
				   altanchor = NONE }
	fun r' (a, r, d, aa) = reglib { anchor = a, relname = r, dir = d,
					altanchor = SOME aa }
	fun a (anch, p) = localanchor { anchor = anch, path = p }

	fun sa (t, d) =
	    standalone { target = t, optheapdir = d, optsrcdir = NONE }

	fun sa' (t, d, s) =
	    standalone { target = t, optheapdir = d, optsrcdir = SOME s }

	(* ------------------------------ *)

	(* process one module *)
	fun one "smlnj-lib" =
	    (if isUnix then
		 r ("unix-lib.cm", "unix-lib.cm", "smlnj-lib/Unix")
	     else ();
	     r ("inet-lib.cm", "inet-lib.cm", "smlnj-lib/INet");
	     r ("regexp-lib.cm", "regexp-lib.cm", "smlnj-lib/RegExp");
	     r ("reactive-lib.cm", "reactive-lib.cm", "smlnj-lib/Reactive");
	     r ("hash-cons-lib.cm", "hash-cons-lib.cm", "smlnj-lib/HashCons"))
	  | one "cml" =
	    (r ("cml", "core-cml.cm", "cml/src");
	     r ("cml", "cml-internal.cm", "cml/src");
	     r ("cml", "cml.cm", "cml/src");
	     r ("cml", "basis.cm", "cml/src"))
	  | one "cml-lib" =
	    (r ("cml-lib", "trace-cml.cm", "cml/cml-lib/cm-descr");
	     r ("cml-lib", "smlnj-lib.cm", "cml/cml-lib/cm-descr"))
	  | one "eXene" =
	    (r ("eXene.cm", "eXene.cm", "eXene"))
	  | one "ckit" =
	    (r ("ckit-lib.cm", "ckit-lib.cm", "../ckit/src"))
	  | one "ml-nlffi-lib" =
	    (r ("c", "memory/memory.cm", "ml-nlffi-lib");
	     r ("c", "internals/c-int.cm", "ml-nlffi-lib");
	     r ("c", "c.cm", "ml-nlffi-lib"))
	  | one "pgraph-util" =
	    (r ("pgraph-util.cm", "pgraph-util.cm", "cm/pgraph"))
	  | one "mlrisc" =
	    (a ("Control.cm", "lib/SMLNJ-MLRISC");
	     a ("Lib.cm", "lib/SMLNJ-MLRISC");
	     a ("Visual.cm", "lib/SMLNJ-MLRISC");
	     a ("MLRISC.cm", "lib/SMLNJ-MLRISC");
	     a ("MLTREE.cm", "lib/SMLNJ-MLRISC");
	     a ("Graphs.cm", "lib/SMLNJ-MLRISC");
	     a ("IA32.cm", "lib/SMLNJ-MLRISC");
	     a ("Peephole.cm", "src/MLRISC/cm");
	     r' ("OTHER-MLRISC", "RA.cm", "MLRISC/cm", "SMLNJ-MLRISC");
	     r' ("OTHER-MLRISC", "Peephole.cm", "MLRISC/cm", "SMLNJ-MLRISC");
	     r' ("OTHER-MLRISC", "IA32-Peephole.cm", "MLRISC/cm", "SMLNJ-MLRISC"))
	  | one "mlrisc-tools" =
	    (r ("mlrisc-tools", "pp.cm", "MLRISC/Tools");
	     r ("mlrisc-tools", "source-map.cm", "MLRISC/Tools");
	     r ("mlrisc-tools", "sml-ast.cm", "MLRISC/Tools");
	     r ("mlrisc-tools", "prec-parser.cm", "MLRISC/Tools");
	     r ("mlrisc-tools", "parser.cm", "MLRISC/Tools");
	     r ("mlrisc-tools", "match-compiler.cm", "MLRISC/Tools"))
	  | one "ml-yacc" =
	      sa ("ml-yacc", SOME "src")
	  | one "ml-lex" =
	      sa ("ml-lex", NONE)
	  | one "ml-burg" =
	      sa ("ml-burg", NONE)
	  | one "ml-nlffigen" =
	      salist := (fn () => sa ("ml-nlffigen", NONE))
			:: !salist
	  | one "nowhere" =
	      salist := (fn () => sa' ("nowhere", NONE, "MLRISC/Tools"))
			:: !salist
	  | one module = fail ["unknown module: ", module, "\n"]
    in
	(app one modules;
	 if dostabs () andalso domoves () andalso dolatesas () then
	     uniqconfig ()
	 else fail ["stabilization failed\n"])
	handle e => fail ["unexpected exception: ",
			  General.exnMessage e, "\n"];
	OS.Process.exit OS.Process.success
    end
end
