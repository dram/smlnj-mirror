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
	  installdir: string,
	  buildcmd: string,
	  instcmd : string -> unit,
	  unpack: (string list -> bool) option } -> unit

end = struct

    structure P = OS.Path
    structure F = OS.FileSys
    structure SI = SMLofNJ.SysInfo
    structure SM = RedBlackMapFn (type ord_key = string
				  val compare = String.compare)
    structure SS = RedBlackSetFn (type ord_key = string
				  val compare = String.compare)

    structure SCC = GraphSCCFn (type ord_key = string
				val compare = String.compare)

    fun say l = TextIO.output (TextIO.stdErr, concat l)
    fun warn l = say ("WARNING: " :: l)
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

    fun rmfile f = F.remove f handle _ => ()

    (* several worklists for delayed execution *)
    val stablist : (unit -> bool) list ref = ref []
    val movlist  : (unit -> unit) list ref = ref []
    val salist : (unit -> unit) list ref = ref []

    (* make a directory (including parent, parent's parent, ...) *)
    fun mkdir "" = ()
      | mkdir d = if fexists d then () else (mkdir (P.dir d); F.mkDir d)

    (* generalized F.rename that works across different file systems *)
    fun rename { old, new } =
	let fun copy () =
		let val ins = BinIO.openIn old
		    val outs = BinIO.openOut new
		    fun loop () =
			let val v = BinIO.input ins
			in
			    if Word8Vector.length v = 0 then
				(BinIO.closeIn ins;
				 BinIO.closeOut outs)
			    else (BinIO.output (outs, v);
				  loop ())
			end
		in
		    loop ()
		end
	in
	    F.rename { old = old, new = new }
	    handle _ =>
		   (* probably on different filesys *)
		   (copy (); rmfile old)
	end

    (* move a stable library file to its final location *)
    fun movelib src dst () =
	(mkdir (P.dir dst); rename { old = src, new = dst })

    (* register a temporary anchor-value binding *)
    fun localanchor { anchor, path } =
	#set (CM.Anchor.anchor anchor) (SOME (native path))

    fun getInputTokens s =
	case TextIO.inputLine s of
	    NONE => NONE
	  | SOME "" => NONE
	  | SOME l =>
	      if String.sub (l, 0) = #"#" then getInputTokens s
	      else SOME (String.tokens Char.isSpace l)
    fun tokenLine l = String.concatWith " " l

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

    (* fold a function over the contents of a pathconfig file: *)
    fun pc_fold g m f =
	let val s = TextIO.openIn f
	    fun loop m =
		case getInputTokens s of
		    NONE => (TextIO.closeIn s; m)
		  | SOME [k, v] => loop (g (m, k, v))
		  | SOME l => (say ("funny line in " :: f :: ":" ::
				    foldr (fn (x, l) => " " :: x :: l)
					  ["\n"] l);
			       loop m)
	in loop m
	end handle _ => m	(* in case file does not exist *)

    (* build those standalone programs that require libraries
     * and, therefore, must be compiled "late"... *)
    fun dolatesas () =
	(app (fn f => f ()) (rev (!salist)); true)
	handle _ => false

    (* our main routine *)
    fun proc { smlnjroot, installdir, buildcmd, instcmd, unpack } = let
	val smlnjroot = F.fullPath smlnjroot
	val installdir = F.fullPath installdir
        val libdir = P.concat (installdir, "lib")
	val configdir = P.concat (smlnjroot, "config")
	val bindir = P.concat (installdir, "bin")
	val heapdir = P.concat (bindir, ".heap")
	val cm_pathconfig = P.concat (libdir, "pathconfig")

	(* dependency file: config/dependencies *)
	val depfile = P.concat (configdir, "dependencies")

	(* where to get additional path configurations *)
	val extrapathconfig = P.concat (configdir, "extrapathconfig")

	(* action file: mapping from "modules" to lists of "actions" *)
	val actionfile = P.concat (configdir, "actions")

	(* add an entry to lib/pathconfig *)
	fun write_cm_pathconfig (a, p) = let
	    val s = TextIO.openAppend cm_pathconfig
	in
	    TextIO.output (s, concat [a, " ", p, "\n"])
	    before TextIO.closeOut s
	end

	(* augment anchor mapping with extra bindings: *)
	val _ =
	    pc_fold (fn ((), k, v) =>
			(#set (CM.Anchor.anchor k)
			      (SOME (P.concat (libdir, native v)));
			 write_cm_pathconfig (k, v)))
		    ()
	            extrapathconfig

	(* find and open first usable targetsfiles *)
	val targetsfiles =
	    [P.concat (configdir, "targets.customized"),
	     P.concat (configdir, "targets")]

	val allsrcfile = P.concat (configdir, "allsources")

	val s =
	    case List.find fexists targetsfiles of
		SOME f => TextIO.openIn f
	      | NONE => fail ["no targetsfiles\n"]

	(* parse the targets file *)
	fun loop (ml, allsrc) =
	    case getInputTokens s of
		NONE => (TextIO.closeIn s; (ml, allsrc))
	      | SOME [x as ("dont_move_libraries" | "move_libraries")] =>
  		  (warn ["\"", x, "\" no longer supported",
			 " (installer always moves libraries)\n"];
		   loop (ml, allsrc))
	      | SOME ["request", "src-smlnj"] => loop (ml, true)
	      | SOME ["request", module] => loop (module :: ml, allsrc)
	      | SOME [] => loop (ml, allsrc)
	      | SOME l => fail ["ill-formed targets line: ", tokenLine l, "\n"]

	val (modules, allsrc) = loop ([], false)

	(* now resolve dependencies; get full list of modules
	 * in correct build order: *)
	val modules = resolve (modules, depfile)

	val moduleset = SS.addList (SS.empty, modules)

	val srcmoduleset =
	    if allsrc andalso fexists allsrcfile then
		let val s = TextIO.openIn allsrcfile
		    fun one (m, ms) =
			if SS.member (ms, m) then ms else SS.add (ms, m)
		    fun loop ms =
			case getInputTokens s of
			    NONE => (TextIO.closeIn s; ms)
			  | SOME l => loop (foldl one ms l)
		in
		    loop moduleset
		end
	    else moduleset

	(* fetch and unpack source trees, using auxiliary helper command
	 * which takes the root directory as its first and the module
	 * names to be fetched as subsequent arguments. *)
	val _ = case unpack of
		    NONE => ()		(* archives must exist *)
		  | SOME upck =>
		      if upck (SS.listItems srcmoduleset) then ()
		      else fail ["unpacking failed\n"]


	(* at the end, read lib/pathconfig and eliminate duplicate entries *)
	fun uniqconfig () = let
	    fun swallow (f, m) = pc_fold SM.insert m f
	    fun finish m =
		let val s = TextIO.openOut cm_pathconfig
		    fun one (k, v) = TextIO.output (s, concat [k, " ", v, "\n"])
		in SM.appi one m; TextIO.closeOut s
		end
	in finish (pc_fold SM.insert SM.empty cm_pathconfig)
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
	     *   name is relative to smlnjroot and in standard syntax *)
	    val nrelname = native relname
	    val ndir = native dir
	    val libname = concat ["$", anchor, "/", relname]
	    val adir = P.concat (smlnjroot, ndir)
	    val finalanchor = getOpt (altanchor, anchor)
	    val { dir = nreldir, file = relbase } = P.splitDirFile nrelname
	    val relloc =
		P.concat (nreldir, P.concat (CM.cm_dir_arc,
					     P.concat (arch_oskind, relbase)))
	    val srcfinalloc = P.concat (adir, relloc)
	    val (finalloc, finalconfigpath) =
		(P.concat (libdir,
			       P.concat (finalanchor, relloc)),
		     finalanchor)
	in
	    if fexists finalloc then
		(say ["Library ", libname, " already existed in ",
		      finalloc, ".  Will rebuild.\n"];
		 rmfile finalloc)
	    else ();
	    if fexists srcfinalloc then	rmfile srcfinalloc else ();
	    if not (fexists (P.concat (adir, nrelname))) then
		fail ["Source tree for ", libname, " at ",
		      P.concat (adir, nreldir), "(", relbase,
		      ") does not exist.\n"]
	    else
		(say ["Scheduling library ", libname, " to be built as ",
		      finalloc, "\n"];
		 stablist := (fn () => CM.stabilize false libname)
			     :: !stablist;
		 #set (CM.Anchor.anchor anchor) (SOME adir);
		 movlist := movelib srcfinalloc finalloc :: !movlist;
		 write_cm_pathconfig (finalanchor, finalconfigpath))
	end

	fun command_pathconfig target =
	    write_cm_pathconfig (target, P.concat (P.parentArc, "bin"))

	(* build a standalone program, using auxiliary build script *)
	fun standalone { target, optheapdir, dir } = let
	    (* target: name of program; this is the same as the basename
	     *   of the heap image to be generated as well as the
	     *   final arc of the source tree's directory name
	     * optheapdir: optional subdirectory where the build command
	     *   drops the heap image
	     * dir:
	     *   The source tree for the target, relative to smlnjroot. *)
	    val heapname = concat [target, ".", heap_suffix]
	    val targetheaploc =
		case optheapdir of
		    NONE => heapname
		  | SOME hd => P.concat (native hd, heapname)
	    val treedir = P.concat (smlnjroot, native dir)
	    val finalheaploc = P.concat (heapdir, heapname)
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
			 (rename { old = targetheaploc,
				   new = finalheaploc };
			  instcmd target;
			  #set (CM.Anchor.anchor target) (SOME bindir))
		     else
			 fail ["Built ", target, "; ", heapname,
			       " still missing.\n"]
		 else
		     fail ["Building ", target, " failed.\n"];
		 command_pathconfig target;
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
	    standalone { target = t, optheapdir = d, dir = "src" }

	fun sa' (t, d, s) =
	    standalone { target = t, optheapdir = d, dir = s }

	(* ------------------------------ *)

	datatype action =
	    RegLib of { anchor: string, relname: string, dir: string,
			altanchor: string option }
		      * bool (* true = only on Unix *)
	  | Anchor of { anchor: string, path: string }
		      * bool (* true = relative to libdir *)
	  | Program of { target: string, optheapdir: string option,
			 dir: string }
		       * bool	(* true = defer *)

	val actions =
	    let val s = TextIO.openIn actionfile
		fun opthd "-" = NONE
		  | opthd h = SOME h
		fun progargs (mn, []) =
		      { target = mn, optheapdir = NONE, dir = mn }
		  | progargs (mn, [t]) =
		      { target = t, optheapdir = NONE, dir = mn }
		  | progargs (mn, [t, h]) =
		      { target = t, optheapdir = opthd h, dir = mn }
		  | progargs (mn, t :: h :: d :: _) =
		      { target = t, optheapdir = opthd h, dir = d }
		fun libargs (a, r, d, aa) =
		      { anchor = a, relname = r, dir = d, altanchor = aa }
		fun loop m =
		    case getInputTokens s of
			NONE => m
		      | SOME [mn, "lib", a, r, d] =>
			  ins (m, mn, RegLib (libargs (a, r, d, NONE), false))
		      | SOME [mn, "lib", a, r, d, aa] =>
			  ins (m, mn, RegLib (libargs (a, r, d, SOME aa), false))
		      | SOME [mn, "ulib", a, r, d] =>
			  ins (m, mn, RegLib (libargs (a, r, d, NONE), true))
		      | SOME [mn, "ulib", a, r, d, aa] =>
			  ins (m, mn, RegLib (libargs (a, r, d, SOME aa), true))
		      | SOME [mn, "anchor", a, p] =>
			  ins (m, mn, Anchor ({ anchor = a, path = p }, false))
		      | SOME [mn, "libanchor", a, p] =>
			  ins (m, mn, Anchor ({ anchor = a, path = p }, true))
		      | SOME (mn :: "prog" :: args) =>
			  ins (m, mn, Program (progargs (mn, args), false))
		      | SOME (mn :: "dprog" :: args) =>
			  ins (m, mn, Program (progargs (mn, args), true))
		      | SOME [] => loop m
		      | SOME other =>
			  fail ["Illegal line in ", actionfile, ": ",
				String.concatWith " " other, "\n"]
		and ins (m, mn, a) =
		    loop (SM.insert (m, mn, a :: getOpt (SM.find (m, mn), [])))
	    in loop SM.empty
	       before TextIO.closeIn s
	    end

	(* ------------------------------ *)

	fun one module =
	    let fun perform (RegLib (args, justunix)) =
		      if not justunix orelse isUnix then reglib args else ()
		  | perform (Anchor ({ anchor, path }, false)) =
		      #set (CM.Anchor.anchor anchor) (SOME (native path))
		  | perform (Anchor ({ anchor, path }, true)) =
		      #set (CM.Anchor.anchor anchor)
		           (SOME (P.concat (libdir, path)))
		  | perform (Program (args, false)) =
		      standalone args
		  | perform (Program (args, true)) =
		      salist := (fn () => standalone args) :: (!salist)
	    in case SM.find (actions, module) of
		   SOME al => app perform (rev al)
		 | NONE => fail ["unknown module: ", module, "\n"]
	    end

	(* ------------------------------ *)

(*
	(* process one module *)
	fun one "smlnj-lib" =
	    (if isUnix then
		 r ("unix-lib.cm", "unix-lib.cm", "src/smlnj-lib/Unix")
	     else ();
	     r ("inet-lib.cm", "inet-lib.cm", "src/smlnj-lib/INet");
	     r ("regexp-lib.cm", "regexp-lib.cm", "src/smlnj-lib/RegExp");
	     r ("reactive-lib.cm", "reactive-lib.cm", "src/smlnj-lib/Reactive");
	     r ("hash-cons-lib.cm", "hash-cons-lib.cm", "src/smlnj-lib/HashCons"))
	  | one "cml" =
	    (r ("cml", "core-cml.cm", "src/cml/src");
	     r ("cml", "cml-internal.cm", "src/cml/src");
	     r ("cml", "cml.cm", "src/cml/src");
	     r ("cml", "basis.cm", "src/cml/src"))
	  | one "cml-lib" =
	    (r ("cml-lib", "trace-cml.cm", "src/cml/cml-lib/cm-descr");
	     r ("cml-lib", "smlnj-lib.cm", "src/cml/cml-lib/cm-descr"))
	  | one "eXene" =
	    (r ("eXene.cm", "eXene.cm", "src/eXene"))
	  | one "ckit" =
	    (r ("ckit-lib.cm", "ckit-lib.cm", "ckit/src"))
	  | one "ml-nlffi-lib" =
	    (r ("c", "memory/memory.cm", "src/ml-nlffi-lib");
	     r ("c", "internals/c-int.cm", "src/ml-nlffi-lib");
	     r ("c", "c.cm", "src/ml-nlffi-lib"))
	  | one "pgraph-util" =
	    (r ("pgraph-util.cm", "pgraph-util.cm", "src/cm/pgraph"))
	  | one "mlrisc" =
	    (a ("Control.cm", P.concat (libdir, "SMLNJ-MLRISC"));
	     a ("Lib.cm", P.concat (libdir, "SMLNJ-MLRISC"));
	     a ("Visual.cm", P.concat (libdir, "SMLNJ-MLRISC"));
	     a ("MLRISC.cm", P.concat (libdir, "SMLNJ-MLRISC"));
	     a ("MLTREE.cm", P.concat (libdir, "SMLNJ-MLRISC"));
	     a ("Graphs.cm", P.concat (libdir, "SMLNJ-MLRISC"));
	     a ("IA32.cm", P.concat (libdir, "SMLNJ-MLRISC"));
	     a ("Peephole.cm", "src/MLRISC/cm");
	     r' ("OTHER-MLRISC", "RA.cm", "src/MLRISC/cm", "SMLNJ-MLRISC");
	     r' ("OTHER-MLRISC", "Peephole.cm", "src/MLRISC/cm", "SMLNJ-MLRISC");
	     r' ("OTHER-MLRISC", "IA32-Peephole.cm", "src/MLRISC/cm", "SMLNJ-MLRISC"))
	  | one "mlrisc-tools" =
	    (r ("mlrisc-tools", "pp.cm", "src/MLRISC/Tools");
	     r ("mlrisc-tools", "source-map.cm", "src/MLRISC/Tools");
	     r ("mlrisc-tools", "sml-ast.cm", "src/MLRISC/Tools");
	     r ("mlrisc-tools", "prec-parser.cm", "src/MLRISC/Tools");
	     r ("mlrisc-tools", "parser.cm", "src/MLRISC/Tools");
	     r ("mlrisc-tools", "match-compiler.cm", "src/MLRISC/Tools"))
	  | one "ml-yacc" =
	      sa ("ml-yacc", SOME "src")
	  | one "ml-lex" =
	      sa ("ml-lex", NONE)
	  | one "lexgen" =
	      sa ("lexgen", SOME "src")
	  | one "ml-burg" =
	      sa ("ml-burg", NONE)
	  | one "heap2asm" =
	      sa ("heap2asm", NONE)
	  | one "ml-nlffigen" =
	      salist := (fn () => sa ("ml-nlffigen", NONE))
			:: !salist
	  | one "nowhere" =
	      salist := (fn () => sa' ("nowhere", NONE, "src/MLRISC/Tools"))
			:: !salist
	  | one module = fail ["unknown module: ", module, "\n"]
*)
    in
	(command_pathconfig "bindir";	(* dummy -- for CM make tool *)
	 app one modules;
	 if dostabs () andalso domoves () andalso dolatesas () then
	     uniqconfig ()
	 else fail ["stabilization failed\n"])
	handle e => fail ["unexpected exception: ",
			  General.exnMessage e, "\n"];
	OS.Process.exit OS.Process.success
    end
end
