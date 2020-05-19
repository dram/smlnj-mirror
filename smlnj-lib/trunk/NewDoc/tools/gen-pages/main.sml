(* main.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure Main : sig

    val main : string * string list -> OS.Process.status

  end = struct

    structure FT = FileTree
    structure P = OS.Path

  (* create a copy-file function with the specified substitutions *)
    fun copy {author, title, file, base} = let
	  val d = Date.fromTimeUniv(OS.FileSys.modTime file)
	  val date = Date.fmt "%Y-%m-%d" d
	  val time = Date.fmt "%H:%M:%S UTC" d
	  in
	    CopyFile.copy [
		("AUTHOR", author),
		("TITLE", Util.clean title),
		("STYLED-TITLE", Util.style title),
		("DATE", !Options.releaseDate),
		("VERSION", !Options.version),
		("FILEDATE", date),
		("FILETIME", time),
		("BASE", base)
	      ]
	  end

    fun gen {dir, stem, title, genTOC} = let
	  val srcFile = P.concat(dir, stem ^ ".adoc")
	  val htmlFile = P.concat(dir, stem ^ ".html")
	  val copy = copy {
		  author = "??",
		  title = title,
		  file = srcFile,
		  base = if dir = "" then "" else "../"
		}
	  val outS = TextIO.openOut htmlFile
	  in
	    if !Options.verbose
	      then print(concat["generating ", htmlFile, "\n"])
	      else ();
	    copy (P.concat(Config.fragDir, "header.in"), outS);
	    genTOC outS;
	    copy (P.concat(Config.fragDir, "shim.in"), outS);
	    RunAsciidoctor.run (srcFile, outS);
	    copy (P.concat(Config.fragDir, "footer.in"), outS);
	    TextIO.closeOut outS
	  end

  (* generate the root page *)
    fun appRoot (ft as FT.ROOT{stem, ...}) = gen {
	    dir = "",
	    stem = stem,
	    title = "Overview",
	    genTOC = GenTOC.root ft
	  }

  (* generate a library page *)
    fun appLib (ft, lib as FT.LIB{dir, stem, title, ...}) = gen {
	    dir = dir,
	    stem = stem,
	    title = title,
	    genTOC = GenTOC.lib (ft, lib)
	  }

  (* generate a TOC file for a manual page *)
    fun appPage (ft, lib, page as FT.PAGE{dir, stem, title, ...}) = gen {
	    dir = dir,
	    stem = stem,
	    title = title,
	    genTOC = GenTOC.page (ft, lib, page)
	  }

    fun loadIndex indexFile = FT.fromJSON (JSONParser.parseFile indexFile)

  (* for every documentation page, generate a table of contents file *)
    val walkTree = FT.app {root = appRoot, lib = appLib, page = appPage}

    fun main (cmd, opts) = (
	  Options.process opts;
	  walkTree (loadIndex (!Options.indexFile));
	  OS.Process.success)
	    handle ex => (
	      TextIO.output(TextIO.stdErr, concat[
		  "uncaught exception ", General.exnName ex,
		  " [", General.exnMessage ex, "]\n"
		]);
	      List.app
		(fn s => TextIO.output(TextIO.stdErr, concat ["  raised at ", s, "\n"]))
		  (SMLofNJ.exnHistory ex);
              OS.Process.failure)

  end
