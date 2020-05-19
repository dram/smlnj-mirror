(* extract-index.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This module implements a tree walker that extracts index information
 * from the documentation sources.
 *)

structure ExtractIndex : sig

  (* given the root directory of the documentation sources, extract
   * index information.
   *)
    val extract : string -> FileTree.t

  end = struct

    structure FT = FileTree
    structure P = OS.Path
    structure RE = RegExpFn(
	structure P = AwkSyntax
	structure E = BackTrackEngine)
    structure MT = MatchTree
    structure SS = Substring

    val includeRE = RE.compileString "^include::([^.]+\\.adoc)\\[\\]"
    val pageRefRE =
	  RE.compileString
	    "[ ]*xref:([^.]+\\.adoc)\\[`\\[\\.kw\\]#([a-z]+)# ([^`]+)`\\]::"

    fun match re = let
	  val prefix = StringCvt.scanString (RE.prefix re)
	  fun getSubstrs s = MT.map (fn {pos, len} => String.substring(s, pos, len))
	  in
	    fn s => Option.map (getSubstrs s) (prefix s)
	  end

    fun openIn (rootDir, path) = TextIO.openIn (P.concat(rootDir, path))

    fun trimWS ss = SS.dropr Char.isSpace (SS.dropl Char.isSpace ss)

  (* open a documentation file and extract some common information *)
    fun openFile rootDir path = let
	  val dir = P.dir path
	  val stem = P.base(P.file path)
	  val inS = openIn (rootDir, path)
	  val SOME firstLn = TextIO.inputLine inS
	  val title = if String.isPrefix "= " firstLn
	        then SS.string(trimWS(SS.extract(firstLn, 2, NONE)))
		else "<title>"
	  in {
	    dir = dir,
	    stem = stem,
	    title = title,
	    inS = inS
	  } end

  (* scan the input stream until a line for which f returns `SOME v` is encountered *)
    fun scanLines f inS = let
	  fun lp () = (case TextIO.inputLine inS
		 of SOME ln => (case f ln
		       of NONE => lp ()
			| someV => someV
		      (* end case *))
		  | NONE => NONE
		(* end case *))
	  in
	    lp ()
	  end

  (* find the next "include" directive in the input stream *)
    fun findInclude inS = scanLines (match includeRE) inS

  (* find the next page reference *)
    fun findPageRef inS = scanLines (match pageRefRE) inS

    fun doPage rootDir libDir {file, kw, name} = let
	  val pagePath = P.joinDirFile{dir = libDir, file = file}
	  val {dir, stem, title, inS} = openFile rootDir pagePath
	  in
	    FT.PAGE{
		dir = dir,
		stem = stem,
		title = title,
		kind = kw,
		name = name
	      }
	    before TextIO.closeIn inS
	  end

  (* extract the list of manual-page files from a library document *)
    fun getPagesFromLib inS = let
	  fun getPages pages = (case findPageRef inS
		 of SOME(MT.Match(_, [
		      MT.Match(file, []), MT.Match(kw, []), MT.Match(name, [])
		    ])) => let
		      val page = {file = file, kw = kw, name = name}
		      in
			getPages (page::pages)
		      end
		  | NONE => List.rev pages
		  | SOME(MT.Match(s, _)) => raise Fail(concat[
			"**bogus page ref \"", String.toString s, "\""
		      ])
		(* end case *))
	  in
	    getPages [] before TextIO.closeIn inS
	  end

    fun doLib rootDir libPath = let
	  val {dir, stem, title, inS} = openFile rootDir libPath
	  in
	    FT.LIB{
		dir = dir,
		stem = stem,
		title = title,
		tutorial = NONE,	(* FIXME *)
		pages = List.map (doPage rootDir dir) (getPagesFromLib inS)
	      }
	    before TextIO.closeIn inS
	  end

  (* extract the list of library files from the root document *)
    fun getLibsFromRoot inS = let
	  fun getIncludes incs = (case findInclude inS
		 of SOME(MT.Match(_, [MT.Match(path, [])])) =>
		      getIncludes(path :: incs)
		  | NONE => List.rev incs
		  | SOME(MT.Match(s, _)) => raise Fail(concat[
			"**bogus include \"", String.toString s, "\""
		      ])
		(* end case *))
	  in
	    getIncludes [] before TextIO.closeIn inS
	  end

    fun extract rootDir = let
	  val rootDir = OS.FileSys.fullPath rootDir
	  val inS = openIn (rootDir, "index.adoc")
	  val libs = getLibsFromRoot inS
	  in
	    FT.ROOT{stem = "index", libs = List.map (doLib rootDir) libs}
	      before TextIO.closeIn inS
	  end

  end
