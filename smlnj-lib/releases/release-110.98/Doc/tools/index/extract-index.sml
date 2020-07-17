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
    structure SIO = TextIO.StreamIO

    val attrRE = RE.compileString "^:!?([^!:]+)!?:(.*)"
    val includeRE = RE.compileString "^include::([^.]+\\.adoc)\\[\\]"
    val xrefRE = RE.compileString
	  "[ ]*xref:([^.]+\\.adoc)\\[([^\\]]+|`\\[\\.kw\\]#[a-z]+# [^`]+`)\\]::"
  (* match the title text for a module xref *)
    val pageRefRE = RE.compileString "`\\[\\.kw\\]#([a-z]+)# ([^`]+)`"

    fun match re = let
	  val prefix = StringCvt.scanString (RE.prefix re)
	  fun getSubstrs s = MT.map (fn {pos, len} => String.substring(s, pos, len))
	  in
	    fn s => Option.map (getSubstrs s) (prefix s)
	  end

    fun openIn (rootDir, path) = TextIO.openIn (P.concat(rootDir, path))

    fun trimWS ss = SS.dropr Char.isSpace (SS.dropl Char.isSpace ss)

  (* match an asciidoctor atrribute *)
    val matchAttr = match attrRE

  (* extract attribute values from lines immediately following the title *)
    fun scanMeta inputStrm = let
	  val inS' = TextIO.getInstream inputStrm
	  val author = ref NONE
	  val keywords = ref []
	  val title = ref NONE
	  fun trim s = SS.string(trimWS(SS.full s))
	  fun scan inS = (case SIO.inputLine inS
		 of SOME(ln, inS') => (case matchAttr ln
		       of SOME(MT.Match(_, [MT.Match(a, []), MT.Match(v, [])])) => (
			    case String.map Char.toLower a
			     of "author" => author := SOME(trim v)
			      | "keywords" => keywords :=
				  List.map trim
				    (String.tokens (fn #"," => true | _ => false) v)
			      | "title" => title := SOME(trim v)
			      | _ => ()
			    (* end case *);
			    scan inS')
			| _ => inS
		      (* end case *))
		  | NONE => inS
		(* end case *))
	  in
	    TextIO.setInstream (inputStrm, scan (TextIO.getInstream inputStrm));
	    { author = !author, kws = !keywords, title = !title }
	  end

  (* open a documentation file and extract some common information *)
    fun scanFile rootDir path getContents processContent = let
	  val dir = P.dir path
	  val stem = P.base(P.file path)
	  val inS = openIn (rootDir, path)
	  val SOME firstLn = TextIO.inputLine inS
	  val title = if String.isPrefix "= " firstLn
	        then SS.string(trimWS(SS.extract(firstLn, 2, NONE)))
		else "<title>"
	  val meta = scanMeta inS
	  val contents = getContents inS
	  in
	    TextIO.closeIn inS;
	    FT.FILE{
		dir = dir,
		stem = stem,
		title = title,
		meta = meta,
		info = processContent contents
	      }
	  end

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

  (* find the next "xref" directive in the input stream *)
    fun findXRef inS = scanLines (match xrefRE) inS

  (* match a module page reference *)
    val matchPageRef = match pageRefRE

    fun doPage rootDir libDir {file, info} = let
	  val pagePath = P.joinDirFile{dir = libDir, file = file}
	  in
(* NOTE: for now, we only extract the header info from pages, but eventually
 * we should get the list of defined modules.
 *)
	    scanFile rootDir pagePath
	      (fn inS => info)
		(fn info => info)
	  end

  (* extract the list of page files from a library document *)
    fun getPagesFromLib inS = let
	(* first we get the `xref` list items *)
	  fun getPages pages = (case findXRef inS
		 of SOME(MT.Match(_, [MT.Match(file, []), MT.Match(title, [])])) => (
		      case matchPageRef title
		       of SOME(MT.Match(_, [MT.Match(kw, []), MT.Match(name, [])])) => let
			    val kind = (case kw
				   of "signature" => FT.SigPage
				    | "structure" => FT.StructPage
				    | "functor" => FT.FunctPage
				    | _ => raise Fail(concat[
					  "**bogus keyword \"", kw, "\""
					])
				  (* end case *))
			    val page = {
				    file = file,
				    info = {kind = kind, name = name}
				  }
			    in
			      getPages (page :: pages)
			    end
			| _ => let (* non-module page *)
			    val page = {
				    file = file,
				    info = {kind = FT.OtherPage, name = title}
				  }
			    in
			      getPages (page :: pages)
			    end
		      (* end case *))
		  | NONE => List.rev pages
		  | SOME(MT.Match(s, _)) => raise Fail(concat[
			"**bogus xref \"", String.toString s, "\""
		      ])
		(* end case *))
	  in
	    {pages = getPages []}
	  end

  (* process a library file *)
    fun doLib rootDir libPath = let
	  val libDir = P.dir libPath
	  in
	    scanFile rootDir libPath
	      getPagesFromLib
		(fn {pages} => {pages = List.map (doPage rootDir libDir) pages})
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
	    getIncludes []
	  end

    fun extract rootDir = let
	  val rootDir = OS.FileSys.fullPath rootDir
	  in
	    scanFile rootDir "index.adoc"
	      getLibsFromRoot
		(fn libs => {libs = List.map (doLib rootDir) libs})
	  end

  end
