(*
 * util/abspath.sml: Operations over abstract path names.
 *
 *   Copyright (c) 1997 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
structure AbsPath:> ABSPATH = struct

    structure P = OS.Path
    structure F = OS.FileSys

    type idstamp = unit ref
	
    val curstamp: idstamp ref = ref (ref ())
    fun reset () = curstamp := ref ()

    datatype id =
	PRESENT of F.file_id
      | ABSENT of string

    fun compareId (PRESENT fid, PRESENT fid') = F.compare (fid, fid')
      | compareId (ABSENT _, PRESENT _) = LESS
      | compareId (PRESENT _, ABSENT _) = GREATER
      | compareId (ABSENT s, ABSENT s') = String.compare (s, s')

    datatype t =
	C				(* cwd *)
      | T of {
	      context: t,
	      spec: string,		(* as specified *)
	      elab: string option ref,	(* lazy context + spec *)
	      id: (id * idstamp) option ref, (* lazy unique file id *)
	      rigid: bool		(* not the result of a search *)
	     }

    val cur = C
    val dummy = T { context = C, spec = "<error:nofile>",
		    elab = ref NONE, id = ref NONE,
		    rigid = true }

    (* cwd caching *)
    val cwd_cache: (string * F.file_id) option ref = ref NONE

    (* query current wd and load cache if necessary *)
    fun cwd'n'id () =
	case !cwd_cache of
	    SOME x => x
	  | NONE => let
		val s = F.getDir ()
		val id = F.fileId s
		val x = (s, id)
	    in
		cwd_cache := SOME x;
		x
	    end

    val cwd = #1 o cwd'n'id
    val cwdid = #2 o cwd'n'id

    (* verify cache contents; tell whether content has changed *)
    fun newcwd () = let
	val s' = F.getDir ()
	val id' = F.fileId s'
	val x' = (s', id')
    in
	reset ();
	case !cwd_cache of
	    SOME x =>
		if x = x' then false else (cwd_cache := SOME x'; true)
	  | NONE => (cwd_cache := SOME x'; true)
    end

    (* pathname as specified (disregarding context) *)
    fun spec (T { spec, rigid, context, ... }) =
	{ name = spec, rigid = rigid, context = context }
      | spec C = { name = P.currentArc, rigid = true, context = C }

    (* elaborate pathname (set spec into context) *)
    fun elab (T { elab = ref (SOME e), ... }) = e
      | elab (T { context, spec, elab = el, ... }) = let
	    val e =
		if P.isAbsolute spec then spec
		else
		    case context of
			C => spec	(* relative to CWD *)
		      | c => P.mkCanonical (P.concat (elab c, spec))
	in
	    el := SOME e;
	    e
	end
      | elab C = P.currentArc

    (* pathname `id' (for telling if two names refer to the same file) *)
    local
	fun getid (p, id) = let
	    val e = elab p
	in
	    let
		val x = PRESENT (F.fileId e)
	    in
		id := SOME (x, !curstamp); x
	    end
	    handle _ => ABSENT e
	end
    in
	fun id (p as T { id = id as ref (SOME (i, s)), ... }) =
	    if s = (!curstamp) then i else getid (p, id)
	  | id C = PRESENT (cwdid ())
	  | id (p as T { id = id as ref NONE, ... }) = getid (p, id)
    end

    (* do two paths refer to the same file *)
    fun compare (p1, p2) = compareId (id p1, id p2)

    fun bare (context, spec, rigid) =
	T { spec = spec, context = context,
	    elab = ref NONE, id = ref NONE,
	    rigid = rigid }

    (* converting from strings using native pathname syntax *)
    fun native { context, spec, rigid } =
	bare (context, P.mkCanonical spec, rigid)

    (* converting from UNIX-style pathnames (the CM `standard') *)
    fun standard { context, spec, rigid } = let
	fun delim #"/" = true
	  | delim #"\\" = true		(* accept DOS-style, too *)
	  | delim _ = false

	fun transl ".." = OS.Path.parentArc
	  | transl "." = OS.Path.currentArc
	  | transl arc = arc

	fun mk x =
	    native { context = context, spec = P.toString x, rigid = rigid }
    in
	case String.fields delim spec of
	    "" :: arcs =>
		mk { isAbs = true, vol = "", arcs = map transl arcs }
	  | arcs =>
		mk { isAbs = false, vol = "", arcs = map transl arcs }
    end

    fun current { name, rigid } =
	native { context = C, spec = name, rigid = rigid }

    fun rigidcur name = current { name = name, rigid = true }

    fun splitDirFile C = { dir = bare (C, P.parentArc, true),
			   file = P.file (cwd ()) }
      | splitDirFile (T { spec, context, rigid, ... }) = let
	val { dir, file } = P.splitDirFile spec
	val dir = if dir = "" then P.currentArc else dir
    in
	{ dir = bare (context, dir, rigid), file = file }
    end

    fun joinDirFile { dir = C, file } = bare (C, file, true)
      | joinDirFile { dir = T { spec, context, rigid, ... }, file } = let
	val j = P.mkCanonical (P.joinDirFile { dir = spec, file = file })
    in
	bare (context, j, rigid)
    end

    val dir = #dir o splitDirFile
    val file = #file o splitDirFile

    fun splitBaseExt C = { base = C, ext = NONE }
      | splitBaseExt (T { spec, context, rigid, ... }) = let
	val { base, ext } = P.splitBaseExt spec
    in
	{ base = bare (context, base, rigid), ext = ext }
    end

    fun joinBaseExt { base = C, ext = NONE } = C
      | joinBaseExt { base = C, ext = SOME e } = let
	    val f = P.file (cwd ())
	    val fe = P.joinBaseExt { base = f, ext = SOME e }
	in
	    bare (C, P.concat (P.parentArc, fe), true)
	end
      | joinBaseExt { base = T { spec, context, rigid, ... }, ext } = let
	    val j = P.joinBaseExt { base = spec, ext = ext }
	in
	    bare (context, j, rigid)
	end

    val base = #base o splitBaseExt
    val ext = #ext o splitBaseExt

    fun extendExt { path = p, ext = e, sep = dot } = let
	val { base, ext } = splitBaseExt p
    in
	case ext of
	    NONE => joinBaseExt { base = base, ext = SOME e }
	  | SOME e' => joinBaseExt { base = base,
				     ext = SOME (concat [e', dot, e]) }
    end

    val exists = File.exists o elab
    val modTime = File.modTime o elab

    val openTextIn = TextIO.openIn o elab
    val openBinIn = BinIO.openIn o elab

    local
	fun openOut fileopener (say: string -> unit) af = let
	    val f = elab af
	    fun generic (maker, pmaker, f) =
		maker f
		handle exn => let
		    val { dir, ... } = P.splitDirFile f
		in
		    if dir = "" orelse File.exists dir then raise exn
		    else (pmaker dir; maker f)
		end
	    fun makedirs dir = generic (F.mkDir, makedirs, dir)
	    fun advertisemakedirs dir =
		(say (concat ["[Creating directory ", dir, " ...]\n"]);
		 makedirs dir)
	in
	    generic (fileopener, advertisemakedirs, f)
	end
    in
	val openTextOut = openOut TextIO.openOut
	val openBinOut = openOut BinIO.openOut
    end
end
