(*
 * Operations over abstract path names.
 *
 * Copyright (c) 1999 by Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@cs.princeton.edu)
 *)
signature ABSPATH = sig

    exception Format	    (* if something is seriously wrong with a pickle *)
    exception BadAnchor of string	(* if anchor cannot be resolved *)

    type context
    type t
    type ord_key = t

    val newEra : unit -> unit
    val revalidateCwd : unit -> unit

    val cwdContext: unit -> context
    val sameDirContext: t -> context

    val osstring : t -> string
    val descr : t -> string
    val compare : t * t -> order
    val contextOf : t -> context
    val specOf : t -> string
    val contextName : context -> string

    (* Replace the anchor context in the path argument with the
     * given context. Returns NONE if there was no anchor context. *)
    val reAnchoredName : t * string -> string option

    val native : { context: context, spec: string } -> t
    val standard : PathConfig.mode -> { context: context, spec: string } -> t

    val fromDescr : PathConfig.mode -> string -> t

    (* the second path argument is the path of the group spec that
     * pickling is based upon. *)
    val pickle : (bool -> unit) -> t * t -> string list
    val unpickle : PathConfig.mode -> string list * t -> t

    val tstamp : t -> TStamp.t
end

structure AbsPath :> ABSPATH = struct

    structure P = OS.Path
    structure F = OS.FileSys
    val impossible = GenericVC.ErrorMsg.impossible

    exception Format
    exception BadAnchor of string

    (* unique file id that can handle absent files *)
    datatype id =
	PRESENT of F.file_id
      | ABSENT of string

    (* comparison of unique file ids *)
    fun compareId (PRESENT fid, PRESENT fid') = F.compare (fid, fid')
      | compareId (ABSENT _, PRESENT _) = LESS
      | compareId (PRESENT _, ABSENT _) = GREATER
      | compareId (ABSENT s, ABSENT s') = String.compare (s, s')

    (* To maximize our chances of recognizing eqivalent path names to
     * non-existing files, we use F.fullPath to expand the largest
     * possible prefix of the path. *)
    fun expandPath f = let
	fun loop { dir, file } =
	    P.concat (F.fullPath dir, file)
	    handle _ => let
		val { dir = dir', file = file' } = P.splitDirFile dir
	    in
		loop { dir = dir', file = P.concat (file', file) }
	    end
    in
	(* An initial call to splitDirFile is ok because we already know
	 * that the complete path does not refer to an existing file. *)
	loop (P.splitDirFile f)
    end

    fun getId f = (PRESENT (F.fileId f) handle _ => ABSENT (expandPath f))

    type elaboration = { stamp : unit ref,
			 name : string,
			 id : id option ref }

    (* When a relative name is to be looked up wrt. CUR:
     *  - if the cwd hasn't changed since, then use relative path
     *  - if the cwd has changed, then make absolute path using name
     * If we come back to the original dir, then ideally we should
     * re-validate the stamp, but that would require having a cwd
     * history -- and, thus, is probably not worth the effort.
     *)

    type cwdinfo = { stamp: unit ref, name: string, id: id }

    datatype context =
	THEN_CWD of cwdinfo
      | CONFIG_ANCHOR of { fetch: unit -> string,
			   cache: elaboration option ref,
			   config_name: string }
      | DIR_OF of t
      | ROOT of string			(* carries volume *)

    and t =
	PATH of { context: context,
		  spec: string,
		  cache: elaboration option ref }

    type ord_key = t

    local
	val cwdInfoCache : cwdinfo option ref = ref NONE
	fun cwdInfo () =
	    case !cwdInfoCache of
		SOME i => i
	      | NONE => let
		    val stamp = ref ()
		    val name = F.getDir ()
		    val id = PRESENT (F.fileId name)
		    val i = { stamp = stamp, name = name, id = id }
		in
		    cwdInfoCache := SOME i;
		    i
		end
	val cwdStamp = #stamp o cwdInfo
	val cwdName = #name o cwdInfo
	val cwdId = #id o cwdInfo
    in
	(* start a new era (i.e., invalidate all previous elaborations) *)
	val newEra = Era.newEra

	(* make sure the cwd is consistent *)
	fun revalidateCwd () =
	    case !cwdInfoCache of
		NONE => ignore (cwdInfo ())
	      | SOME { name, id, ... } => let
		    val name' = F.getDir ()
		    val id' = PRESENT (F.fileId name')
		in
		    if compareId (id, id') <> EQUAL then
			(newEra ();
			 cwdInfoCache := SOME { stamp = ref (),
					        name = name', id = id' })
		    else ()
		end

	fun cwdContext () =
	    (revalidateCwd ();
	     THEN_CWD { stamp = cwdStamp (),
		        name = cwdName (),
			id = cwdId () })

	fun sameDirContext p = DIR_OF p

	fun mkElab (cache, name) = let
	    val e : elaboration =
		{ stamp = Era.thisEra (), name = name, id = ref NONE }
	in
	    cache := SOME e; e
	end

	fun validElab NONE = NONE
	  | validElab (SOME (e as { stamp, name, id })) =
	    if Era.isThisEra stamp then SOME e else NONE

	fun rootName vol = P.toString { isAbs = true, arcs = [], vol = vol }
	val rootId = let
	    val m = ref (StringMap.empty: id option ref StringMap.map)
	in
	    fn vol =>
	    (case StringMap.find (!m, vol) of
		 NONE => let
		     val idr = ref NONE
		 in
		     m := StringMap.insert (!m, vol, idr);
		     idr
		 end
	       | SOME idr => idr)
	end

	fun elabContext c =
	    case c of
		THEN_CWD { stamp, name, id } =>
		    { stamp = Era.thisEra (), id = ref (SOME id),
		      name = if stamp = cwdStamp () orelse name = cwdName ()
			     then P.currentArc else name }
	      | CONFIG_ANCHOR { fetch, cache, config_name } =>
		    (case validElab (!cache) of
			 SOME e => e
		       | NONE => mkElab (cache, fetch ()))
	      | DIR_OF p => let
		    val { name, stamp, ... } = elab p
		in 
		    { name = P.dir name, stamp = stamp, id = ref NONE }
		end
	      | ROOT vol => { stamp = Era.thisEra (),
			      name = rootName vol,
			      id = rootId vol }

	and elab (PATH { context, spec, cache }) =
	    case validElab (!cache) of
		SOME e => e
	      | NONE => mkElab (cache,
				P.concat (#name (elabContext context), spec))

	(* get the file id (calls elab, so don't cache externally!) *)
	fun id p = let
	    val { id, name, ... } = elab p
	in
	    case !id of
		NONE => let
		    val i = getId name
		in
		    id := SOME i; i
		end
	      | SOME i => i
	end

	(* get the name as a string (calls elab, so don't cache externally!) *)
	fun osstring p = #name (elab p)

	(* get the context back *)
	fun contextOf (PATH { context = c, ... }) = c
	fun contextName c = #name (elabContext c)

	(* get the spec back *)
	fun specOf (PATH { spec = s, ... }) = s

	(* compare pathnames efficiently *)
	fun compare (p1, p2) = compareId (id p1, id p2)

	fun fresh (context, spec) =
	    PATH { context = context, spec = spec, cache = ref NONE }

	(* make an abstract path from a native string *)
	fun native { spec, context } = let
	    val { isAbs, vol, arcs } = P.fromString spec
	    val relSpec = P.toString { isAbs = false, vol = "", arcs = arcs }
	in
	    if isAbs then fresh (ROOT vol, relSpec)
	    else fresh (context, relSpec)
	end

	(* make an abstract path from a standard string *)
	fun standard mode { spec, context } = let
	    fun delim #"/" = true
	      | delim #"\\" = true		(* accept DOS-style, too *)
	      | delim _ = false

	    fun transl ".." = P.parentArc
	      | transl "." = P.currentArc
	      | transl arc = arc

	    fun mk (arcs, context) =
		fresh (context,
		       P.toString { isAbs = false, vol = "",
				    arcs = map transl arcs })
	in
	    case String.fields delim spec of
		[""] => impossible "AbsPath.standard: zero-length name"
	      | "" :: arcs => mk (arcs, ROOT "")
	      | [] => impossible "AbsPath.standard: no fields"
	      | arcs as (arc1 :: _) =>
		    (case PathConfig.configAnchor mode arc1 of
			 NONE => mk (arcs, context)
		       | SOME fetch => let
			     val anchorcontext =
				 CONFIG_ANCHOR { fetch = fetch,
						 cache = ref NONE,
						 config_name = arc1 }
			 in
			     mk (arcs, anchorcontext)
			 end)
	end

	(* make a pickle-string *)
	fun pickle warn (path, gpath) = let
	    fun p_p (PATH { spec, context, ... }) =
		spec :: p_c context
	    and p_c (ROOT vol) = (warn true; [vol, "r"])
	      | p_c (THEN_CWD _) = impossible "AbsPath.pickle: THEN_CWD"
	      | p_c (CONFIG_ANCHOR { config_name = n, ... }) = [n, "a"]
	      | p_c (DIR_OF p) =
		if compare (p, gpath) = EQUAL then (warn false; ["c"])
		else p_p p
	in
	    p_p path
	end

	fun unpickle mode (l, gpath) = let
	    fun u_p (s :: l) =
		PATH { spec = s, context = u_c l, cache = ref NONE }
	      | u_p [] = raise Format
	    and u_c [vol, "r"] = ROOT vol
	      | u_c ["c"] = DIR_OF gpath
	      | u_c [n, "a"] =
		(case PathConfig.configAnchor mode n of
		     NONE => raise BadAnchor n
		   | SOME fetch =>
			 CONFIG_ANCHOR { config_name = n,
					 fetch = fetch,
					 cache = ref NONE })
	      | u_c l = DIR_OF (u_p l)
	in
	    u_p l
	end

	fun tstamp p = TStamp.fmodTime (osstring p)

	fun descr (PATH { spec, context, ... }) = let
	    fun dir (x, l) =
		case P.dir x of
		    "" => l
		  | d => d :: "/" :: l
	    fun d_c (CONFIG_ANCHOR { config_name = n, ... }, l) =
		"$" :: n :: "/" :: l
	      | d_c (DIR_OF (PATH { spec, context, ... }), l) =
		d_c (context, dir (spec, l))
	      | d_c (THEN_CWD _, l) = "./" :: l
	      | d_c (ROOT "", l) = "/" :: l
	      | d_c (ROOT vol, l) = "%" :: vol :: "/" :: l
	in
	    concat (d_c (context, [spec]))
	end

	fun fromDescr mode "" = fresh (cwdContext (), P.currentArc)
	  | fromDescr mode d = let
		val l = size d
		fun split n =
		    if n >= l then
			(String.substring (d, 1, l), P.currentArc)
		    else if String.sub (d, n) = #"/" then
			(String.substring (d, 1, n - 1),
			 String.extract (d, n + 1, NONE))
		    else split (n + 1)
	    in
		case String.sub (d, 0) of
		    #"$" => let
			val (a, s) = split 1
		    in
			case PathConfig.configAnchor mode a of
			    NONE => raise BadAnchor a
			  | SOME fetch =>
				fresh (CONFIG_ANCHOR { config_name = a,
						       fetch = fetch,
						       cache = ref NONE },
				       s)
		    end
		  | #"/" => fresh (ROOT "", String.extract (d, 1, NONE))
		  | #"." => fresh (cwdContext (), String.extract (d, 2, NONE))
		  | #"%" => let
			val (v, s) = split 1
		    in
			fresh (ROOT v, s)
		    end
		  | _ => fresh (cwdContext (), d)
	    end

	fun reAnchoredName (p, dirstring) = let
	    fun path (PATH { context, spec, ... }) = let
		fun mk c = P.concat (c, spec)
	    in
		Option.map mk (ctxt context)
	    end
	    and ctxt (CONFIG_ANCHOR { config_name = n, ... }) =
		SOME (P.concat (dirstring, n))
	      | ctxt (DIR_OF p) = Option.map P.dir (path p)
	      | ctxt (THEN_CWD _) = NONE
	      | ctxt (ROOT _) = NONE
	in
	    path p
	end
    end
end
