(*
 * Parser for CM description files.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
signature PARSE = sig
    val parse : { load_plugin: SrcPath.context -> string -> bool,
		  gr: GroupReg.groupreg,
		  param: GeneralParams.param,
		  stabflag: bool option,
		  group: SrcPath.t,
		  init_group: CMSemant.group,
		  paranoid: bool }
	-> (CMSemant.group * GeneralParams.info) option
    val reset : unit -> unit
    val listLibs : unit -> SrcPath.t list
    val dropPickles : unit -> unit
    val dismissLib : SrcPath.t -> unit
end

functor ParseFn (val pending : unit -> DependencyGraph.impexp SymbolMap.map
		 val evictStale : unit -> unit
		 structure StabModmap : STAB_MODMAP
		 structure Stabilize : STABILIZE) :> PARSE = struct

    structure VerifyStable = VerStabFn (structure Stabilize = Stabilize)

    val lookAhead = 30

    structure S = GenericVC.Source
    structure EM = GenericVC.ErrorMsg
    structure SM = GenericVC.SourceMap
    structure GG = GroupGraph
    structure DG = DependencyGraph

    structure CMLrVals = CMLrValsFun (structure Token = LrParser.Token)
    structure CMLex = CMLexFun (structure Tokens = CMLrVals.Tokens)
    structure CMParse =
	JoinWithArg (structure ParserData = CMLrVals.ParserData
		     structure Lex = CMLex
		     structure LrParser = LrParser)

    (* the "stable group cache" *)
    val sgc = ref (SrcPathMap.empty: CMSemant.group SrcPathMap.map)
    fun reset () = sgc := SrcPathMap.empty

    fun majorGC () = SMLofNJ.Internals.GC.doGC 7

    fun cachedStable (p, ig as GG.GROUP { grouppath, ... }) =
	if SrcPath.compare (p, grouppath) = EQUAL then SOME ig
	else SrcPathMap.find (!sgc, p)
      | cachedStable (_, GG.ERRORGROUP) = NONE

    (* When an entry A vanishes from the stable cache (this only happens in
     * paranoid mode), then all the other ones that refer to A must
     * vasish, too.  They might still be valid themselves, but if they
     * had been unpickled before A became invalid they will point to
     * invalid data.  By removing them from the cache we force them to
     * be re-read and re-unpickled.  This restores sanity. *)
    fun delCachedStable (p, GG.GROUP { grouppath = igp, ... }) =
	let val changed = ref true
	    fun canStay GG.ERRORGROUP = true (* doesn't matter *)
	      | canStay (GG.GROUP { sublibs, ... }) = let
		  fun goodSublib (p, GG.GROUP { kind = GG.STABLELIB _, ... }) =
		      SrcPath.compare (p, igp) = EQUAL orelse
		      SrcPathMap.inDomain (!sgc, p)
		    | goodSublib _ = true
		  val cs = List.all goodSublib sublibs
		in
		    if cs then () else changed := true;
		    cs
		end
	in
	    (sgc := #1 (SrcPathMap.remove (!sgc, p)))
	    handle LibBase.NotFound => ();
	    while !changed do
               (changed := false; sgc := SrcPathMap.filter canStay (!sgc))
	end
      | delCachedStable (_, GG.ERRORGROUP) = ()

    fun listLibs () = map #1 (SrcPathMap.listItemsi (!sgc))

    fun dropPickles () = let
	fun drop (GG.GROUP { kind = GG.STABLELIB dropper, ... }) = dropper ()
	  | drop _ = ()
    in
	SrcPathMap.app drop (!sgc)
    end

    fun dismissLib l =
	(StabModmap.reset ();
	 sgc := #1 (SrcPathMap.remove (!sgc, l)))
	handle LibBase.NotFound => ()

    fun parse args = let
	val { load_plugin, gr, param, stabflag, group,
	      init_group, paranoid } = args

	val { grouppath = init_gname, ... } =
	    case init_group of
		GG.GROUP x => x
	      | GG.ERRORGROUP =>
		EM.impossible "parse.sml: parse: bad init group"

	val stabthis = isSome stabflag
	val staball = stabflag = SOME true

	val groupreg = gr
	val errcons = EM.defaultConsumer ()
	val ginfo = { param = param, groupreg = groupreg, errcons = errcons }
	val keep_going = #keep_going param

	(* The "group cache" -- we store "group options";  having
	 * NONE registered for a group means that a previous attempt
	 * to parse it had failed.
	 * This registry is primed with the "init" group because it is
	 * "special" and cannot be parsed directly. *)
	val gc = ref (SrcPathMap.singleton (init_gname, SOME init_group))

	val em = ref StableMap.empty

	fun update_em (GG.GROUP ns_g, GG.GROUP s_g) =
	    let val s_e = #exports s_g
		fun add (sy, ((_ , DG.SB_SNODE (DG.SNODE sn)), _)) =
		    (case SymbolMap.find (s_e, sy) of
			 SOME ((_, DG.SB_BNODE (DG.BNODE bn, _)), _) =>
			 em := StableMap.insert (!em, #bininfo bn, #smlinfo sn)
		       | _ => ())
		  | add _ = ()
	    in
		SymbolMap.appi add (#exports ns_g)
	    end
	  | update_em _ = ()

	fun registerNewStable (p, g) =
	    (sgc := SrcPathMap.insert (!sgc, p, g);
	     SrcPathSet.app (SmlInfo.cleanGroup true) (Reachable.groupsOf g);
	     evictStale ();
	     (gc := #1 (SrcPathMap.remove (!gc, p));
	      (* ... and for good measure, do a major GC... *)
	      majorGC ())
	     handle LibBase.NotFound => ())

	fun hasCycle (group, groupstack) = let
	    (* checking for cycles among groups and printing them nicely *)
	    fun findCycle ([], _) = []
	      | findCycle ((h as (g, (s, p1, p2))) :: t, cyc) =
		if SrcPath.compare (g, group) = EQUAL then rev (h :: cyc)
		else findCycle (t, h :: cyc)
	    fun report ((g, (s, p1, p2)), hist) = let
		fun pphist pps = let
		    fun loop (_, []) = ()
		      | loop (g0, (g, (s, p1, p2)) :: t) = let
			    val s = EM.matchErrorString s (p1, p2)
			in
			    PrettyPrint.add_newline pps;
			    PrettyPrint.add_string pps s;
			    PrettyPrint.add_string pps ": importing ";
			    PrettyPrint.add_string pps (SrcPath.specOf g0);
			    loop (g, t)
			end
		in
		    loop (g, hist)
		end
	    in
		EM.error s (p1, p2) EM.COMPLAIN
		           ("group hierarchy forms a cycle with " ^
			    SrcPath.specOf group)
			   pphist
	    end
	in
	    case findCycle (groupstack, []) of
		h :: t => (report (h, t); true)
	      | [] => false
	end

	fun mparse (group, groupstack, pErrFlag, stabthis, curlib) = let
	    fun getStable stablestack gpath = let
		(* This is a separate "findCycle" routine that detects
		 * cycles among stable libraries.  These cycles should
		 * never occur unless someone purposefully renames
		 * stable library files in a bad way. *)
		fun findCycle ([], _) = NONE
		  | findCycle (h :: t, cyc) =
		    if SrcPath.compare (h, gpath) = EQUAL then SOME (h :: cyc)
		    else findCycle (t, h :: cyc)
		fun report cyc = let
		    fun pphist pps = let
			fun loop [] = ()
			  | loop (h :: t) =
			    (PrettyPrint.add_newline pps;
			     PrettyPrint.add_string pps (SrcPath.descr h);
			     loop t)
		    in
			loop (rev cyc)
		    end
		in
		    EM.errorNoFile (errcons, pErrFlag) SM.nullRegion
		      EM.COMPLAIN
		      ("stable libraries form a cycle with " ^
		       SrcPath.descr gpath)
		      pphist
		end
		fun load () = let
		    val go = Stabilize.loadStable ginfo
			{ getGroup = getStable (gpath :: stablestack),
			  anyerrors = pErrFlag }
			gpath
		in
		    case go of
			NONE => NONE
		      | SOME g => 
			    (registerNewStable (gpath, g);
			     Say.vsay ["[library ", SrcPath.descr gpath,
				       " is stable]\n"];
			     SOME g)
		end
	    in
		case findCycle (stablestack, []) of
		    NONE => (case cachedStable (gpath, init_group) of
				 SOME g => SOME g
			       | NONE => load ())
		  | SOME cyc => (report cyc; NONE)
	    end

	    fun stabilize NONE = NONE
	      | stabilize (SOME g) =
		(case g of
		     GG.ERRORGROUP => NONE
		   | GG.GROUP { kind = GG.LIB _, ... } => let
			 val go = Stabilize.stabilize ginfo
			     { group = g, anyerrors = pErrFlag }
		     in
			 case go of
			     NONE => NONE
			   | SOME g' =>
				 (registerNewStable (group, g'); SOME g')
		     end
		   | _ => SOME g)
	in
	    case SrcPathMap.find (!gc, group) of
		SOME gopt => gopt
	      | NONE => let
		    fun try_s () = getStable [] group
		    fun try_n () = parse' (group, groupstack, pErrFlag, curlib)
		    fun reg gopt =
			(gc := SrcPathMap.insert (!gc, group, gopt); gopt)
		    fun proc_n gopt =
			reg (if stabthis then stabilize gopt
			     else (SmlInfo.cleanGroup false group; gopt))
		in
		    if paranoid then
			case try_n () of
			    NONE => reg NONE
			  | SOME g => let
				val gopt' =
				    if VerifyStable.verify ginfo (!em) g then
					reg (case try_s () of
						 NONE => SOME g
					       | SOME g' => SOME g')
				    else (delCachedStable (group, init_group);
					  proc_n (SOME g))
			    in
				case gopt' of
				    NONE => NONE
				  | SOME g' => (update_em (g, g'); SOME g')
			    end
		    else case try_s () of
			SOME g => reg (SOME g)
		      | NONE => proc_n (try_n ())
		end
	end

	(* Parse' is used when we are sure that we don't want to load
	 * a stable library. *)
	and parse' (group, groupstack, pErrFlag, curlib) = let

	    (* normal processing -- used when there is no cycle to report *)
	    fun normal_processing () = let
		val _ = Say.vsay ["[scanning ", SrcPath.descr group, "]\n"]

		val context = SrcPath.sameDirContext group

		fun work stream = let
		    val source =
			S.newSource (SrcPath.osstring group,
				     1, stream, false, errcons)
		    val sourceMap = #sourceMap source
		    val _ = GroupReg.register groupreg (group, source)

		    (* We can hard-wire the source into this
		     * error function because the function is only for
		     * immediate use and doesn't get stored into persistent
		     * data structures. *)
		    fun error r m =
			EM.error source r EM.COMPLAIN m EM.nullErrorBody
		    fun obsolete r =
			if #get StdConfig.warn_obsolete () then
			    EM.error source r EM.WARN
			      "old-style operator (obsolete)" EM.nullErrorBody
			else ()

		    (* recParse returns a group (not an option).
		     * This function is used to parse sub-groups.
		     * Errors are propagated by explicitly setting the
		     * "anyErrors" flag of the parent group. *)
		    fun recParse (p1, p2) curlib p = let
			val gs' = (group, (source, p1, p2)) :: groupstack
			(* my error flag *)
			val mef = #anyErrors source
		    in
			(* unless we are in keep-going mode we do no further
			 * recursive traversals once there was an error on
			 * this group. *)
			if !mef andalso not keep_going then GG.ERRORGROUP
			else case mparse (p, gs', mef, staball, curlib) of
				 NONE => (mef := true; GG.ERRORGROUP)
			       | SOME res => res
		    end
	            handle exn as IO.Io _ =>
			(error (p1, p2) (General.exnMessage exn);
			 GG.ERRORGROUP)

		    fun doMember ({ name, mkpath }, p1, p2, c, oto) =
			CMSemant.member (ginfo, recParse (p1, p2), load_plugin)
			  { name = name, mkpath = mkpath,
			    class = c, tooloptions = oto,
			    group = (group, (p1, p2)),
			    context = context }

		    (* Build the argument for the lexer; the lexer's local
		     * state is encapsulated here to make sure the parser
		     * is re-entrant. *)
		    val lexarg = let
			(* local state *)
			val depth = ref 0
			val curstring = ref []
			val startpos = ref 0
			val instring = ref false
			(* handling comments *)
			fun enterC () = depth := !depth + 1
			fun leaveC () = let
			    val d = !depth - 1
			in
			    depth := d;
			    d = 0
			end
			(* handling strings *)
			fun newS pos =
			    (instring := true;
			     curstring := [];
			     startpos := pos)
			fun addS c = curstring := c :: !curstring
			fun addSC (s, offs) =
			    addS (chr (ord (String.sub (s, 2)) - offs))
			fun addSN (s, pos) = let
			    val ns = substring (s, 1, 3)
			    val n = Int.fromString ns
			in
			    addS (chr (valOf n))
			    handle _ =>
				error (pos, pos + size s)
				         ("illegal decimal char spec: " ^ ns)
			end
			fun getS (pos, tok) =
			    (instring := false;
			     tok (implode (rev (!curstring)), !startpos, pos))
			(* handling EOF *)
			fun handleEof () = let
			    val pos = SM.lastChange sourceMap
			in
			    if !depth > 0 then
				error (pos, pos)
				       "unexpected end of input in comment"
			    else if !instring then
				error (pos, pos)
				       "unexpected end of input in string"
			    else ();
			    pos
			end
			(* handling line breaks *)
			fun newline pos = SM.newline sourceMap pos
			(* handling #line directives *)
			fun sync (p, t) = let
			    fun sep c = c = #"#" orelse Char.isSpace c
			    fun cvt s = getOpt (Int.fromString s, 0)
			    fun r (line, col, file) = SM.resynch sourceMap
				(p, { fileName = file,
				      line = line, column = col })
			in
			    case String.tokens sep t of
				[_, line] =>
				    r (cvt line, NONE, NONE)
			      | [_, line, file] =>
				    r (cvt line, NONE, SOME file)
			      | [_, line, col, file] =>
				    r (cvt line, SOME (cvt col), SOME file)
			      | _ => error (p, p + size t)
				    "illegal #line directive"
			end
		    in
			{ enterC = enterC,
			  leaveC = leaveC,
			  newS = newS,
			  addS = addS,
			  addSC = addSC,
			  addSN = addSN,
			  getS = getS,
			  handleEof = handleEof,
			  newline = newline,
			  obsolete = obsolete,
			  error = error,
			  sync = sync}
		    end

		    fun inputc k = TextIO.input stream

		    val lexer = CMLex.makeLexer inputc lexarg
		    val tokenStream = LrParser.Stream.streamify lexer
		    val (parseResult, _) =
			CMParse.parse (lookAhead, tokenStream,
				       fn (s,p1,p2) => error (p1, p2) s,
				       (group, context, obsolete, error,
					doMember, curlib, ginfo, init_group))
		in
		    if !(#anyErrors source) then NONE
		    else SOME parseResult
		end
		fun openIt () = TextIO.openIn (SrcPath.osstring group)
	    in
		SafeIO.perform { openIt = openIt,
				 closeIt = TextIO.closeIn,
				 work = work,
				 cleanup = fn _ => () }
	    end
            handle LrParser.ParseError => NONE
	in
	    if hasCycle (group, groupstack) then NONE
	    else normal_processing ()
	end
    in
	SmlInfo.newGeneration ();
	case mparse (group, [], ref false, stabthis, NONE) of
	    NONE => NONE
	  | SOME g => SOME (g, ginfo)
    end
end
