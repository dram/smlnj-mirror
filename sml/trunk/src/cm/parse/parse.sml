(*
 * Parser for CM description files.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
signature PARSE = sig
    val parse :
	GroupReg.groupreg option ->
	GeneralParams.param -> bool option ->
	SrcPath.t -> (CMSemant.group * GeneralParams.info) option
    val reset : unit -> unit
    val listLibs : unit -> SrcPath.t list
    val dismissLib : SrcPath.t -> unit
end

functor ParseFn (val pending : unit -> DependencyGraph.impexp SymbolMap.map
		 structure Stabilize: STABILIZE) :> PARSE = struct

    val lookAhead = 30

    structure S = GenericVC.Source
    structure EM = GenericVC.ErrorMsg
    structure SM = GenericVC.SourceMap
    structure GG = GroupGraph

    structure CMLrVals = CMLrValsFun (structure Token = LrParser.Token)
    structure CMLex = CMLexFun (structure Tokens = CMLrVals.Tokens)
    structure CMParse =
	JoinWithArg (structure ParserData = CMLrVals.ParserData
		     structure Lex = CMLex
		     structure LrParser = LrParser)

    (* the "stable group cache" *)
    val sgc = ref (SrcPathMap.empty: CMSemant.group SrcPathMap.map)
    fun reset () = sgc := SrcPathMap.empty

    fun listLibs () = map #1 (SrcPathMap.listItemsi (!sgc))

    fun dismissLib l =
	(sgc := #1 (SrcPathMap.remove (!sgc, l)))
	handle LibBase.NotFound => ()

    fun parse gropt param stabflag group = let

	val stabthis = isSome stabflag
	val staball = stabflag = SOME true

	val groupreg =
	    case gropt of
		SOME r => r
	      | NONE => GroupReg.new ()
	val errcons = EM.defaultConsumer ()
	val ginfo = { param = param, groupreg = groupreg, errcons = errcons }

	(* The "group cache" -- we store "group options";  having
	 * NONE registered for a group means that a previous attempt
	 * to parse it had failed. *)
	val gc = ref (SrcPathMap.empty: CMSemant.group option SrcPathMap.map)

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
			    (sgc := SrcPathMap.insert (!sgc, gpath, g);
			     Say.vsay ["[library ", SrcPath.descr gpath,
				       " is stable]\n"];
			     SmlInfo.cleanGroup gpath;
			     SOME g)
		end
	    in
		case findCycle (stablestack, []) of
		    NONE => (case SrcPathMap.find (!sgc, gpath) of
				 SOME g => SOME g
			       | NONE => load ())
		  | SOME cyc => (report cyc; NONE)
	    end
	in
	    case getStable [] group of
		SOME g => SOME g
	      | NONE =>
		    (case SrcPathMap.find (!gc, group) of
			 SOME gopt => gopt
		       | NONE => let
			     val pres =
				 parse' (group, groupstack, pErrFlag,
					 stabthis, curlib)
			 in
			     gc := SrcPathMap.insert (!gc, group, pres);
			     pres
			 end)
	end

	and parse' (group, groupstack, pErrFlag, stabthis, curlib) = let

	    (* We stabilize libraries only because a stable library will
	     * encompass the contents of its sub-groups
	     * (but not sub-libraries!). *)
	    fun stabilize (g as GG.GROUP { kind = GG.NOLIB, ... }) = SOME g
	      | stabilize g =
		Stabilize.stabilize ginfo { group = g, anyerrors = pErrFlag }

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
			val myErrorFlag = #anyErrors source
		    in
			case mparse (p, gs', myErrorFlag, staball, curlib) of
			    NONE => (myErrorFlag := true;
				     CMSemant.emptyGroup group)
			  | SOME res => res
		    end
	            handle exn as IO.Io _ =>
			(error (p1, p2) (General.exnMessage exn);
			 CMSemant.emptyGroup group)

		    fun doMember (p, p1, p2, c) =
			CMSemant.member (ginfo, recParse (p1, p2))
			         { sourcepath = p, class = c,
				   group = (group, (p1, p2)) }

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
					doMember, curlib, ginfo))
		in
		    if !(#anyErrors source) then NONE
		    else SOME parseResult
		end
		fun openIt () = TextIO.openIn (SrcPath.osstring group)
		val pro =
		    SafeIO.perform { openIt = openIt,
				     closeIt = TextIO.closeIn,
				     work = work,
				     cleanup = fn _ => () }
	    in
		case pro of
		    NONE => NONE
		  | SOME pr =>
			(SmlInfo.cleanGroup group;
			 if stabthis then stabilize pr
			 else SOME pr)
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
