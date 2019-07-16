(*
 * entity/description.sml: Entity description file parser.
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
functor EntityDescFun (structure Decl: DECL
		       structure FnameRules: FNAME_RULES
		       structure SmlSource: SML_SOURCE
		       structure Control: CONTROL
		       structure Tools: TOOLS
		       sharing Decl.MD = SmlSource.MD): ENTITY_DESC =
  struct

    structure Tools = Tools

    exception BadEntityDescription of string * string
    exception BugInParser
    exception FileNotFound of string
    exception AliasNestingTooDeep of string

    val maxalias = 32			(* maximum alias nesting *)

    datatype member = M of { name: AbsPath.t,
			     history: string list,
			     classification: Tools.classification }

    datatype export =
	SIG of string
      | STR of string
      | FCT of string
      | FSIG of string

    datatype elab_params =
	EP of {
	       path: AbsPath.t list,
	       lparm: {
		       strdef: string -> bool,
		       sigdef: string -> bool,
		       fctdef: string -> bool,
		       fsigdef: string -> bool,
		       symval: string -> int option
		      }
	      }

    (* group/library of (exports, members, directory) *)
    datatype description =
	ENTITY of {
		   lib: bool,
		   exports: export list,
		   members: member list,
		   location: AbsPath.t,
		   stable: bool
		  }
    
    type time = Time.time

    datatype kind = GROUP' | LIBRARY' | ALIAS'

    fun generic_search (default, process, mk, search_path) name = let
	fun search [] = default
	  | search (hd :: tl) = let
		val f = mk { context = hd, spec = name, rigid = false }
	    in
		if AbsPath.exists f then process f else search tl
	    end
    in
	search search_path
    end

    fun parser ep (context, absfname, s) = let

	val EP { path = search_path, lparm } = ep

	fun generic_canon mk { context, name, copt, c_override } = let

	    fun isDesc Tools.CMFILE = true
	      | isDesc Tools.SCLIBRARY = true
	      | isDesc Tools.SCGROUP = true
	      | isDesc _ = false

	    val classify =
		case c_override of
		    SOME c => (fn _ => c)
		  | NONE => (fn p => Tools.classify (p, copt))

	    val default = mk { context = context, spec = name, rigid = true }
	    val defaultclass = classify default

	    val search =
		generic_search ((default, defaultclass),
				fn f => (f, classify f),
				mk, search_path)

	    (*
	    fun search [] = (default, defaultclass)
	      | search (hd :: tl) = let
		    val f = mk { context = hd, spec = name, rigid = false }
		in
		    if AbsPath.exists f then (f, classify f)
		    else search tl
		end
            *)
	in
	    if AbsPath.exists default orelse not (isDesc defaultclass) then
		(default, defaultclass)
	    else
		search name
	end

	val native_canon = generic_canon AbsPath.native
	val standard_canon = generic_canon AbsPath.standard

	val fname = AbsPath.elab absfname

	fun bogus s =
	    raise BadEntityDescription (fname, s)

	val lex = Lexer.lexer lparm (fname, s)
	val lookahead: Lexer.token list ref = ref []
	fun normal () =
	    case !lookahead of
		[] => lex Lexer.NORMAL
	      | (h :: t) => (lookahead := t; h)
	fun member () =
	    case !lookahead of
		[] => lex Lexer.MEMBERS
	      | (h :: t) => (lookahead := t; h)
	fun unget t = lookahead := (t :: (!lookahead))

	fun get_kind () =
	    case normal () of
		Lexer.T_KEYWORD Lexer.K_GROUP => GROUP'
	      | Lexer.T_KEYWORD Lexer.K_LIBRARY => LIBRARY'
	      | Lexer.T_KEYWORD Lexer.K_ALIAS => ALIAS'
	      | _ => bogus "expected: 'group' or 'library'"

	fun readExport () = let
	    fun name () =
		case normal () of
		    Lexer.T_SYMBOL s => s
		  | Lexer.T_STRING s => s
		  | _ => bogus "missing exported name"
	in
	    case normal () of
		Lexer.T_KEYWORD Lexer.K_SIGNATURE => SOME (SIG (name ()))
	      | Lexer.T_KEYWORD Lexer.K_STRUCTURE => SOME (STR (name ()))
	      | Lexer.T_KEYWORD Lexer.K_FUNCTOR => SOME (FCT (name ()))
	      | Lexer.T_KEYWORD Lexer.K_FUNSIG => SOME (FSIG (name ()))
	      | x => (unget x; NONE)
	end

	fun readList readItem () = let
	    fun loop accu =
		case readItem () of
		    NONE => accu
		  | SOME i => loop (i :: accu)
	    val l = loop []
	in
	    rev l
	end

	val readExportList = readList readExport

	fun autocanon (canon, name) copt =
	    canon { context = context, name = name,
		    copt = copt, c_override = NONE }

	fun getFileName () =
	    case member () of
		Lexer.T_SYMBOL name =>
		    SOME (autocanon (standard_canon, name))
	      | Lexer.T_STRING name =>
		    SOME (autocanon (native_canon, name))
	      | t => (unget t; NONE)

	fun readMember () = let
	    fun processfile f =
		case member () of
		    Lexer.T_COLON => let
			fun processclass class = let
			    val (name, classification) = f (SOME class)
			in
			    SOME (M { name = name,
				      history = [],
				      classification = classification })
			end
		    in
			case member () of
			    Lexer.T_SYMBOL class => processclass class
			  | Lexer.T_STRING class => processclass class
			  | _ => bogus "missing class name"
		    end
		  | t => let
			val (name, classification) = f NONE
		    in
			unget t;
			SOME (M { name = name,
				  history = [],
				  classification = classification })
		    end
	in
	    case getFileName () of
		SOME f => processfile f
	      | NONE => NONE
	end

	val readMemberList = readList readMember

	fun readSCMember () = let
	    fun process str = let
		val (lib, name) =
		    if String.sub (str, 0) = #"-" then
			(true, substring (str, 1, size str - 1))
		    else
			(false, str)
		val c_override =
		    if lib then SOME Tools.SCLIBRARY
		    else NONE
		val (name, classification) =
		    standard_canon { context = context,
				     name = name,
				     copt = NONE,
				     c_override = c_override }
	    in
		SOME (M { name = name,
			  history = [],
			  classification = classification })
	    end
	in
	    case member () of
		Lexer.T_SYMBOL str => process str
	      | Lexer.T_STRING str => process str
	      | t => (unget t; NONE)
	end

	val readSCMemberList = readList readSCMember

	fun readMembers () =
	    case normal () of
		Lexer.T_KEYWORD Lexer.K_IS =>
		    (if !lookahead <> [] then raise BugInParser else ();
		     readMemberList ())
	      | _ => bogus "missing keyword 'is'"

    in
	{ get_kind = get_kind,
	  getFileName = getFileName,
	  readExportList = readExportList,
	  readMembers = readMembers,
	  readSCMemberList = readSCMemberList }
    end

    fun sameFile (f, f') = AbsPath.compare (f, f') = EQUAL

    (*
     * The internal cache for descriptions:
     *  normal descriptions are put into the cache with SOME timestamp,
     *  stable descriptions have NONE.
     *)
    val cache: (AbsPath.t, description * time option) Table.table =
	Table.create sameFile

    fun clear () = Table.clear cache

    (* the main read function for CM entities *)
    fun read (ep as EP { path = search_path, ... }) = let

	val parser = parser ep

	fun do_read (0, pathname, _) =
	    raise AliasNestingTooDeep (AbsPath.elab pathname)
	  | do_read (cnt, pathname, msg) = let

		val context = AbsPath.dir pathname
		val pathstring = AbsPath.elab pathname

		fun bogus s = raise BadEntityDescription (pathstring, s)

		fun getTime () = AbsPath.modTime pathname

		fun notcached modtime = let
		    val msg = pathstring :: msg
		    val ins = TextIO.openIn pathstring
			handle _ => raise FileNotFound pathstring
		    val { get_kind, getFileName,
			  readExportList, readMembers, ... } =
			parser (context, pathname, ins)
		    fun parse () =
			case get_kind () of
			    ALIAS' =>
				(case getFileName () of
				     NONE => bogus "alias name missing"
				   | SOME f => let
					 val _ = TextIO.closeIn ins
					 val (alias, _) = f NONE
				     in
					 do_read (cnt - 1, alias,
						  " -> " :: msg)
				     end)
			  | kind => let
				val _ = Control.vsay
				    (concat (rev ("]\n" :: msg)))
				val exports = readExportList ()
				val members = readMembers ()
				val _ = TextIO.closeIn ins
				val lib = kind = LIBRARY'
				val _ = 
				    if lib andalso exports = [] then
					bogus "library without export list"
				    else ()
				fun get (Decl.ENTITY { name, class }) =
				    M { name = name,
				        history = [],
				        classification =
					Tools.classify (name, SOME class) }
				  | get (Decl.FILE { name, decl }) =
				    (SmlSource.mkstable (name, decl);
				     M { name = name,
					 history = ["stable"],
					 classification = Tools.SMLSOURCE })
				val stablefile =
				    FnameRules.stableFileFor pathname
				val descdir = AbsPath.dir pathname
				val _ =
				    Control.vsay
				      (concat ["[checking ",
					       AbsPath.elab stablefile,
					       " ..."])
				fun default name =
				    AbsPath.native { context = context,
						     spec = name,
						     rigid = true }
				fun search name =
				    generic_search (default name,
						    fn x => x,
						    AbsPath.native,
						    search_path) name
				fun canon { name, rigid = true } = default name
				  | canon { name, ... } = search name
			    in
				case Decl.recover_stable
				    (stablefile, modtime, canon) of
				    NONE => let
					val _ = Control.vsay " not usable]\n"
					val e = ENTITY {
							lib = lib,
							exports = exports,
							members = members,
							location = pathname,
							stable = false
						       }
				    in
					Table.enter (cache, pathname,
						     (e, SOME modtime));
					e
				    end
				  | SOME ml => let
					val _ = Control.vsay " ok - stable]\n"
					val e = ENTITY {
							lib = lib,
							exports = exports,
							members = map get ml,
							location = pathname,
							stable = true
						       }
				    in
					Table.enter (cache, pathname,
						     (e, NONE));
					e
				    end
			    end
			val e =
			    parse ()
			    handle exn => (TextIO.closeIn ins; raise exn);
		in
		    TextIO.closeIn ins;
		    e
		end
	    in
		case Table.find (cache, pathname) of
		    NONE => notcached (getTime ())
		  | SOME (e, NONE) => e
		  | SOME (e, SOME ts) => let
			val mt = getTime ()
		    in
			if Time.< (ts, mt) then
			    notcached mt
			else e
		    end
	    end

    in
	fn pathname =>
	do_read (maxalias, pathname, ["[scanning "])
    end

    fun readSCEntity lib (ep as EP { path, lparm }) pathname = let
	val context = AbsPath.dir pathname
	val pathstring = AbsPath.elab pathname
	val ins = TextIO.openIn pathstring
	    handle _ => raise FileNotFound pathstring
	val { readSCMemberList, ... } = parser ep (context, pathname, ins)
	val ml = readSCMemberList ()
	    handle exn => (TextIO.closeIn ins; raise exn)
	val _ = TextIO.closeIn ins
    in
	ENTITY {
		lib = lib,
		exports = [],
		members = ml,
		location = pathname,
		stable = false
	       }
    end

    val readSCGroup = readSCEntity false
    val readSCLibrary = readSCEntity true

  end
