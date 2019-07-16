(*
 * tools/tools.sml: managing source classes and tools
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
functor ToolsFun (structure Control: CONTROL): TOOLS = struct

    exception ToolError of { tool: string, msg: string }

    type fname = string
    type class = string

    exception UnknownClass of class

    datatype classifier =
	SFX_CLASSIFIER of string -> class option
      | GEN_CLASSIFIER of fname -> class option

    type target = fname * class option

    type rulefn = unit -> target list
    type rulecontext = rulefn -> target list
    type rule = fname * rulecontext -> target list
    type simplerule = fname -> target list

    fun dontcare sr (fname, _) = sr fname
    fun withcontext sr (fname, ctxt) = ctxt (fn () => sr fname)

    type validator = { source: fname, targets: target list } -> bool
    type processor = { source: fname, targets: target list } -> unit

    val sfx_classifiers: (string -> class option) list ref = ref []
    val gen_classifiers: (fname -> class option) list ref = ref []

    local
	fun add (x, r) = r := x :: (!r)
    in
	fun addClassifier (SFX_CLASSIFIER c) = add (c, sfx_classifiers)
	  | addClassifier (GEN_CLASSIFIER c) = add (c, gen_classifiers)
    end

    datatype classinfo =
	SMLCLASS | CMCLASS | SCGCLASS | SCLCLASS
      | TOOLCLASS of { rule: rule, validator: validator, processor: processor }

    val string_eq = (op =): string * string -> bool
    val classes: (class, classinfo) Table.table = Table.create string_eq

    fun addSmlClass c = Table.enter (classes, c, SMLCLASS)
    fun addCmClass c = Table.enter (classes, c, CMCLASS)
    fun addScGClass c = Table.enter (classes, c, SCGCLASS)
    fun addScLClass c = Table.enter (classes, c, SCLCLASS)

    fun addToolClass { class, rule, validator, processor } =
	Table.enter (classes, class,
		     TOOLCLASS { rule = rule,
				 validator = validator,
				 processor = processor })

    type abstarget = AbsPath.t * class option

    datatype classification =
	SMLSOURCE | CMFILE | SCGROUP | SCLIBRARY
      | TOOLINPUT of
	{ targets: abstarget list, validate: unit -> bool, make: unit -> unit }

    fun defaultClassOf source = let
	fun gen_loop [] = NONE
	  | gen_loop (h :: t) =
	    (case h source of
		 NONE => gen_loop t
	       | SOME c => SOME c)

	fun sfx_loop e = let
	    fun loop [] = gen_loop (!gen_classifiers)
	      | loop (h :: t) =
		(case h e of
		     NONE => loop t
		   | SOME c => SOME c)
	in
	    loop (!sfx_classifiers)
	end
    in
	case OS.Path.ext source of
	    SOME e => sfx_loop e
	  | NONE => gen_loop (!gen_classifiers)
    end

    fun classify (source, class) = let

	val { context, name, rigid } = AbsPath.spec source

	fun incontext thunk = let
	    val dir = AbsPath.elab context
	    val cwd = OS.FileSys.getDir ()
	    fun doit () = let
		val _ = OS.FileSys.chDir dir
		val res = thunk ()
		val _ = OS.FileSys.chDir cwd
	    in res end
	in
	    (Interrupt.guarded doit)
	    handle exn => (OS.FileSys.chDir cwd; raise exn)
	end

	fun elab_class c =
	    case Table.find (classes, c) of
		NONE => raise UnknownClass c
	      | SOME SMLCLASS => SMLSOURCE
	      | SOME CMCLASS => CMFILE
	      | SOME SCGCLASS => SCGROUP
	      | SOME SCLCLASS => SCLIBRARY
	      | SOME (TOOLCLASS { rule, validator, processor }) =>
		    let
			val targets = rule (name, incontext)
			fun mkabs (n, co) =
			    (AbsPath.native { context = context,
					      spec = n, rigid = rigid },
			     co)
			val abstargets = map mkabs targets
			fun validate_thunk () =
			    validator { source = name, targets = targets }
			fun validate () = incontext validate_thunk
			fun make_thunk () =
			    processor { source = name, targets = targets }
			fun make () = incontext make_thunk
		    in
			TOOLINPUT { targets = abstargets,
				    validate = validate,
				    make = make }
		    end
    in
	case class of
	    SOME c =>
		elab_class (String.map Char.toLower c)
	  | NONE =>
		(case defaultClassOf (AbsPath.elab source) of
		     NONE => SMLSOURCE
		   | SOME c => elab_class c)
    end

    fun stdSfxClassifier { sfx, class } =
	SFX_CLASSIFIER (fn e => if sfx = e then SOME class else NONE)

    fun stdTStampValidator { source, targets } = let
	val sourcetime = File.modTime source
	fun isyounger (n, _) =
	    File.exists n andalso
	    Time.< (sourcetime, File.modTime n)
    in
	List.all isyounger targets
    end

    fun stdExistenceValidator { source, targets } = let
	fun exists (n, _) = File.exists n
    in
	List.all exists targets
    end

    fun stdShellProcessor { command, tool } { source, targets } = let
	val cmd = concat [command, " ", source]
	val _ = Control.vsay ("[" ^ cmd ^ "]\n")
    in
	if (OS.Process.system cmd) = OS.Process.success then ()
	else raise ToolError { tool = tool, msg = cmd }
    end

    (* some standard classes and classifiers *)

    val _ = addSmlClass "sml"
    val _ = addCmClass "cmfile"
    val _ = addScGClass "scgroup"
    val _ = addScLClass "sclibrary"

    val _ = addClassifier (stdSfxClassifier { sfx = "sml", class = "sml" })
    val _ = addClassifier (stdSfxClassifier { sfx = "sig", class = "sml" })
    val _ = addClassifier (stdSfxClassifier { sfx = "fun", class = "sml" })
    val _ = addClassifier (stdSfxClassifier { sfx = "link", class = "sml" })
    val _ = addClassifier (stdSfxClassifier { sfx = "yacc", class = "sml" })
    val _ = addClassifier (stdSfxClassifier { sfx = "sc", class = "scgroup" })
    val _ = addClassifier (stdSfxClassifier { sfx = "cm", class = "cmfile" })
end
