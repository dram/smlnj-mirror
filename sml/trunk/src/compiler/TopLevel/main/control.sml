(* COPYRIGHT (c) 1995 AT&T Bell Laboratories *)
(* control.sml *)

structure Control_MC : MCCONTROL =
struct
    val m = Controls.module { name = "match-compiler settings",
			      priority = [10, 10, 4],
			      obscurity = 2,
			      prefix = "compiler-mc-",
			      default_suffix = SOME "-default",
			      mk_ename = NONE }

    val r = Controls.registry m Controls.bool

    fun flag (s, d, f) = Controls.new_ref
			     r { stem = s, descr = d, fallback = f }

    val printArgs = flag ("print-args", "arguments print mode", false)
    val printRet = flag ("print-ret", "return print mode", false)
    val bindNoVariableWarn =
	flag ("nobind-warn", "whether to warn if no variables get bound",
	      false)
    val bindNonExhaustiveWarn =
	flag ("warn-non-exhaustive-bind",
	      "whether to warn on non-exhaustive bind",
	      true)
    val matchNonExhaustiveWarn =
	flag ("warn-non-exhaustive-match",
	      "whether to warn on non-exhaustive match",
	      true)
    val matchNonExhaustiveError =
	flag ("error-non-exhaustive-match",
	      "whether non-exhaustive match is an error",
	      false)
    (* matchExhaustiveError overrides matchExhaustiveWarn *)
    val matchRedundantWarn =
	flag ("warn-redundant",
	      "whether to warn on redundant matches",
	      true)
    val matchRedundantError =
	flag ("error-redundant",
	      "whether a redundant match is an error",
	      true)
    (* matchRedundantError overrides matchRedundantWarn *)
(*
    val expandResult =
	flag ("expand-result",
	      "whether to expand result of match",
	      false)
*)
end

structure Control_CG : CGCONTROL =
struct
    val m = Controls.module { name = "code generator settings",
			      priority = [10, 11, 2],
			      obscurity = 6,
			      prefix = "cg-",
			      default_suffix = SOME "-default",
			      mk_ename = NONE }

    val b = Controls.registry m Controls.bool

    val i = Controls.registry m Controls.int

    val r = Controls.registry m Controls.real

    val sl = Controls.registry m Controls.stringList

    fun new (r, s, d, f) =
	Controls.new_ref r { stem = s, descr = d, fallback = f }

    val tailrecur = new (b, "tailrecur", "?", true)
    val recordopt = new (b, "recordopt", "?", true)
    val tail = new (b, "tail", "?", true)
    val allocprof = new (b, "allocprof", "?", false)
    val closureprint = new (b, "closureprint", "?", false)
    val closureStrategy = new (i, "closure-strategy", "?", 0)
    val lambdaopt = new (b, "lambdaopt", "?", true)
    val cpsopt = new (sl, "cpsopt", "cps optimizer phases",
		      ["zeroexpand", "last_contract"])
    (* ["first_contract", "eta", "uncurry", "etasplit",
	"cycle_expand", "eta", "last_contract" ] *)
    val rounds = new (i, "rounds", "max # of cpsopt rounds", 10)
    val path = new (b, "path", "?", false)
    val betacontract = new (b, "betacontract", "?", true)
    val eta = new (b, "eta", "?", true)
    val selectopt = new (b, "selectopt", "?", true)
    val dropargs = new (b, "dropargs", "?", true)
    val deadvars = new (b, "deadvars", "?", true)
    val flattenargs = new (b, "flattenargs", "?", false)
    val extraflatten = new (b, "extraflatten", "?", false)
    val switchopt = new (b, "switchopt", "?", true)
    val handlerfold = new (b, "handlerfold", "?", true)
    val branchfold = new (b, "branchfold", "?", false)
    val arithopt = new (b, "arithopt", "?", true)
    val betaexpand = new (b, "betaexpand", "?", true)
    val unroll = new (b, "unroll", "?", true)
    val knownfiddle = new (b, "knownfiddle", "?", false)
    val invariant = new (b, "invariant", "?", true)
    val targeting = new (i, "targeting", "?", 0)
    val lambdaprop = new (b, "lambdaprop", "?", false)
    val newconreps = new (b, "newconreps", "?", true)
    val boxedconstconreps = ElabControl.boxedconstconreps
    val unroll_recur = new (b, "unroll-recur", "?", true)
    val sharepath = new (b, "sharepath", "?", true)
    val staticprof = new (b, "staticprof", "?", false)
    val hoistup = new (b, "hoistup", "?", false)
    val hoistdown = new (b, "hoistdown", "?", false)
    val recordcopy = new (b, "recordcopy", "?", true)
    val recordpath = new (b, "recordpath", "?", true)
    val verbose = new (b, "verbose", "?", false)
    val debugcps = new (b, "debugcps", "?", false)
    val misc4 = new (i, "misc4", "?", 0)
    val argrep = new (b, "argrep", "?", true)
    val bodysize = new (i, "bodysize", "?", 20)
    val reducemore = new (i, "reducemore", "?", 15)
    val alphac = new (b, "alphac", "?", true)
    val comment = new (b, "comment", "?", false)
    val knownGen = new (i, "known-gen", "?", 0)
    val knownClGen = new (i, "known-cl-gen", "?", 0)
    val escapeGen = new (i, "escape-gen", "?", 0)
    val calleeGen = new (i, "callee-gen", "?", 0)
    val spillGen = new (i, "spill-gen", "?", 0)
    val foldconst = new (b, "foldconst", "?", true)
    val etasplit = new (b, "etasplit", "?", true)
    val printit = new (b, "printit", "whether to show CPS", false)
    val printsize = new (b, "printsize", "?", false)
    val scheduling = new (b, "scheduling", "?", true)
    val cse = new (b, "cse", "?", false)
    val optafterclosure = new (b, "opt-after-closure", "?", false)
    val uncurry = new (b, "uncurry", "?", true)
    val ifidiom = new (b, "if-idiom", "?", true)
    val comparefold = new (b, "comparefold", "?", true)
    val csehoist = new (b, "csehoist", "?", false)
    val rangeopt = new (b, "rangeopt", "?", false)
    val icount = new (b, "icount", "?", false)
    val debugRep = new (b, "debug-rep", "?", false)
    val checklty1 = new (b, "checklty1", "?", false)
    val checklty2 = new (b, "checklty2", "?", false)
    val checklty3 = new (b, "checklty3", "?", false)
    val checkcps1 = new (b, "checkcps1", "?", false)
    val checkcps2 = new (b, "checkcps2", "?", false)
    val checkcps3 = new (b, "checkcps3", "?", false)
    val checkcps = new (b, "checkcps", "?", false)
    val flatfblock = new (b, "flatfblock", "?", true)
    val deadup = new (b, "deadup", "?", true)
    val pollChecks = new (b, "poll-checks", "?", false)
    val pollRatioAtoI = new (r, "poll-ratio-a-to-i", "?", 1.0)

    val printFlowgraphStream = ref TextIO.stdOut

    val memDisambiguate = new (b, "mem-disambiguate", "?", false)
    val controlDependence = new (b, "control-dependence", "?", false)
    val flinton = new (b, "flinton", "?", true)

    val compdebugging = new (b, "compdebugging", "?", false)
    val mudebugging   = ElabDataControl.mudebugging
    val eedebugging   = ElabDataControl.eedebugging
    val insdebugging  = ElabControl.insdebugging
    val smdebugging   = ElabControl.smdebugging
    val emdebugging   = ElabControl.emdebugging
    val esdebugging   = ElabControl.esdebugging
    val etdebugging   = ElabControl.etdebugging
    val ecdebugging   = new (b, "ecdebugging", "?", false)
    val tmdebugging   = new (b, "tmdebugging", "?", false)
end

structure Control : CONTROL =
  struct

    local
	val m = Controls.module { name = "miscellaneous control settings",
				  priority = [10, 10, 9],
				  obscurity = 4,
				  prefix = "control-",
				  default_suffix = SOME "-default",
				  mk_ename = NONE }

	val b = Controls.registry m Controls.bool

	fun new (r, s, d, f) =
	    Controls.new_ref r { stem = s, descr = d, fallback = f }
    in

    structure Print : PRINTCONTROL = Control_Print

    structure MC : MCCONTROL = Control_MC

    structure MLRISC = MLRiscControl

    structure FLINT :> FLINTCONTROL = FLINT_Control

    structure CG : CGCONTROL = Control_CG

    open BasicControl
    (* provides: val printWarnings = ref true
     *)
    open ParserControl
    (* provides: val primaryPrompt = ref "- "
		 val secondaryPrompt = ref "= "
		 val overloadKW = ref false
		 val lazysml = ref false
		 val quotation = ref false
     *)

    val saveLvarNames = ElabDataControl.saveLvarNames

    val valueRestrictionLocalWarn = ElabControl.valueRestrictionLocalWarn
    val valueRestrictionTopWarn = ElabControl.valueRestrictionTopWarn
    val multDefWarn = ElabControl.multDefWarn
    val shareDefError = ElabControl.shareDefError
    val instantiateSigs = ElabControl.instantiateSigs
    val debugging = new (b, "debugging", "?", false)
    val internals = ElabControl.internals
    val interp = new (b, "interp", "?", false)
(*
    val debugLook = ref false
    val debugCollect = ref false
    val debugBind = ref false
*)
    val markabsyn = ElabControl.markabsyn
    val trackExn =
	new (b, "track-exn",
	     "whether to generate code that tracks exceptions", true)
    (* warning message when call of polyEqual compiled: *)
    val polyEqWarn =
	new (b, "poly-eq-warn",
	     "wheter to warn when generating call of polyEqual", true)
    val indexing = new (b, "indexing", "?", false)
    val instSigs = new (b, "inst-sigs", "?", true)

    val preserveLvarNames : bool ref = new (b, "preserve-names", "?", false)
    (* these are really all the same ref cell: *)
    val saveit : bool ref = saveLvarNames
    val saveAbsyn : bool ref = saveit
    val saveLambda : bool ref = saveit
    val saveConvert : bool ref = saveit
    val saveCPSopt : bool ref = saveit
    val saveClosure : bool ref = saveit

    structure LambdaSplitting = struct
	datatype globalsetting = Off | Default of int option
	type localsetting = int option option
	val UseDefault : localsetting = NONE
	fun Suggest s : localsetting = SOME s
	fun parse "off" = SOME Off
	  | parse "on" = SOME (Default NONE)
	  | parse s = Option.map (Default o SOME) (Int.fromString s)
	fun show Off = "off"
	  | show (Default NONE) = "on"
	  | show (Default (SOME i)) = Int.toString i
	local
	    val m = Controls.module { name = "cross-module inlining",
				      priority = [10, 10, 0, 1],
				      obscurity = 1,
				      prefix = "inline-",
				      default_suffix = SOME "-default",
				      mk_ename = NONE }
	    val r = Controls.registry m
		    { tname = "Control.LambdaSplitting.globalsetting",
		      parse = parse, show = show }
	    val state = Controls.new r
		    { stem = "split-aggressiveness",
		      descr = "aggressiveness of lambda-splitter",
		      fallback = Default NONE }
	in
   	    val set = #set state
	    fun get () =
		case #get state () of
		    Off => NONE
		  | Default d => d
	    fun get' NONE = get ()
	      | get' (SOME a) =
		(case #get state () of
		     Off => NONE
		   | Default _ => a)
	end
    end
    val btrace = BTrace.enabled

    end (* local *)
  end
