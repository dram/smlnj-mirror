(* control.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *)

signature PRINTCONTROL =
  sig
   val printDepth : int ref
   val printLength : int ref
   val stringDepth : int ref
   val printLoop : bool ref
   val signatures : int ref
   val printOpens : bool ref
   val out : {say : string -> unit, flush : unit -> unit} ref
   val linewidth : int ref
   val say : string -> unit 
   val flush: unit -> unit
 end

signature LAZYCONTROL = 
sig
  val printDebug : bool ref  (* General Degubing Info *)
  val printStats : bool ref  (* General Statistics *)
  val redGenFD1 : bool ref   (* Reduce Generated Force-Delays at Ast Level *)
  val redGenFD2 : bool ref   (* Reduce Generated Force-Delays at matchcomp *)
  val redAllFD1 : bool ref   (* Reduce All Force-Delays at Ast Level *)
  val redAllFD2 : bool ref   (* Reduce All Force-Delays at machcomp *)
  val inlineF : bool ref     (* Inline Force *)
  val inlineD : bool ref     (* Inline Delay *)
  val earlyDT : bool ref     (* Early (Ast) Dollar translate *)
  val ok_MARK : bool ref     (* Correct treatment of MARK in translate *)
  val earlyFP : bool ref     (* Early fixity parsing *)
end

(* match compiler flags *)
signature MCCONTROL =
sig
  val printArgs : bool ref
  val printRet : bool ref
  val bindNoVariableWarn : bool ref
    (* warning for binding patterns containing no variables; default false *)
  val bindNonExhaustiveWarn : bool ref
    (* warning for nonexhaustive binding pattern; default true *)
  val matchNonExhaustiveWarn : bool ref
    (* warning for nonexhaustive match; default true *)
  val matchNonExhaustiveError : bool ref
    (* error for nonexhaustive match (overrides matchNonExhaustiveWarn;
     * default false *)
  val matchRedundantWarn : bool ref
    (* warning for redundant rules in match; default true *)
  val matchRedundantError : bool ref
    (* error for redundant rules in match (overrides matchRedundantWarn);
     * default true *)
  val expandResult : bool ref
end

signature CGCONTROL =
sig
  val tailrecur : bool ref
  val recordopt : bool ref
  val specialize : bool ref
  val tail : bool ref
  val allocprof : bool ref
  val closureprint : bool ref
  val closureStrategy : int ref
  val lambdaopt : bool ref
  val cpsopt : bool ref
  val rounds : int ref
  val path : bool ref
  val betacontract : bool ref
  val eta : bool ref
  val selectopt : bool ref
  val dropargs : bool ref
  val deadvars : bool ref
  val flattenargs : bool ref
  val extraflatten : bool ref
  val switchopt : bool ref
  val handlerfold : bool ref
  val branchfold : bool ref
  val arithopt : bool ref
  val betaexpand : bool ref
  val unroll : bool ref
  val knownfiddle : bool ref
  val invariant: bool ref
  val targeting: int ref
  val lambdaprop: bool ref
  val newconreps : bool ref
  val boxedconstconreps : bool ref
  val sharepath : bool ref
  val staticprof : bool ref
  val unroll_recur : bool ref
  val hoistup : bool ref
  val hoistdown : bool ref
  val recordcopy : bool ref
  val recordpath : bool ref
  val debugcps : bool ref
  val misc4 : int ref
  val argrep : bool ref
  val bodysize : int ref
  val reducemore : int ref
  val alphac : bool ref
  val comment : bool ref
  val knownGen : int ref
  val knownClGen : int ref
  val escapeGen : int ref
  val calleeGen : int ref
  val spillGen : int ref
  val foldconst : bool ref
  val etasplit : bool ref
  val printLambda : bool ref
  val printit : bool ref
  val printsize : bool ref
  val scheduling : bool ref
  val cse : bool ref
  val optafterclosure : bool ref
  val uncurry : bool ref
  val ifidiom : bool ref
  val comparefold : bool ref
  val csehoist : bool ref
  val rangeopt : bool ref
  val icount : bool ref
  val debugRep : bool ref  
  val sharewrap : bool ref
  val checklty1 : bool ref
  val checklty2 : bool ref
  val checklty3 : bool ref
  val checkcps1 : bool ref
  val checkcps2 : bool ref
  val checkcps3 : bool ref
  val checkcps  : bool ref
  val liftLiterals : bool ref
  val flatfblock : bool ref
  val deadup : bool ref
  val pollChecks : bool ref
  val pollRatioAtoI : real ref

  datatype mlrisc_phase = 
      NO_PHASE
    | AFTER_INSTR_SEL
    | AFTER_RA
    | AFTER_SCHED
    | PHASES of mlrisc_phase * mlrisc_phase
  val printFlowgraph : mlrisc_phase ref
  val printFlowgraphStream : TextIO.outstream ref

  val memDisambiguate : bool ref
  

  val mudebugging : bool ref
  val eedebugging : bool ref
  val insdebugging : bool ref
  val smdebugging : bool ref
  val emdebugging : bool ref
  val esdebugging : bool ref
  val etdebugging : bool ref
  val ecdebugging : bool ref
  val tmdebugging : bool ref
end

signature CONTROL = 
   sig structure MC : MCCONTROL
       structure Lazy : LAZYCONTROL
       structure CG : CGCONTROL
       structure Print : PRINTCONTROL
       val debugging : bool ref
       val primaryPrompt : string ref
       val secondaryPrompt : string ref
       val printWarnings : bool ref
          (* if false, suppress all warning messages *)
       val valueRestrictionLocalWarn : bool ref
          (* warning message on failure of value restriction in local decls *)
       val valueRestrictionTopWarn : bool ref
          (* warning message on failure of value restriction at top level *)
       val multDefWarn : bool ref    (* default false *)
          (* warning messages for multiple defs in sigs *)
       val instantiateSigs : bool ref 
          (* check signatures at declaration by instantiating them *)
       val internals : bool ref
          (* print internal representations of types at top level *)
       val interp : bool ref
          (* turn on interpreter -- defunct *)
(*
       val debugLook : bool ref
       val debugCollect : bool ref
       val debugBind : bool ref
*)
       val saveLambda : bool ref
       val saveLvarNames : bool ref
       val preserveLvarNames : bool ref
       val markabsyn : bool ref
       val trackExn : bool ref
       val indexing : bool ref
       val instSigs : bool ref
       val quotation : bool ref

       val saveit : bool ref
       val saveAbsyn : bool ref
       val saveConvert : bool ref
       val saveCPSopt : bool ref
       val saveClosure : bool ref

       val lambdaSplitEnable: bool ref
       val crossInlineEnable: bool ref
   end

structure Control : CONTROL =
  struct
    structure Print : PRINTCONTROL =
      struct
        val printDepth = ref 5
        val printLength = ref 12
        val stringDepth = ref 70
        val printLoop = ref true
        val signatures = ref 2
	val printOpens = ref true
        val out = ref{
		say = fn s => TextIO.output(TextIO.stdOut,s),
		flush = fn () => TextIO.flushOut TextIO.stdOut
	      }
        val linewidth = ref 79
        fun say s = #say (!out) s
        fun flush() = #flush (!out) ()
      end

    structure Lazy : LAZYCONTROL =
      struct
        val printDebug = ref false (* General Degubing Info *)
        val printStats = ref false (* General Statistics *)
        val redGenFD1 = ref true  (* Reduce Generated Force-Delays at Ast Level *)
        val redGenFD2 = ref true  (* Reduce Generated Force-Delays at matchcomp *)
        val redAllFD1 = ref true  (* Reduce All Force-Delays at Ast Level *)
        val redAllFD2 = ref true  (* Reduce All Force-Delays at machcomp *)
        val inlineF = ref true    (* Inline Force *)
        val inlineD = ref true    (* Inline Delay *)
        val earlyDT = ref false   (* Early (Ast) Dollar translate *)
        val ok_MARK = ref true    (* Correct treatment of MARK in translate *)
        val earlyFP = ref true    (* Early fixity parsing *)
      end
 
    (* match compiler flags *)
    structure MC : MCCONTROL =
    struct
      val printArgs = ref false
      val printRet = ref false
      val bindNoVariableWarn = ref false
      val bindNonExhaustiveWarn = ref true
      val matchNonExhaustiveWarn = ref true
      val matchNonExhaustiveError = ref false
          (* matchExhaustiveError overrides matchExhaustiveWarn *)
      val matchRedundantWarn = ref true
      val matchRedundantError = ref true
          (* matchRedundantError overrides matchRedundantWarn *)
      val expandResult = ref false
    end

    structure CG : CGCONTROL =
    struct
      val tailrecur = ref true
      val recordopt = ref true
      val specialize = ref true
      val tail = ref true
      val allocprof = ref false
      val closureprint = ref false
      val closureStrategy = ref 0
      val lambdaopt = ref true
      val cpsopt = ref true
      val rounds = ref 10
      val path = ref false
      val betacontract = ref true
      val eta = ref true
      val selectopt = ref true
      val dropargs = ref true
      val deadvars = ref true
      val flattenargs = ref false
      val extraflatten = ref false
      val switchopt = ref true
      val handlerfold = ref true
      val branchfold = ref false
      val arithopt = ref true
      val betaexpand = ref true
      val unroll = ref true
      val knownfiddle = ref false
      val invariant = ref true
      val targeting = ref 0
      val lambdaprop = ref false
      val newconreps = ref true
      val boxedconstconreps = ref false
      val unroll_recur = ref true
      val sharepath = ref true
      val staticprof = ref false
      val hoistup = ref false
      val hoistdown = ref false
      val recordcopy = ref true
      val recordpath = ref true
      val verbose = ref false
      val debugcps = ref false
      val misc4 = ref 0
      val argrep = ref true
      val bodysize = ref 20
      val reducemore = ref 15
      val alphac = ref true
      val comment = ref false
      val knownGen = ref 0
      val knownClGen = ref 0
      val escapeGen = ref 0
      val calleeGen = ref 0
      val spillGen = ref 0
      val foldconst = ref true
      val etasplit = ref true
      val printLambda = ref false
      val printit = ref false
      val printsize = ref false
      val scheduling = ref true
      val cse = ref false
      val optafterclosure = ref false
      val uncurry = ref true
      val ifidiom = ref true
      val comparefold = ref true
      val csehoist = ref false
      val rangeopt = ref false
      val icount = ref false
      val debugRep = ref false
      val sharewrap = ref true
      val checklty1 = ref false
      val checklty2 = ref false
      val checklty3 = ref false
      val checkcps1 = ref false
      val checkcps2 = ref false
      val checkcps3 = ref false
      val checkcps = ref false
      val liftLiterals = ref false
      val flatfblock = ref true
      val deadup = ref true
      val pollChecks = ref false
      val pollRatioAtoI = ref 1.0

      datatype mlrisc_phase = 
	  NO_PHASE
	| AFTER_INSTR_SEL
	| AFTER_RA
	| AFTER_SCHED
	| PHASES of mlrisc_phase * mlrisc_phase
      val printFlowgraph = ref NO_PHASE
      val printFlowgraphStream = ref TextIO.stdOut

      val memDisambiguate = ref false

      val mudebugging = ref false
      val eedebugging = ref false
      val insdebugging = ref false
      val smdebugging = ref false
      val emdebugging = ref false
      val esdebugging = ref false
      val etdebugging = ref false
      val ecdebugging = ref false
      val tmdebugging = ref false
    end
    val primaryPrompt = ref "- "
    val secondaryPrompt = ref "= "
    val printWarnings = ref true
    val valueRestrictionLocalWarn = ref false
    val valueRestrictionTopWarn = ref true
    val multDefWarn = ref false
    val instantiateSigs = ref true
    val debugging = ref false
    val internals = ref false
    val interp = ref false
(*
    val debugLook = ref false
    val debugCollect = ref false
    val debugBind = ref false
*)
    val markabsyn = ref true
    val trackExn = ref true
    val indexing = ref false
    val instSigs = ref true
    val quotation = ref false  (* controls backquote quotation *)

    val preserveLvarNames : bool ref = ref false
    val saveit = ref false 
    val saveLvarNames : bool ref = saveit
    val saveAbsyn : bool ref = saveit
    val saveLambda : bool ref = saveit
    val saveConvert : bool ref = saveit
    val saveCPSopt : bool ref = saveit
    val saveClosure : bool ref = saveit

    val lambdaSplitEnable = ref false
    val crossInlineEnable  = ref false
end

(*
 * $Log: control.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:43  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.14.2.3  1999/07/20 21:33:09  dbm
 * add multDefWarn flag
 *
 * Revision 1.14.2.2  1999/07/20 14:22:18  dbm
 * change name of bindContainsVar to bindNoVariableWarn
 *
 * Revision 1.14.2.1  1999/06/21 18:39:56  dbm
 * change default value of Control.MC.bindContainsVar and spelling of Control.MC.bindExhaustive (to bindNonExhaustiveWarn)
 *
 * Revision 1.14  1997/11/07 05:38:34  dbm
 *   New flag valueRestrictionTopWarn controlling printing of top level
 *   value restriction warning.  Changed name of valueRestrictionWarn
 *   to valueRestrictionLocalWarn.
 *
 * Revision 1.13  1997/09/30  02:22:49  dbm
 *   Replaced matchExhaustive with matchNonExhaustiveWarn and
 *   matchNonExhaustiveError in Control.MC.
 *
 * Revision 1.12  1997/09/23  03:49:14  dbm
 *   Added flag tmdebugging.
 *
 * Revision 1.11  1997/08/26  12:58:14  walidt
 * Added Control.Lazy structure, with flags for controlling translation.
 *
 * Revision 1.10  1997/08/22  18:34:54  george
 *   Deleting the mtderiv flag, adding the sharewrap flag. -- zsh
 *
 * Revision 1.9  1997/08/15  20:32:42  dbm
 *   Added printOpens in Control.Print to control printing of structure
 *   signatures when structures are opened at top level.  Fixes bug 720.
 *
 * Revision 1.8  1997/07/28  20:07:06  george
 * *** empty log message ***
 *
 * Revision 1.7  1997/07/17  20:35:44  dbm
 *   Added instantiateSigs flag to control whether signatures are instantiated
 *   where they are defined.
 *
 * Revision 1.6  1997/07/16  19:42:23  dbm
 *   Version 109.30.
 *
 * Revision 1.5  1997/07/15  16:01:08  dbm
 *   Added matchRedundantError, printWarnings, valueRestrictionWarn flags
 *   and deleted obsolete flags weakUnderscore and copyToplevelOpen.
 *
 * Revision 1.4  1997/06/13  15:28:44  george
 *   Added flag to support printing of flowgraphs -- leunga
 *
 * Revision 1.3  1997/05/05  20:00:02  george
 *   Change the term language into the quasi-A-normal form. Added a new round
 *   of lambda contraction before and after type specialization and
 *   representation analysis. Type specialization including minimum type
 *   derivation is now turned on all the time. Real array is now implemented
 *   as realArray. A more sophisticated partial boxing scheme is added and
 *   used as the default.
 *
 * Revision 1.2  1997/04/18  15:42:54  george
 *   Type specialization is now added in (though it is still turned off).
 *   A pretty-fancy kind of minimum type derivation is also done. -- zsh
 *
 * Revision 1.1.1.1  1997/01/14  01:38:27  george
 *   Version 109.24
 *
 *)
