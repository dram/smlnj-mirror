(* Elaborator/matchcomp/matchcomp.sml *)

(* This is the "unified" match compiler, where match compilation is embeded in an absyn to
 * absyn translation, which drives the process (andor tree, decision tree, "code" gen).
 * The "code" (Absyn.exp) for a match is generated from the decision tree and information
 * from the original andor (used for record/vector destruction and variable bindings (and types).
 * This code performs pattern dispatching and destruction and then invokes the appropriate
 * RHS expressions with the necessary variable bindings.
 *
 * The "code" is in the form of an Absyn.exp.  Absyn.exp has been augmented with two new
 * expression forms: SWITCHexp and VSWITCHexp, which are created only by the match
 * compilation. Match compilation is invoked (on absyn decs) type checking, so it is
 * responsible for "maintaining" the correct type information in the translated absyn.
 *)

structure MatchComp =
struct

local

  structure T = Types
  structure TU = TypesUtil
  structure A = Access
  structure V = VarCon
  structure AS = Absyn
  structure AU = AbsynUtil
  structure EU = ElabUtil

  structure MCC = MCControl
  structure MC = MCCommon
  structure DT = DecisionTree

  structure PP = PrettyPrint
  structure PU = PPUtil
  structure ED = ElabDebug
  structure EM = ErrorMsg

  open AS MC
  structure RS = RuleSet

  (* printing for matchComp *)

  fun bug msg = ErrorMsg.impossible ("MatchComp: " ^ msg)

  val printAndor = MCC.printAndor
  val printDecisionTree = MCC.printDecisionTree
  val printMatchAbsyn = MCC.printMatchAbsyn
  val debugging = MCC.mcdebugging
  val stats = MCC.mcstats

  val say = Control_Print.say
  fun newline () = say "\n"
  fun saynl msg = (say msg; newline())
  fun says strings = saynl (concat strings)

  fun dbsay msg =
      if !debugging
      then (say msg; newline())
      else ()
  fun dbsays msgs = dbsay (concat msgs)

  val db_printDepth = 100

  fun ppAndor andor =
      PP.with_default_pp
	  (fn ppstrm =>
	      (PP.string ppstrm "andor:\n";
	       MCPrint.ppAndor ppstrm andor;
	       PP.newline ppstrm))

  fun ppDecisionTree dectree =
      PP.with_default_pp
	  (fn ppstrm =>
	      (PP.string ppstrm "dectree:\n";
	       MCPrint.ppDecTree ppstrm dectree;
	       PP.newline ppstrm))

  fun ppExp (exp, msg) =
      PP.with_default_pp
          (fn ppstrm =>
	      (PP.string ppstrm msg;
	       PPAbsyn.ppExp (StaticEnv.empty, NONE) ppstrm (exp, db_printDepth);
	       PP.newline ppstrm))

  fun ppDec (dec, msg) =
      PP.with_default_pp
          (fn ppstrm =>
	      (PP.string ppstrm msg;
	       PPAbsyn.ppDec (StaticEnv.empty, NONE) ppstrm (dec, db_printDepth);
	       PP.newline ppstrm))

  fun ppPat pat =
      PP.with_default_pp(fn ppstrm => PPAbsyn.ppPat StaticEnv.empty ppstrm (pat, db_printDepth))

  fun ppVar var =
      PP.with_default_pp(fn ppstrm => PPVal.ppVar ppstrm var)

  fun ppType msg ty =
      PP.with_default_pp
	(fn ppstrm => (PP.string ppstrm (msg^": "); PPType.ppType StaticEnv.empty ppstrm ty))

  fun timeIt x = TimeIt.timeIt (!stats) x

in

(* How should we treat SINGLE constructor patterns, and in particular the
 * "special" ones like *ref* (and *susp* )?  We generate a special single
 * datacon "deconstructor" (expressed as a single-variant SWITCHexp) in these cases.
 * The special cases (ref,susp) are detected and handled in Translate
 * (FLINT/trans/translate.sml).
 * SWITCHexp translates almost directly to Plambda.SWITCH.
 *
 * Also need to deconstruct AND and SINGLE _below_ a terminal OR/Decision node,
 * since variables may occur below the node. This is done by the call of
 * genAndor within the body of genDecTree.
 *)

val choiceTotalThreshold = 10

fun reportStats (nodeCount: int, {rulesUsed, failures, choiceTotal, choiceDist}: DT.decTreeStats) =
    if !stats andalso choiceTotal > choiceTotalThreshold
    then (say "decTree Stats: \n";
	  says ["  nodeCount =   ", Int.toString nodeCount];
	  says ["  choiceTotal = ", Int.toString choiceTotal];
	  newline())
    else ()


(* matchComp : AS.rule list * T.ty * T.ty * T.datacon option
                -> AS.exp * V.var * ruleno list * bool * bool *)
fun matchComp (rules, lhsTy: T.ty, rhsTy: T.ty, failExnOp: T.datacon option) =
let fun timeIt x = TimeIt.timeIt (!stats) x
    val location = "nolocation"
    val rules' = map (fn AS.RULE x => x) rules  (* strip RULE constructor *)
    val _ = MCPrint.debugPrint
              ("matchComp: match = \n",
	        (fn ppstrm => MCPrint.ppMatch ppstrm rules))

    val (numExpandedRules, expandedPats, rhsFunBinders, ruleMap) =
	Preprocessing.expandPats (rules', lhsTy, rhsTy)

    (* ruleset containing all rulenos after or-expansion. If there are or-patterns
     * in the match, numRulesExpanded > length rules. *)
    val allRules: RS.set = RS.fromList (List.tabulate(numExpandedRules, fn x => x));

    val protoAndor: protoAndor = (* ProtoAndor.makeProtoAndor expandedPats *)
        timeIt ("makeProtoAndor", location, ProtoAndor.makeProtoAndor, expandedPats)

    val _ = MCPrint.debugPrint
	      ("** matchComp: protoAndor = ",
	       (fn ppstrm => MCPrint.ppProtoAndor ppstrm protoAndor))

    val (andor: andor, pvarmap: Andor.pvarmap, nodeCount) =
	(* Andor.makeAndor (protoAndor, allRules) *)
	timeIt ("Andor.makeAndor", location, Andor.makeAndor, (protoAndor, lhsTy, allRules))

    val _ = MCPrint.debugPrint
	      ("** matchComp: andor (nodeCount = " ^ Int.toString nodeCount ^ ") =",
	       (fn ppstrm => MCPrint.ppAndor ppstrm andor))

    val _ = MCPrint.debugPrint
	      ("** matchComp: pvarmap = ",
	       (fn ppstrm => MCPrint.ppPvarMap ppstrm pvarmap))

    val decTree = (* DecisionTree.genDecisionTree (andor, allRules) *)
	timeIt ("genDecisionTree", location, DT.genDecisionTree, (andor, allRules))

    val _ = MCPrint.debugPrint
	      ("** matchComp: decTree = ",
	       (fn ppstrm => MCPrint.ppDecTree ppstrm decTree))

    (* checking exhaustiveness and redundancy of rules *)

    val decTreeStats as {rulesUsed, failures, ...} : DT.decTreeStats = DT.decTreeStats decTree
    val unusedRules : ruleset = RS.difference (allRules, rulesUsed)  (* expanded rules *)
    val unusedOriginalRules : ruleset = RS.map (#3 o ruleMap) unusedRules
        (* unusedRules translated back to corresponding original rule numbers *)
    val redundant = not (RS.isEmpty unusedRules)
    val nonexhaustive = (failures > 0)  (* any FAIL nodes => nonexhaustive rules *)

   (* generating the "core" exp for the match *)

    val (coreExp, rootVar) = (* Generate.genMatch (andor, decTree, pvarmap, ruleMap,
                                                      fail, toTcLt, giis) *)
        timeIt ("genMatch", location, Generate.genMatch,
		(andor, decTree, pvarmap, ruleMap, (failExnOp, rhsTy)))

    (* fullExp: wrapping let-bindings of abstracted right-hand-sides around coreExp,
     * (corresponds to newmc "genprelude") *)

    val fullExp: AS.exp = foldl (fn (fbinder, body) => fbinder body) coreExp
				rhsFunBinders

    val _ = if !printMatchAbsyn
	    then ED.withInternals (fn () => ppExp (fullExp, "matchComp: fullExp =\n"))
	    else ()

    val _ = reportStats (nodeCount, decTreeStats)

    (* rudundant <=> not (null unusedOriginalRules) <=> not (null unusedExpandedRules) *)
 in (fullExp, rootVar, RS.toList unusedOriginalRules, redundant, nonexhaustive)
end
(* --------------------------------------------------------------------------- *)
(* Match Compiler entry points *)

(*
 * The three entry points for the match compiler.
 *
 * They take as arguments:
 *   env -- an environment (env);
 *   rules : Absyn.rule list -- a match represented as a list of absyn rules;
 *   rhsTy : Types.ty -- the type of the right-hand-side of the match (rule exps)
 *   err:  a function err to use in printing warning/error messages
 *
 * env and err are only used in the printing of diagnostic information; they are
 * not used in the match compilation per se.
 *
 * If the control flag Control.MC.printArgs is set, they print match.
 *
 * They call matchComp to actually compile the match.
 * This returns a 5-tuple (matchExp, rootVar, unused, redundant, exhaustive).
 *   matchExp : Absyn.exp -- the expression that implements the match.
 *   unused : ruleno list -- unused rules (if match is redundant)
 *   redundant : bool -- is match redundant?
 *   exhaustive : bool -- is match exhaustive?
 *
 * They print warning messages as appropriate, as described below.
 * If the control flag Control.MC.printRet is set, they print the matchExp.
 *
 * They return the matchExp + rootVar (the root variable to which the matched
 * value will be bound (dynamically).
 *)

val emptyEnv = StaticEnv.empty (* default -- needed for warning/error printing *)

fun stripRULE (AS.RULE r) = r

(* bindCompile: Entry point for compiling matches associated with val declarations
 *  (e.g., val x::xs = list).
 *  The match consists of a single rule, corresponding to the let binding.
 *  A match failure causes the Bind exception to be raised.
 *  If the control flag Control.MC.bindNonExhaustiveWarn
 *  is set then a nonexhaustive binding warning is printed. If the control
 *  flag Control.MC.bindNoVariableWarn is set, and the pattern
 *  of the match rule contains no variables or wildcards, a warning is printed.
 *   Note: Arguably, a pattern containing no variables, but one or more wildcards,
 *   should also trigger a warning, but this would cause warnings on declarations
 *   like val _ = <exp>  and  val _:<ty> = <exp>.
 *)
fun bindCompile (rules, lhsTy, rhsTy, err: ErrorMsg.complainer): (AS.exp * V.var) =
    let (* val _ = if !printMatch
		then (say "bindCompile called with:"; PPL.ppMatch emptyEnv rules)
		else () *)
	val bindExn = EU.getBindExn ()
	val (matchExp, rootLvar, _, _, nonexhaustive) =
	    matchComp (rules, lhsTy, rhsTy, SOME bindExn)

	val nonexhaustiveF =
	    nonexhaustive andalso (!MCC.bindNonExhaustiveWarn orelse !MCC.bindNonExhaustiveError)
	val noVarsF = !MCC.bindNoVariableWarn andalso AU.noVarsInPat (#1 (stripRULE (hd rules)))

     in if nonexhaustiveF
        then err (if !MCC.bindNonExhaustiveError then EM.COMPLAIN else EM.WARN)
	       ("binding not exhaustive" ^
	                (if noVarsF then " and contains no variables" else ""))
		       (MatchPrint.bindPrint (emptyEnv, map stripRULE rules))
        else if noVarsF
        then err EM.WARN "binding contains no variables"
                 (MatchPrint.bindPrint (emptyEnv, map stripRULE rules))
        else ();

	(matchExp, rootLvar)
    end

(*
 * handlerCompile: Entry point for compiling exception handler matches.
 *  (e.g., handle Bind => Foo).  
 *  Controls: if match redundant:
 *    Control.MC.matchRedundantWarn => match redundancy warning
 *    Control.MC.matchRedundantError => match redundancy error
 *)
fun handlerCompile (rules, lhsTy, rhsTy, err: ErrorMsg.complainer): (AS.exp * V.var) =
    let (* val _ = if !printMatch
		then (say "handlerCompile called with: "; PPL.ppMatch env rules)
                else () *)
	val (matchExp, rootVar, unused, redundant, _) =
	    matchComp (rules, lhsTy, rhsTy, NONE)
	val reportRedundancy = !MCC.matchRedundantError orelse !MCC.matchRedundantWarn
     in if redundant andalso reportRedundancy
	then err
	     (if !MCC.matchRedundantError then EM.COMPLAIN else EM.WARN)
	     "redundant patterns in match"
             (MatchPrint.matchPrint (emptyEnv, map stripRULE rules, unused))
	else ();
	(matchExp, rootVar)
    end

(*
 * matchCompile: Entry point for compiling matches induced by function expressions
 *  (and thus case expression, if-then-else expressions, while expressions
 *  and fun declarations) (e.g., fn (x::y) => ([x],y)).
 *  Controls: if match redundant
 *    Control.MC.matchRedundantWarn => match redundancy warning
 *    Control.MC.matchRedundantError => match redundancy error
 *  If match is nonexhaustive
 *    Control.MC.matchExhaustive => match nonexhaustive warning
 *)
fun matchCompile (rules, lhsTy, rhsTy, err: ErrorMsg.complainer): (AS.exp * V.var) =
    let (* val _ = if !printMatch
		then (say "matchCompile called with: "; PPL.ppMatch emptyEnv rules)
		else () *)
	val matchExn = EU.getMatchExn ()
	val (matchExp, rootVar, unused, redundant, nonexhaustive) =
            matchComp (rules, lhsTy, rhsTy, SOME matchExn)

	val nonexhaustiveF =
	    nonexhaustive andalso
	    (!MCC.matchNonExhaustiveError orelse !MCC.matchNonExhaustiveWarn)
	val redundantF =
	    redundant andalso
	    (!MCC.matchRedundantError orelse !MCC.matchRedundantWarn)
     in case (nonexhaustiveF,redundantF)
	  of (true, true) =>
             err (if !MCC.matchRedundantError orelse !MCC.matchNonExhaustiveError
		  then EM.COMPLAIN else EM.WARN)
	        "match redundant and nonexhaustive"
	        (MatchPrint.matchPrint (emptyEnv, map stripRULE rules, unused))
           | (true, false) =>
             err (if !MCC.matchNonExhaustiveError then EM.COMPLAIN else EM.WARN)
                 "match nonexhaustive"
		 (MatchPrint.matchPrint (emptyEnv, map stripRULE rules, unused))
           | (false, true) =>
              err (if !MCC.matchRedundantError then EM.COMPLAIN else EM.WARN)
	          "match redundant" (MatchPrint.matchPrint (emptyEnv, map stripRULE rules, unused))
           | _ => ();

	(matchExp, rootVar)
    end

end (* top local *)
end (* structure MatchComp *)
