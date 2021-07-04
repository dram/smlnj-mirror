(* FLINT/trans/matchcomp.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature MATCH_COMP =
sig

  (* toTcLt - type of the pair of type translation functions (to tyc and lty, resp.) *)
  type toTcLt = (Types.ty -> PLambdaType.tyc) * (Types.ty -> PLambdaType.lty)

  (* giisTy - type of genintinfswitch function *)
  type giisTy =
       PLambda.lexp * (IntInf.int * PLambda.lexp) list * PLambda.lexp
       -> PLambda.lexp

  (* failInfo - package of info needed to generate raise experssions for match failures *)
  type failInfo =
         PLambda.lexp option                 (* match failure exception lexp (SOME Bind/Match, NONE => uncaught) *)
	 * Types.ty                          (* rhs (return) type of the match *)
	 * string                            (* match region location string *)

  val bindCompile :
        StaticEnv.staticEnv                  (* static environment, only needed for printing error msgs *)
	* (Absyn.pat * PLambda.lexp) list    (* hybrid rules *)
	* failInfo                           (* for generating match failure raise code *)
	* toTcLt                             (* type translator functions *)
	* ErrorMsg.complainer                (* err function - for redundant and nonexhaustive errors *)
	* giisTy                             (* intinf switch generator *)
	-> PLambda.lexp * LambdaVar.lvar

  val matchCompile :
        StaticEnv.staticEnv
	* (Absyn.pat * PLambda.lexp) list
	* failInfo
	* toTcLt
	* ErrorMsg.complainer
	* giisTy
	-> PLambda.lexp * LambdaVar.lvar

  val handlerCompile :
	StaticEnv.staticEnv
	* (Absyn.pat * PLambda.lexp) list
	* failInfo
	* toTcLt
	* ErrorMsg.complainer
	* giisTy
	-> PLambda.lexp * LambdaVar.lvar

end (* signature MATCH_COMP *)


structure MatchComp : MATCH_COMP =
struct

local
  structure DA = Access
  structure T = Types
  structure BT = BasicTypes
  structure V = VarCon
  structure AS = Absyn
  structure AU = AbsynUtil
  structure PL = PLambda
  structure LT = PLambdaType
  structure TU = TypesUtil
  structure PO = Primop
  structure LV = LambdaVar
  structure PPL = PPLexp
  structure LN = LiteralToNum
  structure EM = ErrorMsg
  structure PP = PrettyPrint
  structure PU = PPUtil
  structure PVM = Andor.PVM
  structure DT = DecisionTree

  open MCCommon
  structure RS = RuleSet
		     
  val debugging = Control.MC.debugging
  val stats = Control.MC.stats
		      
  fun bug s = EM.impossible ("MatchComp: " ^ s)
  fun say msg = (Control.Print.say msg; Control.Print.flush ())
  fun says msgs = say (concat msgs)
  fun saynl msg = (say (msg^"\n"))
  fun saysnl msgs = (saynl (concat msgs))
  fun newline () = say "\n"

  fun dbsay msg = if !debugging then say msg else ()
  fun dbsays msgs = if !debugging then says msgs else ()
  fun dbsaynl msg = if !debugging then saynl msg else ()

  val pd = Control.Print.printDepth

  fun ppLexp ppstrm le = PPL.ppLexp 100 ppstrm le

  fun ppDecTree dt =
      PP.with_default_pp (fn ppstrm => MCPrint.ppDecTree ppstrm dt)

  fun ppPvarMapEntry ppstrm (lvar, alist) =
      let fun prBind ppstrm (r, id) =
	      (PP.openHBox ppstrm;
		 PP.string ppstrm "(";
		 PP.string ppstrm (Int.toString r);
		 PP.string ppstrm ",";
		 PP.string ppstrm (Int.toString id);
		 PP.string ppstrm ")";
	       PP.closeBox ppstrm)
       in PP.openHBox ppstrm;
	  PP.string ppstrm (LV.toString lvar);
	  PU.pps ppstrm " = {";
	  PU.ppSequence ppstrm
	   {sep = (fn ppstrm => PU.pps ppstrm ","),
	    pr = prBind,
	    style = PU.INCONSISTENT}
	   alist;
	  PU.pps ppstrm "}";
	  PP.closeBox ppstrm (* openHBox *)
      end

  fun ppPvarMap ppstrm pvarmap =
      let val contents = PVM.listItemsi pvarmap
      in case contents
	  of nil => PP.string ppstrm "<< empty pvarmap >>"
	   | _ => 
	     (PP.openVBox ppstrm (PP.Abs 3);
		PU.ppvseq ppstrm 0 "" ppPvarMapEntry contents;
	      PP.closeBox ppstrm)
      end

in

type toTcLt = (T.ty -> LT.tyc) * (T.ty -> LT.lty)

type giisTy = (* type of int inf switch generator *)
     PLambda.lexp * (IntInf.int * PLambda.lexp) list * PLambda.lexp -> PLambda.lexp

val mkv = LambdaVar.mkLvar  (* use the "global" lvar generator from LambdaVar *)

(* --------------------------------------------------------------------------- *)
(* utility functions used by matchComp *)
	      

val choiceTotalThreshold = 10

fun reportStats (nodeCount: int, {rulesUsed, failures, choiceTotal, choiceDist}: DT.decTreeStats) =
    if !stats andalso choiceTotal > choiceTotalThreshold
    then (say "decTree Stats: \n";
	  saysnl ["  nodeCount =   ", Int.toString nodeCount];
	  saysnl ["  choiceTotal = ", Int.toString choiceTotal];
	  newline())
    else ()

(* --------------------------------------------------------------------------- *)
(* matchComp: Main match compiler driver function *)

type failInfo = PL.lexp option * T.ty * string

(* matchComp : (pat * lexp) list * failInfo * toLcLt * errTy * giisTy
                -> lexp * lvar * ruleno list * bool * bool *)
fun matchComp (hybridMatch, fail: failInfo, toTcLt as (_, toLty), giis) =
let fun timeIt x = TimeIt.timeIt (!stats) x
    val (_,_,location) = fail
    val _ = MCPrint.debugPrint
              ("matchComp: hmatch = \n",
	        (fn ppstrm => MCPrint.ppHMatch ppstrm hybridMatch))

    val (numExpandedRules, expandedPats, rhsFunBinders, ruleMap) =
	Preprocessing.expandPats toLty hybridMatch

    (* RS.set of rulenos after or-expansion. If there are or-patterns
     * in the match, numRulesExpanded > length hybridMatch. *)
    val allRules: RS.set = RS.fromList (List.tabulate(numExpandedRules, fn x => x));

    val simpleAndor: simpleAndor = AndorSimple.makeAndor expandedPats

    val _ = MCPrint.debugPrint
	      ("** matchComp: simpleAndor = ",
	       (fn ppstrm => MCPrint.ppSimpleAndor ppstrm simpleAndor))

    val (andor: andor, pvarmap: Andor.pvarmap, nodeCount) = (* Andor.makeAndor (simpleAndor, allRules) *)
	timeIt ("Andor.makeAndor", location, Andor.makeAndor, (simpleAndor, allRules))

    val _ = MCPrint.debugPrint
	      ("** matchComp: andor (nodeCount = " ^ Int.toString nodeCount ^ ") =",
	       (fn ppstrm => MCPrint.ppAndor ppstrm andor))

    val _ = MCPrint.debugPrint
	      ("** matchComp: pvarmap = ",
	       (fn ppstrm => ppPvarMap ppstrm pvarmap))

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

    (* generating the "raw" lexp for the match *)

    val (matchLexp, rootLvar) = (* Generate.genMatch (andor, decTree, pvarmap, ruleMap,
                                                      fail, toTcLt, giis) *)
        timeIt ("genMatch", location, Generate.genMatch,
		(andor, decTree, pvarmap, ruleMap, fail, toTcLt, giis))

    (* wrapping let-bindings of abstracted right-hand-sides around match code,
     * (corresponds to newmc "genprelude") *)

    val code: PL.lexp = foldl (fn (fbinder, body) => fbinder body) matchLexp
			      rhsFunBinders

    val _ = MCPrint.debugPrint
	      ("** matchComp: code = ",
	       (fn ppstrm => ppLexp ppstrm code))

    val _ = reportStats (nodeCount, decTreeStats)

    (* rudundant <=> not (null unusedOriginalRules) <=> not (null unusedExpandedRules) *)
 in (code, rootLvar, RS.toList unusedOriginalRules, redundant, nonexhaustive)
end

(* --------------------------------------------------------------------------- *)
(* Match Compiler entry points *)

(*
 * The three entry points for the match compiler.
 *
 * They take as arguments an environment (env); a match represented
 * as a list of pattern--lambda expression pairs (hybrid rules); a
 * function err to use in printing warning messages (warn), etc.
 *
 * env and warn are only used in the printing of diagnostic information.
 *
 * If the control flag Control.MC.printArgs is set, they print match.
 *
 * They call matchComp to actually compile match.
 * This returns a 4-tuple (code, unused, redundant, exhaustive).
 * code is lambda code that implements match.  unused
 * is a list of the indices of the unused rules.  redundant
 * and exhaustive are boolean flags which are set if
 * match is redundant or exhaustive respectively.
 *
 * They print warning messages as appropriate, as described below.
 * If the control flag Control.MC.printRet is set, they print code.
 *
 * They return code.
 *
 * They assume that match has one element for each rule of the match
 * to be compiled, in order, plus a single, additional, final element.
 * This element must have a pattern that is always matched
 * (in practice, it is either a variable or wildcard), and a
 * lambda expression that implements the appropriate behavior
 * for argument values that satisfy none of the guard patterns.
 * A pattern is exhaustive if this dummy rule is never used,
 * and is irredundant if all of the other rules are used.
 *)

(* make various match compiler control flags (from Control.MC) visible *)
val bindNonExhaustiveWarn = Control.MC.bindNonExhaustiveWarn
val bindNonExhaustiveError = Control.MC.bindNonExhaustiveError
val matchNonExhaustiveWarn = Control.MC.matchNonExhaustiveWarn
val matchNonExhaustiveError = Control.MC.matchNonExhaustiveError
val matchRedundantWarn = Control.MC.matchRedundantWarn
val matchRedundantError = Control.MC.matchRedundantError
val bindNoVariableWarn = Control.MC.bindNoVariableWarn
val printMatch = Control.MC.printMatch

(* bindCompile: Entry point for compiling matches induced by val declarations
 *  (e.g., val listHead::listTail = list).
 *  The match (rules) is a two  element list. The first rule corresponds
 *  to the let binding itself, while the second is a default rule
 *  (usually "_ => raise Bind") added, e.g. in the function mkVBs in
 *  translate.sml, or by applying ElabUtil.completeMatch.
 *  Thus the match itself will always be exhaustive, but the case where the
 *  let binding per se is nonexhaustive will still be detected by matchComp
 *  (see the comment above), and if the control flag Control.MC.bindNonExhaustiveWarn
 *  is set then a nonexhaustive binding warning is printed. If the control
 *  flag Control.MC.bindNoVariableWarn is set, and the first pattern
 *  (i.e., the only non-dummy pattern) of match contains no variables or
 *  wildcards, a warning is printed. Arguably, a pattern containing no
 *  variables, but one or more wildcards, should also trigger a warning,
 *  but this would cause warnings on constructions like
 *  val _ = <exp>  and  val _:<ty> = <exp>.
 *)
fun bindCompile (env, rules, fail, toTcLt, err, giis): (PL.lexp * LV.lvar) =
    let val _ = if !printMatch
		then (say "BC called with:"; PPL.ppMatch env rules)
		else ()
	val (code, rootLvar, _, _, nonexhaustive) = matchComp (rules, fail, toTcLt, giis)

	val nonexhaustiveF =
	    nonexhaustive andalso (!bindNonExhaustiveWarn orelse !bindNonExhaustiveError)
	val noVarsF = !bindNoVariableWarn andalso AU.noVarsInPat (#1 (hd rules))

     in if nonexhaustiveF
        then err (if !bindNonExhaustiveError then EM.COMPLAIN else EM.WARN)
	       ("binding not exhaustive" ^
	                (if noVarsF then " and contains no variables" else ""))
		       (MatchPrint.bindPrint(env,rules))
        else if noVarsF
        then err EM.WARN "binding contains no variables"
                 (MatchPrint.bindPrint(env,rules))
        else ();

	(code, rootLvar)
    end

(*
 * handlerCompile: Entry point for compiling matches induced by exception handlers.
 *  (e.g., handle Bind => Foo).  If the control flag
 *  Control.MC.matchRedundantWarn is set, and match is redundant,
 *  a warning is printed.  If Control.MC.matchRedundantError is also
 *  set, the warning is promoted to an error message.
 *)
fun handlerCompile (env, rules, fail, toTcLt, err, giis): (PL.lexp * LV.lvar) =
    let val _ = if !printMatch then (say "HC called with: "; PPL.ppMatch env rules)
                else ()
	val (code, rootLvar, unused, redundant, _) = matchComp (rules, fail, toTcLt, giis)
	val redundantF= !matchRedundantWarn andalso redundant

     in if redundantF
	then err
	     (if !matchRedundantError then EM.COMPLAIN else EM.WARN)
	     "redundant patterns in match"
             (MatchPrint.matchPrint(env,rules,unused))
	else ();
	(code, rootLvar)
    end

(*
 * matchCompile: Entry point for compiling matches induced by function expressions
 *  (and thus case expression, if-then-else expressions, while expressions
 *  and fun declarations) (e.g., fn (x::y) => ([x],y)). If the control flag
 *  Control.MC.matchRedundantWarn is set, and match is redundant, a warning
 *  is printed; if Control.MC.matchRedundantError is also set, the warning
 *  is promoted to an error. If the control flag Control.MC.matchExhaustive
 *  is set, and match is nonexhaustive, a warning is printed.
 *)
fun matchCompile (env, rules, fail, toTcLt, err, giis): (PL.lexp * LV.lvar) =
    let val _ = if !printMatch then (say "MC called with: "; PPL.ppMatch env rules)
		else ()
	val (code, rootLvar, unused, redundant, nonexhaustive) =
            matchComp (rules, fail, toTcLt, giis)

	val nonexhaustiveF =
	    nonexhaustive andalso (!matchNonExhaustiveError orelse !matchNonExhaustiveWarn)
	val redundantF =
	    redundant andalso (!matchRedundantError orelse !matchRedundantWarn)
     in case (nonexhaustiveF,redundantF)
	  of (true, true) =>
             err (if !matchRedundantError orelse !matchNonExhaustiveError
		  then EM.COMPLAIN else EM.WARN)
	        "match redundant and nonexhaustive"
	        (MatchPrint.matchPrint(env, rules, unused))
           | (true, false) =>
             err (if !matchNonExhaustiveError then EM.COMPLAIN else EM.WARN)
                 "match nonexhaustive"
		 (MatchPrint.matchPrint(env, rules, unused))
           | (false, true) =>
              err (if !matchRedundantError then EM.COMPLAIN else EM.WARN)
	          "match redundant" (MatchPrint.matchPrint(env, rules, unused))
           | _ => ();

      (code, rootLvar)
  end

val matchCompile =
    Stats.doPhase(Stats.makePhase "Compiler 045 matchcomp") matchCompile

end (* topleve local *)
end (* structure MatchComp *)

