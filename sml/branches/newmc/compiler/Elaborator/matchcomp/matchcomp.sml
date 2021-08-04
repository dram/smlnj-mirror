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
	-> PLambda.lexp * LambdaVar.lvar option

  val matchCompile :
        StaticEnv.staticEnv
	* (Absyn.pat * PLambda.lexp) list
	* failInfo
	* toTcLt
	* ErrorMsg.complainer
	* giisTy
	-> PLambda.lexp * LambdaVar.lvar option

  val handlerCompile :
	StaticEnv.staticEnv
	* (Absyn.pat * PLambda.lexp) list
	* failInfo
	* toTcLt
	* ErrorMsg.complainer
	* giisTy
	-> PLambda.lexp * LambdaVar.lvar option

end (* signature MATCH_COMP *)


structure MatchComp : MATCH_COMP =
struct

local
  structure T = Types
  structure AU = AbsynUtil
  structure PL = PLambda
  structure LT = PLambdaType
  structure LV = LambdaVar
  structure PPL = PPLexp
  structure EM = ErrorMsg
  structure PP = PrettyPrint
  structure PU = PPUtil
  structure IM = Andor.IM
  structure DT = DecisionTree
  structure ST = MCStats

  open MCCommon Control.MC (* match compiler control flags *)
  structure RS = RuleSet
		     
  val debugging = Control.MC.debugging
  val stats = Control.MC.stats
  val timings = Control.MC.timings
  val printProtoAndor = Control.MC.printProtoAndor
  val printAndor = Control.MC.printAndor
  val printDectree = Control.MC.printDectree
  val printPvarMap = Control.MC.printPvarMap
  val printCode = Control.MC.printCode
		       
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

  val ppLexp = PPL.ppLexp 100

  fun ppDecTree dt =
      PP.with_default_pp (fn ppstrm => MCPrint.ppDectree ppstrm dt)

in

type toTcLt = (T.ty -> LT.tyc) * (T.ty -> LT.lty)

type giisTy = (* type of int inf switch generator *)
     PL.lexp * (IntInf.int * PL.lexp) list * PL.lexp -> PL.lexp

(* --------------------------------------------------------------------------- *)
(* matchComp: Main match compiler driver function *)

type failInfo = PL.lexp option * T.ty * string

(* matchComp : (pat * lexp) list * failInfo * toLcLt * giisTy
                -> lexp * lvar * ruleno list * bool * bool *)
fun matchComp (hybridMatch, fail: failInfo, toTcLt as (_, toLty), giis) =
let fun timeIt x = TimeIt.timeIt (!timings) x
    val (_,_,location) = fail
    val _ = MCPrint.debugPrint debugging
              ("matchComp: hmatch = \n", MCPrint.ppHMatch, hybridMatch)

    val (numExpandedRules, expandedPats, rhsFunBinders, ruleMap) =
	Preprocessing.expandPats toLty hybridMatch

    (* RS.set of rulenos after or-expansion. If there are or-patterns
     * in the match, numRulesExpanded > length hybridMatch. *)
    val allRules: RS.set = RS.fromList (List.tabulate(numExpandedRules, fn x => x));

    val _ = ST.initialLvar := LV.nextLvar ()  (* for counting how many lvars are generated *)

    val protoAndor: protoAndor = (* ProtoAndor.makeProtoAndor expandedPats *)
        timeIt ("makeProtoAndor", location, ProtoAndor.makeProtoAndor, expandedPats)

    val _ = MCPrint.debugPrint printProtoAndor
	      ("** matchComp: protoAndor = ", MCPrint.ppProtoAndor, protoAndor)

    val andor: andor =
	(* Andor.makeAndor (protoAndor, allRules) *)
	timeIt ("Andor.makeAndor", location, Andor.makeAndor, (protoAndor, allRules))
    val _ = if !stats then ST.collectAndorStats andor else ()

    val _ = MCPrint.debugPrint printAndor ("** matchComp: andor", MCPrint.ppAndor, andor)

    val dectree = (* DecisionTree.makeDectree (andor, allRules) *)
	timeIt ("makeDectree", location, DT.makeDectree, (andor, allRules))
    val _ = ST.collectDectreeStats dectree  (* must collect dectree stats for rulesUsed and numFAIL *)

    val _ = MCPrint.debugPrint printDectree ("** matchComp: dectree = ", MCPrint.ppDectree, dectree)

    (* checking exhaustiveness and redundancy of rules *)
    (* It may be that there are unused _ramified_ rules, but all original rules are used!? Example? *)
    val SOME{rulesUsed, numFAIL, ...} = !ST.dectreeStats
    val unusedRules : ruleset = RS.difference (allRules, rulesUsed)  (* expanded rules *)
    val unusedOriginalRules : ruleset = RS.map (#3 o ruleMap) unusedRules
        (* unusedRules translated back to corresponding original rule numbers. WRONG??? *)
    val redundant = not (RS.isEmpty unusedRules)
    val nonexhaustive = numFAIL > 0  (* any FAIL nodes => nonexhaustive rules *)

    (* generating the "raw" lexp for the match *)

    val (matchLexp, rootMvarOp) = (* Generate.generate (andor, dectree, ruleMap,
                                                      fail, toTcLt, giis) *)
        timeIt ("Generate.generate", location, Generate.generate,
		(andor, dectree, ruleMap, allRules, fail, toTcLt, giis))

    (* wrapping let-bindings of abstracted right-hand-sides around match code,
     * (corresponds to "genprelude" in newmc) *)

    val code: PL.lexp = foldl (fn (fbinder, body) => fbinder body) matchLexp
			      rhsFunBinders

    val _ = MCPrint.debugPrint printCode ("** matchComp: code = ", ppLexp, code)

    val _ = ST.finalLvar := LV.nextLvar ()
				    
    val _ = if !stats then ST.reportStats () else ()

    (* rudundant <=> not (null unusedOriginalRules) <=> not (null unusedExpandedRules) *)
 in (code, rootMvarOp, RS.toList unusedOriginalRules, redundant, nonexhaustive)
end (* fun matchComp *)

(* --------------------------------------------------------------------------- *)
(* Match Compiler entry points *)

(*
 * The three entry points for the match compiler are bindCompile, handleCompile,
 * and matchCompile.
 *
 * They take as arguments an environment (env); a match represented
 * as a list of pattern--lambda expression pairs (hybrid rules); a
 * function err to use in printing warning messages (err), etc.
 *
 * env and err are only used in the printing of diagnostic information.
 *
 * If the control flag Control.MC.printArgs is set, they print the match.
 *
 * They call matchComp to actually compile the match.
 * This returns a 4-tuple (code, unused, redundant, exhaustive).
 * code is a PLambda.lexp expression implementing the match.
 *   - unused is a list of the indices of the unused rules.
 *   - redundant and exhaustive are boolean flags which are set if
 *     match is redundant or exhaustive respectively.
 *
 * They print warning messages as appropriate, as described below.
 * If the control flag Control.MC.printCode is set, they print the match code.
 *)

(* bindCompile: Entry point for compiling matches induced by val declarations
 *  (e.g., val x::xs = list).
 *  The match consists of a single rule that corresponds to the let binding itself.
 *  If the control flag Control.MC.bindNonExhaustiveWarn
 *  is set then a nonexhaustive binding warning is printed. If the control
 *  flag Control.MC.bindNoVariableWarn is set, and pattern contains no variables or
 *  wildcards, a warning is printed. Arguably, a pattern containing no
 *  variables, but one or more wildcards, should also trigger a warning,
 *  but this would cause warnings on constructions like
 *  val _ = <exp>  and  val _:<ty> = <exp>.
 *)
fun bindCompile (env, rules, fail, toTcLt, err, giis): (PL.lexp * LV.lvar option) =
    let val _ = if !printMatch
		then (say "BC called with:"; PPL.ppMatch env rules)
		else ()
	val (code, rootMvarOp, _, _, nonexhaustive) = matchComp (rules, fail, toTcLt, giis)

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

	(code, rootMvarOp)
    end

(* handlerCompile: Entry point for compiling matches induced by exception handlers.
 *  (e.g., handle Bind => Foo).  If the control flag
 *  Control.MC.matchRedundantWarn is set, and match is redundant,
 *  a warning is printed.  If Control.MC.matchRedundantError is also
 *  set, the warning is promoted to an error message.
 *)
fun handlerCompile (env, rules, fail, toTcLt, err, giis): (PL.lexp * LV.lvar option) =
    let val _ = if !printMatch then (say "HC called with: "; PPL.ppMatch env rules)
                else ()
	val (code, rootMvarOp, unused, redundant, _) = matchComp (rules, fail, toTcLt, giis)
	val redundantF= !matchRedundantWarn andalso redundant

     in if redundantF
	then err
	     (if !matchRedundantError then EM.COMPLAIN else EM.WARN)
	     "redundant patterns in match"
             (MatchPrint.matchPrint(env,rules,unused))
	else ();
	(code, rootMvarOp)
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
fun matchCompile (env, rules, fail, toTcLt, err, giis): (PL.lexp * LV.lvar option) =
    let val _ = if !printMatch then (say "MC called with: "; PPL.ppMatch env rules)
		else ()
	val (code, rootMvarOp, unused, redundant, nonexhaustive) =
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

      (code, rootMvarOp)
  end

val matchCompile =
    Stats.doPhase(Stats.makePhase "Compiler 045 matchcomp") matchCompile

end (* topleve local *)
end (* structure MatchComp *)

