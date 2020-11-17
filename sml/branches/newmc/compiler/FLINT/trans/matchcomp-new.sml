(* matchcomp.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature MATCH_COMP =
sig

  type toTcLt = (Types.ty -> PLambdaType.tyc) * (Types.ty -> PLambdaType.lty)

  type genintinfswitch =
       PLambda.lexp * (IntInf.int * PLambda.lexp) list * PLambda.lexp
       -> PLambda.lexp

  val bindCompile :
        StaticEnv.staticEnv * (Absyn.pat * PLambda.lexp) list
          * (PLambda.lexp -> PLambda.lexp) * LambdaVar.lvar * toTcLt
          * ErrorMsg.complainer
	  * genintinfswitch
	-> PLambda.lexp

  val matchCompile :
        StaticEnv.staticEnv * (Absyn.pat * PLambda.lexp) list
          * (PLambda.lexp -> PLambda.lexp) * LambdaVar.lvar * toTcLt
          * ErrorMsg.complainer
	  * genintinfswitch
	-> PLambda.lexp

  val handCompile :
        StaticEnv.staticEnv * (Absyn.pat * PLambda.lexp) list
          * (PLambda.lexp -> PLambda.lexp) * LambdaVar.lvar * toTcLt
          * ErrorMsg.complainer * genintinfswitch
	-> PLambda.lexp

end (* signature MATCH_COMP *)


structure MatchComp : MATCH_COMP =
struct

local structure DA = Access
      structure BT = BasicTypes
      structure LT = PLambdaType
      structure TU = TypesUtil
      structure PO = Primop
      structure MP = PPLexp
      structure EM = ErrorMsg
      structure TP = Types
      structure LN = LiteralToNum
      structure PP = PrettyPrint

      open VarCon Types
      open Absyn PLambda
      open PrettyPrint
      open MCCommon

in

(* utility functions for managing rule lists (type rules) *)
val intersect=SortedList.intersect
val union = SortedList.merge
val setDifference = SortedList.difference
fun member(i,set) = SortedList.member set i

val debugging = Control.MC.debugging
fun bug s = EM.impossible ("MatchComp: " ^ s)
fun say msg = (Control.Print.say msg; Control.Print.flush ())
fun debugsay msg =
    if !debugging then say msg else ()
val pd = Control.Print.printDepth
fun ppLexp le =
    PP.with_default_pp(fn ppstrm => MP.ppLexp (!pd) ppstrm le)
fun ppDectree dt =
    PP.with_default_pp(fn ppstrm => PPMatchComp.ppDectree (!pd) ppstrm dt)

type toTcLt = (ty -> LT.tyc) * (ty -> LT.lty)

type genintinfswitch =
     PLambda.lexp * (IntInf.int * PLambda.lexp) list * PLambda.lexp
     -> PLambda.lexp


local open PrintUtil
      val printDepth = Control.Print.printDepth
in

fun matchPrint (env,rules,unused) ppstrm =
  let fun matchPrint' ([],_,_) = ()
        | matchPrint' ([(pat,_)],_,_) = () (* never print last rule *)
        | matchPrint' ((pat,_)::more,[],_) =
           (PP.string ppstrm "        ";
            PPAbsyn.ppPat env ppstrm (pat,!printDepth);
            PP.string ppstrm " => ...";
            PP.newline ppstrm;
            matchPrint' (more,[],0))
        | matchPrint' ((pat,_)::more,(taglist as (tag::tags)),i) =
           if i = tag then
            (PP.string ppstrm "  -->   ";
             PPAbsyn.ppPat env ppstrm (pat,!printDepth);
             PP.string ppstrm " => ...";
             PP.newline ppstrm;
             matchPrint'(more,tags,i+1))
           else
            (PP.string ppstrm "        ";
             PPAbsyn.ppPat env ppstrm (pat,!printDepth);
             PP.string ppstrm " => ...";
             PP.newline ppstrm;
             matchPrint'(more,taglist,i+1))
   in PP.newline ppstrm;
      PP.openHVBox ppstrm (PP.Rel 0);
      matchPrint'(rules,unused,0);
      PP.closeBox ppstrm
  end

fun bindPrint (env,(pat,_)::_) ppstrm =
      (PP.newline ppstrm; PP.string ppstrm "        ";
       PPAbsyn.ppPat env ppstrm (pat,!printDepth);
       PP.string ppstrm " = ...")
  | bindPrint _ _ = bug "bindPrint -- unexpected args"

end (* local printutil *)

(* rulesUsed : dectree -> rules
 *  returns all rules used in the dectree, maintaining ordering
 *  (because union operation does) *)
fun rulesUsed (RHS n) = [n]
  | rulesUsed (BIND(_, dt)) = rulesUsed dt
  | rulesUsed (CASETEST(_, _, cases, NONE)) =
      foldr (fn((_,a), b) => union(rulesUsed a, b)) nil cases
  | rulesUsed (CASETEST(_, _, cases, SOME dt)) =
      foldr (fn((_,a), b) => union(rulesUsed a, b)) (rulesUsed dt) cases

(* fixupUnused : rules * matchRepsTy -> rules *)
(* this code is buggy - the elements of mr aren't what it thinks they are *)
fun fixupUnused (unused: ruleset, mr: matchRepsTy) =
    let fun fixup (nil, _, _, _, out) = out
	  | fixup (unused, (nil, _)::rest, n, m, out) =
	    fixup (unused, rest, n, m + 1, out)
	  | fixup (unused::urest, (rule::rules, x)::mrest, n, m, nil) =
	    if unused = n then
		fixup(urest, (rules, x)::mrest, n + 1, m, [m])
	    else
		fixup(unused::urest, (rules, x)::mrest, n + 1, m, nil)
	  | fixup (unused::urest, (rule::rules, z)::mrest, n, m, x::y) =
	    if unused = n then
		(if m <> x then
		     fixup(urest, (rules, z)::mrest, n + 1, m, m::x::y)
		 else fixup(urest, (rules, z)::mrest, n + 1, m, x::y))
	    else fixup(unused::urest, (rules, z)::mrest, n + 1, m, x::y)
	  | fixup _ = bug "fixup - unexpected arg"
    in rev(fixup(unused, mr, 0, 0, nil))
    end

(* redundant : ruleset * ruleno -> bool
 *  true if rules contains a member not equal to ruleno
 *  i.e. false only if rules = [ruleno]  ??? looks bogus *)
fun redundant (nil, n: int) = false
  | redundant (a::b, n) = a <> n orelse redundant (b, n)

fun complement(n, m, a::b) =
      if n < a then n::(complement(n + 1, m, a::b))
      else complement(n + 1, m, b)
  | complement(n, m, nil) =
      if n < m then n::(complement(n + 1, m, nil)) else nil

(* pass1cases : (pcon * dectree) list * pathSet * pathSet option * matchRepTy * path
 *              -> (pcon * dectree) list * pathSet *)
fun pass1cases ((pcon,subtree)::rest, envin, SOME envout, rhs, path) =
        let val (subtree', myEnvout) = pass1(subtree, envin, rhs)
            val (mustBindHere, otherBindings) =
                  dividePathset(pathDepends(DELTAPATH(pcon,path)),myEnvout)
            val envoutSoFar = intersectPathsets(envout, otherBindings)
            val (rest', envout') =
                  pass1cases(rest, envin, SOME envoutSoFar, rhs, path)
            val iBind2 = differencePathsets(otherBindings, envout')
            val subtree'' =
                  wrapBindings(unitePathsets(mustBindHere, iBind2), subtree')
         in ((pcon,subtree'')::rest', envout')
        end
  | pass1cases ((pcon,subtree)::rest, envin, NONE, rhs, path) =
        let val (subtree', myEnvout) = pass1(subtree, envin, rhs)
            val (mustBindHere, otherBindings) =
                  dividePathset(pathDepends(DELTAPATH(pcon,path)),myEnvout)
            val (rest', envout') =
                  pass1cases(rest, envin, SOME otherBindings, rhs, path)
            val iBind2 = differencePathsets(otherBindings, envout')
            val subtree'' =
                  wrapBindings(unitePathsets(mustBindHere, iBind2), subtree')
         in ((pcon,subtree'')::rest', envout')
        end
  | pass1cases (nil, envin, SOME envout, rhs, path) =
        (nil, unitePathsets(envin, envout))
  | pass1cases (nil, envin, NONE, rhs, path) = bug "pass1cases - unexpected arg"

(* pass1 : dectree * pathSet * matchRepTy -> dectree * pathSet *)
and pass1(RHS n, envin, rhs) = (RHS n, rhsbindings(n, rhs))
  | pass1(CASETEST(path, sign, cases, NONE), envin, rhs) =
        let val (cases', envout') =
              pass1cases(cases, unitePathsets(envin, subPaths path),
                         NONE, rhs, path)
         in (CASETEST(path, sign, cases', NONE), envout')
        end
  | pass1(CASETEST(path, sign, cases, SOME subtree), envin, rhs) =
        let val newenv = unitePathsets(envin, subPaths path)
            val (subtree', subEnvout) = pass1(subtree, newenv, rhs)
            val (cases', envout') =
              pass1cases(cases, newenv, SOME subEnvout, rhs, path)
            val subbindings = differencePathsets(subEnvout, envout')
            val subtree'' = wrapBindings(subbindings, subtree')
         in (CASETEST(path, sign, cases', SOME subtree''), envout')
        end
  | pass1 _ = bug "pass1 - unexpected arg"


(* generate : dectree * matchRepTy * lvar * (toTycTy * toLtyTy) * giisTy
 *            -> codeTy
 * Given a decision tree for a match, a matchRep list and the lvar
 * bound to the value to be matched, produce code for the match.
 *)
fun generate (dt, matchRep, rootVar, (toTyc, toLty), giis) =
  let val (subtree, envout) = pass1(dt, [(0, [ROOTPATH])], matchRep)
      fun mkDcon (DATACON {name, rep, typ, ...}) =
            (name, rep, toDconLty toLty typ)
      fun genpath (PIPATH(n, path), env) =
            SELECT(n, VAR(lookupPath(path, env)))
        | genpath (p as DELTAPATH(pcon, path), env) =
            VAR(lookupPath(p, env))
        | genpath (VPIPATH(n, t, path), env) =
	    let val tc = toTyc t
		val lt_sub =
                    let val x = LT.ltc_vector (LT.ltc_tv 0)
                    in LT.ltc_poly([LT.tkc_mono],
				   [LT.ltc_parrow(LT.ltc_tuple [x, LT.ltc_int], LT.ltc_tv 0)])
                    end
	    in APP(PRIM(PO.SUBSCRIPTV, lt_sub, [tc]),
		   RECORD[ VAR(lookupPath(path, env)),
		           INT{ival = IntInf.fromInt n, ty = Target.defaultIntSz} ])
            end
        | genpath (VLENPATH (t, path), env) =
            let val tc = toTyc t
                val lt_len = LT.ltc_poly([LT.tkc_mono],
                                 [LT.ltc_parrow(LT.ltc_tv 0, LT.ltc_int)])
                val argtc = LT.tcc_vector tc
             in APP(PRIM(PO.LENGTH, lt_len, [argtc]),
                    VAR(lookupPath(path, env)))
            end
        | genpath (ROOTPATH, env) = VAR(lookupPath(ROOTPATH, env))

      (* moved to trans/translate.sml for new match compiler *)
      fun genswitch (sv, sign, [(DATAcon((_, DA.REF, lt), ts, x), e)], NONE) =
            LET(x, APP (PRIM (Primop.DEREF, LT.lt_swap lt, ts), sv), e)
        | genswitch (sv, sign, [(DATAcon((_, DA.SUSP(SOME(_, DA.LVAR f)), lt),
                                        ts, x), e)], NONE) =
            let val v = mkv()
             in LET(x, LET(v, TAPP(VAR f, ts), APP(VAR v, sv)), e)
            end
	| genswitch (sv, sign, cases as ((INTcon{ty=0, ...}, _) :: _), default) =
	    let fun strip (INTcon{ty=0, ival}, e) = (ival, e)
		  | strip _ = bug "genswitch - INTINFcon"
	    in
		case default of
		    NONE => bug "getswitch - no default in switch on IntInf"
		  | SOME d => giis (sv, map strip cases, d)
	    end
        | genswitch x = SWITCH x

      fun pass2rhs (n, env, matchRep) =
        (case List.nth(matchRep, n)
          of (_, [path], fname) => APP(VAR fname, VAR(lookupPath(path, env)))
           | (_, paths, fname) =>
               APP(VAR fname,
                 RECORD (map (fn path => VAR(lookupPath(path, env))) paths)))

      fun pass2 (BIND(DELTAPATH _, subtree), env, rhs) =
            pass2(subtree, env, rhs)
            (** we no longer generate explicit DECON anymore, instead,
                we add a binding at each switch case. *)
        | pass2 (BIND(path, subtree), env, rhs) =
            let val newvar = mkv()
                val subcode = pass2(subtree, (path, newvar)::env, rhs)
             in LET(newvar, genpath(path, env), subcode)
            end
        | pass2 (CASETEST(path, sign, [], NONE), _, _) =
            bug "pass2 - empty cases"
        | pass2 (CASETEST(path, sign, [], SOME subtree), env, rhs) =
            pass2(subtree,env,rhs)
        | pass2 (CASETEST(path, sign, cases, dft), env, rhs) =
            let val switchVar = VAR(lookupPath(path, env))
		val switchCases = pass2cases(path,cases,env,rhs)
		val switchDefault = Option.map (fn subtree => pass2(subtree,env,rhs)) dft
             in genswitch(switchVar, sign, switchCases, switchDefault)
            end
        | pass2 (RHS n, env, rhs) = pass2rhs(n, env, rhs)

      and pass2cases (path, nil, env, rhs) = nil
        | pass2cases(path, (pcon,subtree)::rest, env, rhs) =
            let (** always implicitly bind a new variable at each branch. *)
                val (ncon, nenv) = pconToCon(pcon, path, env)
                val res = (ncon, pass2(subtree, nenv, rhs))
             in res::(pass2cases(path, rest, env, rhs))
            end

      (* pconToCon : pcon * path * (path * lvar) list -> lexp * (path * lvar) list *)
      and pconToCon (pcon, path, env) =
	  (case pcon
	     of DATApcon (dc, ts) =>
		  let val newvar = mkv()
		      val nts = map (toTyc o TP.VARty) ts
		      val nenv = (DELTAPATH(pcon, path), newvar)::env
		   in (DATAcon (mkDcon dc, nts, newvar), nenv)
		  end
	      | VLENpcon(i, t) => (VLENcon i, env)
	      | INTpcon i => (INTcon i, env)
	      | WORDpcon w => (WORDcon w, env)
	      | STRINGpcon s => (STRINGcon s, env)
	    (* end case *))

   in case wrapBindings(envout, subtree)
       of BIND(ROOTPATH, subtree') =>
            pass2(subtree', [(ROOTPATH, rootVar)], matchRep)
        | _ => pass2(subtree, [], matchRep)
  end

(* doMatchCompile : (pat * exp) list * (lexp -> lexp?) * lvar * toLcLtTy * errTy * giisTy
                    -> lexp * ruleset * bool * bool ? *)
fun doMatchCompile(absyn_rules, finish, rootvar, toTcLt as (_, toLty), err, giis) =
  let val lastRule = length absyn_rules - 1
      val matchReps = map (preProcessPat toLty) absyn_rules
      val (matchRep,rhsRep) =
          foldr (fn ((a,b),(c,d)) => (a@c,b::d)) ([], []) matchReps
      val allRules = List.tabulate(length matchRep, fn x => x);
          (* length matchRep can be > length absyn_rules if OR pats are expanded
           * (by orExpand in preProcessPat) *)
      val flattened = flattenAndors(makeAndor(matchRep,err),allRules)
      val ready = fireConstraint(ROOTPATH,flattened,nil,nil)
      val dt = genDecisionTree(ready,allRules)
      val _ = PPMatchComp.debugPrint debugging
	       ("#dectree#", fn ppstrm => fn dt => PPMatchComp.ppDectree (!pd) ppstrm dt, dt)
      val numRules = length matchRep
      val rawUnusedRules = complement(0,numRules,rulesUsed dt)
      val unusedRules = fixupUnused(rawUnusedRules,matchReps)
      val exhaustive = member(lastRule,unusedRules)
      val redundantF = redundant(unusedRules, lastRule)

      fun g((fname, fbody), body) = LET(fname, fbody, body)
      val code = foldr g (generate(dt, matchRep, rootvar, toTcLt,giis)) rhsRep

   in (finish(code), unusedRules, redundantF, exhaustive)
  end

(* type as_match = (Absyn.pat * Absyn.exp) list *)

(* noVarsIn : as_match -> bool
 * Test pat, the guard pattern of the first match rule of a match,
 * for the occurence of variables (including layering variables)
 * or wildcards.  Return true if any are present, false otherwise.
 *)
fun noVarsIn ((pat,_)::_) =
      let fun var WILDpat = true (* might want to flag this *)
            | var (VARpat _) = true
            | var (LAYEREDpat _) = true
            | var (CONSTRAINTpat(p,_)) = var p
            | var (APPpat(_,_,p)) = var p
            | var (RECORDpat{fields,...}) = List.exists (var o #2) fields
            | var (VECTORpat(pats,_)) = List.exists var pats
            | var (ORpat (pat1,pat2)) = var pat1 orelse var pat2
            | var _ = false
       in not(var pat)
      end
  | noVarsIn _ = bug "noVarsIn - unexpected arg"


(*
 * The three entry points for the match compiler.
 *
 * They take as arguments an environment (env); a match represented
 * as a list of pattern--lambda expression pairs (match); and a
 * function to use in printing warning messages (warn).
 *
 * env and warn are only used in the printing of diagnostic information.
 *
 * If the control flag Control.MC.printArgs is set, they print match.
 *
 * They call doMatchCompile to actually compile match.
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

local open Control.MC (* make various control flags visible *)
in

(*
 * bindCompile: Entry point for compiling matches induced by val declarations
 * (e.g., val listHead::listTail = list).
 * The match (rules) is a two  element list. The first rule corresponds
 * to the let binding itself, while the second is a default rule
 * (usually "_ => raise Bind") added, e.g. in the function mkVBs in
 * translate.sml, or by applying ElabUtil.completeMatch.
 * Thus the match itself will always be exhaustive, but the case where the
 * let binding per se is nonexhaustive will still be detected by doMatchCompile
 * (see the comment above), and if the control flag Control.MC.bindNonExhaustiveWarn
 * is set then a nonexhaustive binding warning is printed. If the control
 * flag Control.MC.bindNoVariableWarn is set, and the first pattern
 * (i.e., the only non-dummy pattern) of match contains no variables or
 * wildcards, a warning is printed. Arguably, a pattern containing no
 * variables, but one or more wildcards, should also trigger a warning,
 * but this would cause warnings on constructions like
 * val _ = <exp>  and  val _:<ty> = <exp>.
 *)
fun bindCompile (env, rules, finish, rootv, toTcLt, err, giis) =
  let val _ =
        if !printArgs then (say "BC called with:"; MP.ppMatch env rules)
        else ()
      val (code, _, _, exhaustive) =
        doMatchCompile(rules, finish, rootv, toTcLt, err, giis)

      val nonexhaustiveF =
	  not exhaustive andalso
	  (!bindNonExhaustiveWarn orelse !bindNonExhaustiveError)
      val noVarsF = !bindNoVariableWarn andalso noVarsIn rules

   in if nonexhaustiveF
      then err (if !bindNonExhaustiveError then EM.COMPLAIN else EM.WARN)
	       ("binding not exhaustive" ^
	                (if noVarsF then " and contains no variables" else ""))
		       (bindPrint(env,rules))
      else if noVarsF
           then err EM.WARN "binding contains no variables"
                    (bindPrint(env,rules))
           else ();

      if !printRet then
        (say "MC:  returns with\n"; ppLexp code)
      else ();
      code
  end

(*
 * Entry point for compiling matches induced by exception handlers.
 * (e.g., handle Bind => Foo).  If the control flag
 *  Control.MC.matchRedundantWarn is set, and match is redundant,
 *  a warning is printed.  If Control.MC.matchRedundantError is also
 *  set, the warning is promoted to an error message.
 *)
fun handCompile (env, rules, finish, rootv, toTcLt, err, giis) =
  let val _ =
        if !printArgs then (say "HC called with: "; MP.ppMatch env rules)
        else ()
      val (code, unused, redundant, _) =
        doMatchCompile(rules, finish, rootv, toTcLt, err, giis)
      val  redundantF= !matchRedundantWarn andalso redundant

   in if redundantF
      then err
	     (if !matchRedundantError then EM.COMPLAIN else EM.WARN)
	     "redundant patterns in match"
             (matchPrint(env,rules,unused))
      else ();

      if !printRet
      then (say "MC:  returns with\n"; ppLexp code)
      else ();
      code
  end

(*
 * Entry point for compiling matches induced by function expressions
 * (and thus case expression, if-then-else expressions, while expressions
 * and fun declarations) (e.g., fn (x::y) => ([x],y)). If the control flag
 * Control.MC.matchRedundantWarn is set, and match is redundant, a warning
 * is printed; if Control.MC.matchRedundantError is also set, the warning
 * is promoted to an error. If the control flag Control.MC.matchExhaustive
 * is set, and match is nonexhaustive, a warning is printed.
 *)
fun matchCompile (env, rules, finish, rootv, toTcLt, err, giis) =
  let val _ =
        if !printArgs then (say "MC called with: "; MP.ppMatch env rules)
        else ()
      val (code, unused, redundant, exhaustive) =
        doMatchCompile(rules, finish, rootv, toTcLt, err, giis)

      val nonexhaustiveF =
	  not exhaustive andalso
	  (!matchNonExhaustiveError orelse !matchNonExhaustiveWarn)
      val redundantF =
	  redundant andalso (!matchRedundantError orelse !matchRedundantWarn)
   in case (nonexhaustiveF,redundantF)
       of (true, true) =>
            err (if !matchRedundantError orelse !matchNonExhaustiveError
		     then EM.COMPLAIN else EM.WARN)
	        "match redundant and nonexhaustive"
	        (matchPrint(env, rules, unused))

        | (true, false) =>
            err (if !matchNonExhaustiveError then EM.COMPLAIN else EM.WARN)
                "match nonexhaustive"
		(matchPrint(env, rules, unused))

        | (false, true) =>
            err (if !matchRedundantError then EM.COMPLAIN else EM.WARN)
	      "match redundant" (matchPrint(env, rules, unused))

        | _ => ();

      if (!printRet)
      then (say "MatchComp:  returns with\n"; ppLexp code) else ();
      code
  end


val matchCompile =
  Stats.doPhase(Stats.makePhase "Compiler 045 matchcomp") matchCompile

end (* local Control.MC *)

end (* topleve local *)
end (* structure MatchComp *)

