(* sml-fun-output.sml
 *
 * COPYRIGHT (c) 2005 
 * John Reppy (http://www.cs.uchicago.edu/~jhr)
 * Aaron Turon (adrassi@gmail.com)
 * All rights reserved.
 *
 * Code generation for SML, using control-flow
 *)

structure SMLFunOutput : OUTPUT = 
  struct

    structure RE = RegExp
    structure Sym = RE.Sym
    structure SIS = RegExp.SymSet
    structure LO = LexOutputSpec

    datatype ml_exp = datatype ML.ml_exp
    datatype ml_pat = datatype ML.ml_pat

    val inp = "inp"
    val inpVar = ML_Var inp

    fun idOf (LO.State {id, ...}) = id
    fun nameOf' i = "yyQ" ^ (Int.toString i)
    fun nameOf s = nameOf' (idOf s)
    fun actName i = "yyAction" ^ (Int.toString i)

  (* simple heuristic to avoid computing unused values *)
    local 
      val has = String.isSubstring
    in
    val hasyytext   = has "yytext"
    val hasREJECT   = has "REJECT"
    val hasyylineno = has "yylineno"
    end

  (* map over the intervals of a symbol set *)
    fun mapInt f syms = 
	  SIS.foldlInt (fn (i, ls) => (f i)::ls) [] syms

  (* transition interval representation *)
    datatype transition_interval = TI of SIS.interval * int * ml_exp
    fun intervalOf (TI (i, t, e)) = i
    fun tagOf      (TI (i, t, e)) = t
    fun actionOf   (TI (i, t, e)) = e
    fun sameTag    (TI (_, t1, _), TI (_, t2, _)) = t1 = t2
    fun singleton  (TI ((i, j), _, _)) = i = j

  (* generate code for transitions: generate a hard-coded binary
   * search on accepting characters
   *)
    fun mkTrans ([], _) = raise Fail "(BUG) SMLFunOutput: alphabet not covered"
      | mkTrans ([t], _)  = actionOf t
      | mkTrans ([t1, t2], _) = 
	  if sameTag (t1, t2) then actionOf t1
	  else let
	    val (_, t1end) = intervalOf t1
	    val (t2start, _) = intervalOf t2
	    in 
	      if singleton t1 then
		ML_If (ML_Cmp (ML.EQ, inpVar, ML_Sym t1end),
		       actionOf t1,
		       actionOf t2)
	      else if singleton t2 then
		ML_If (ML_Cmp (ML.EQ, inpVar, ML_Sym t2start),
		       actionOf t2,
		       actionOf t1)
	      else
		ML_If (ML_Cmp (ML.LEQ, inpVar, ML_Sym t1end),
		       actionOf t1,
		       actionOf t2)
            end
      | mkTrans (ts, len) = let
	  val lh = len div 2
	  fun split (ls, 0, l1) = (List.rev l1, ls)
	    | split (l::ls, cnt, l1) = split (ls, cnt-1, l::l1)
	    | split _ = raise Fail "(BUG) SMLFunOutput: split failed"
	  val (ts1, ts2) = split (ts, lh, [])
	  val (ts2start, ts2end) = intervalOf (List.hd ts2)
	  val (ts2', ts2len) = if ts2start = ts2end
			       then (List.tl ts2, len - lh - 1)
			       else (ts2, len - lh)
	(* we want to take advantage of the special case when 
	 * len = 3 and hd ts2 is a singleton.  this case often
	 * occurs when we have an arrow for a single character.
	 *)
	  val elseClause = 
	        if lh = 1 andalso ts2len = 1 
		then mkTrans ([List.hd ts1, List.hd ts2'], 2)
		else ML_If (ML_Cmp (ML.LT, inpVar, ML_Sym ts2start),
			    mkTrans (ts1, lh),
			    mkTrans (ts2', ts2len))
	  in
	    ML_If (ML_Cmp (ML.EQ, inpVar, ML_Sym ts2start),
		   actionOf (List.hd ts2),
		   elseClause)
          end

    fun mkState actionVec (s, k) = let
          val LO.State {id, label, final, next} = s
	  fun addMatch (i, lastMatch) = let
		val lastMatch' = if hasREJECT (Vector.sub (actionVec, i))
				 then lastMatch
				 else ML_Var "yyNO_MATCH"
		in
	          ML_App ("yyMATCH",
			  [ML_Var "strm",
			   ML_Var (actName i),
			   lastMatch'])
		end
	  val (curMatch, nextMatches) = (case final
					  of [] => (NONE, [])
					   | f::fs => (SOME f, fs)
					 (* end case *))
	  val lastMatch = List.foldr addMatch (ML_Var "lastMatch") nextMatches
	(* collect all valid transition symbols *)
	  val labels = List.foldl SIS.union SIS.empty (List.map #1 (!next))
        (* pair transition intervals with associated actions/transitions *)
	  val newFinal = (case curMatch
			   of SOME j => addMatch (j, lastMatch)
			    | NONE => lastMatch
			  (* end case *))
	  fun arrows (syms, s) = 
	        mapInt 
		  (fn i => TI (i, idOf s, 
		     ML_App (nameOf s, [ML_Var "strm'", newFinal])))
		  syms
	  val TIs = List.map arrows (!next)
	  val errAct' = 
	        (case curMatch
		  of SOME j => 
		       ML_App (actName j, 
			       [ML_Var "strm", 
				if hasREJECT (Vector.sub (actionVec, j))
				then lastMatch
				else ML_Var "yyNO_MATCH"])
		   | NONE =>  ML_App ("yystuck", [lastMatch])
		 (* end case *))
        (* if first state in machine, check for eof *)
	  val errAct = if id = 0
		       then ML_If (ML_App("yyInput.eof", [ML_Var "strm"]),
				   ML_App("UserDeclarations.eof", [ML_Var "yyarg"]),
				   errAct')
		       else errAct'
        (* error transitions = complement(valid transitions) *)
	  val error = SIS.complement labels
	  val errTIs = mapInt (fn i => TI (i, ~1, errAct)) error
	(* the arrows represent intervals that partition the entire
	 * alphabet, with each interval mapped to some transition or
	 * action.  we sort the intervals by their smallest member.
	 *)
	  fun gt (a, b) = (#1 (intervalOf a)) > (#1 (intervalOf b))
	  val sorted = ListMergeSort.sort gt (List.concat (errTIs :: TIs))
        (* now we want to find adjacent partitions with the same 
	 * action, and merge their intervals
	 *)
	  fun merge [] = []
	    | merge [t] = [t]
	    | merge (t1::t2::ts) = 
	        if sameTag (t1, t2) then let
		    val TI ((i, _), tag, act) = t1
		    val TI ((_, j), _,   _  ) = t2
		    val t = TI ((i, j), tag, act)
		    in
		      merge (t::ts)
		    end
		else
		  t1::(merge (t2::ts))
	  val merged = merge sorted
        (* create the transition code *)
	  val trans = mkTrans(merged, List.length merged)
        (* create the input code *)
	  val getInp = 
             (* trans has at least the error action.  if length(merged)
	      * is 1 then we can avoid getting any input and simply
	      * take the error transition in all cases.  note that
	      * the "error" transition may actually be a match
	      *)
	        (case merged
		  of [_] => errAct
		   | _ => ML_Case (ML_App ("yygetc", [ML_Var "strm"]),
			      [(ML_ConPat ("NONE", []), errAct),
			       (ML_ConPat ("SOME", [ML_VarPat (inp ^ ", strm'")]), 
				  trans)])
		 (* end case *))
	  in
            ML_Fun (nameOf s, ["strm", "lastMatch"], getInp, k)
          end

    fun mkAction (i, action, k) = let
          val updStrm = ML_RefPut (ML_Var "yystrm", ML_Var "strm")
	  val act = ML_Raw [ML.Tok action]
	  val seq = ML_Seq [updStrm, act]
	  val lett = if hasyytext action 
		     then ML_Let 
			    ("yytext", 
			     ML_App("yymktext", [ML_Var "strm"]), 
			     seq)
		     else seq
	  val letl = if hasyylineno action
		     then ML_Let 
			    ("yylineno", 
			     ML_App("ref", 
				 [ML_App ("yyInput.getlineNo", 
					  [ML_RefGet (ML_Var "yystrm")])]), 
			     lett)
		     else lett
	  val letr = if hasREJECT action
		     then ML_Let
			    ("oldStrm", ML_RefGet (ML_Var "yystrm"),
			     ML_Fun
			       ("REJECT", [],
				ML_Seq 
				  [ML_RefPut (ML_Var "yystrm", 
					      ML_Var "oldStrm"),
				   ML_App("yystuck", [ML_Var "lastMatch"])],
				letl))
		     else letl
	  in  
	    ML_Fun (actName i, ["strm", "lastMatch"], letr, k)
	  end

    fun lexerHook spec strm = let
          val LO.Spec {actions, dfa, startStates, ...} = spec
	  fun matchSS (label, state) =
	        (ML_ConPat (label, []), 
		   ML_App (nameOf state, 
				[ML_RefGet (ML_Var "yystrm"), 
				 ML_Var "yyNO_MATCH"]))
	  val innerExp = ML_Case (ML_RefGet (ML_Var "yyss"),
				  List.map matchSS startStates)
	  val statesExp = List.foldr (mkState actions) innerExp dfa
	  val lexerExp = Vector.foldri mkAction statesExp actions
          val ppStrm = TextIOPP.openOut {dst = strm, wid = 80}
          in
            ML.ppML (ppStrm, lexerExp)
          end

    fun startStatesHook spec strm = let
          val LO.Spec {startStates, ...} = spec
	  val machNames = #1 (ListPair.unzip startStates)
          in 
            TextIO.output (strm, String.concatWith " | " machNames)
          end

    fun userDeclsHook spec strm = let
          val LO.Spec {decls, ...} = spec
          in 
            TextIO.output (strm, decls)
	  end

    fun headerHook spec strm = let
          val LO.Spec {header, ...} = spec
          in 
            TextIO.output (strm, header)
	  end

    fun argsHook spec strm = let
          val LO.Spec {arg, ...} = spec
	  val arg' = if String.size arg = 0 
		     then "(yyarg as ())"
		     else "(yyarg as " ^ arg ^ ") ()"
          in 
            TextIO.output (strm, arg')
	  end

    structure TIO = TextIO
    val template = let
          val file = TIO.openIn "BackEnds/SML/template-sml-fun.sml"
	  fun done () = TIO.closeIn file
	  fun read () = (case TIO.inputLine file
			  of NONE => []
			   | SOME line => line::read()
			 (* end case *))
	  in 
            read() handle ex => (done(); raise ex)
	    before done()
	  end

    fun output (spec, fname) = 
          ExpandFile.expand {
	      src = template,
	      dst = fname ^ ".sml",
	      hooks = [("lexer", lexerHook spec),
		       ("startstates", startStatesHook spec),
		       ("userdecls", userDeclsHook spec),
		       ("header", headerHook spec),
		       ("args", argsHook spec)]
	    }

  end
