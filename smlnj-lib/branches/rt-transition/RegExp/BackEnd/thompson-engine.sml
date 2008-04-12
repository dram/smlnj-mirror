(* thompson-engine.sml
 *
 * COPYRIGHT (c) 2008 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This is an implementation of Ken Thompson's RE matchine algorithm from
 * CACM (1968).  It is based on the description of the algorithm by Russ
 * Cox at http://swtch.com/~rsc/regexp/regexp1.html.
 *)

structure ThompsonEngine : REGEXP_ENGINE =
  struct

    structure RE = RegExpSyntax
    structure CSet = RE.CharSet
    structure M = MatchTree

  (* a match specifies the position (as a stream) and the length of the match *)
    type 'a match = {pos : 'a, len : int} MatchTree.match_tree

  (* intermediate representation of states *)
    datatype state'
      = CHR' of (int * char * state' ref)
      | CSET' of (int * CSet.set * state' ref)
      | NCSET' of (int * CSet.set * state' ref)
      | SPLIT' of (int * state' ref * state' ref)
      | FINAL'

    type frag = {start : state', out : state' ref list}

  (* return the ID of a state *)
    fun idOf (CHR'(id, _, _)) = id
      | idOf (CSET'(id, _, _)) = id
      | idOf (NCSET'(id, _, _)) = id
      | idOf (SPLIT'(id, _, _)) = id
      | idOf (FINAL') = 0

  (* interpreter representation of states *)
    datatype state
      = CHR of (char * int)
      | CSET of (CSet.set * int)
      | NCSET of (CSet.set * int)
      | SPLIT of (int * int)
      | FINAL

    fun cvtState (CHR'(id, c, out)) = CHR(c, idOf(!out))
      | cvtState (CSET'(id, cset, out)) = CSET(cset, idOf(!out))
      | cvtState (NCSET'(id, cset, out)) = NCSET(cset, idOf(!out))
      | cvtState (SPLIT'(id, out1, out2)) = SPLIT(idOf(!out1), idOf(!out2))
      | cvtState (FINAL') = FINAL

    datatype regexp = RE of {start : int, states : state vector}

    fun compile re = let
	(* the list of states; state 0 is always the accepting state *)
	  val nStates = ref 1
	  val states = ref [FINAL']
	(* create new states *)
	  fun new mk = let
		val id = !nStates
		val s = mk id
		in
		  states := s :: !states;
		  nStates := id+1;
		  s
		end
	  fun newChr (c, out) = new (fn id => CHR'(id, c, out))
	  fun newCset (cset, out) = new (fn id => CSET'(id, cset, out))
	  fun newNcset (cset, out) = new (fn id => NCSET'(id, cset, out))
	  fun newSplit (out1, out2) = new (fn id => SPLIT'(id, out1, out2))
	(* update the outputs of a fragment *)
	  fun setOuts (f : frag, s : state') = List.app (fn r => r := s) (#out f)
	(* compile an RE *)
	  fun reComp re = (case re
		 of RE.Group re => reComp re
		  | RE.Alt[re] => reComp re
		  | RE.Alt(re::rest) =>  let
		      val f1 = reComp re
		      val f2 = reComp (RE.Alt rest)
		      val s = newSplit(ref(#start f1), ref(#start f2))
		      in
			{start = s, out = #out f1 @ #out f2}
		      end
		  | RE.Concat[re] => reComp re
		  | RE.Concat(re::rest) => let
		      val f1 = reComp re
		      val f2 = reComp(RE.Concat rest)
		      in
			setOuts (f1, #start f2);
			{start = #start f1, out = #out f2}
		      end
		  | RE.Interval(re, 0, SOME 1) => option re
		  | RE.Interval(re, 0, NONE) => closure re
		  | RE.Interval(re, 1, NONE) => posClosure re
		  | RE.Interval(re, i, optn) => raise Fail "Interval"
		  | RE.Option re => option re
		  | RE.Star re => closure re
		  | RE.Plus re => posClosure re
		  | RE.MatchSet cset => let
		      val out = ref FINAL'
		      in
			{start = newCset(cset, out), out = [out]}
		      end
		  | RE.NonmatchSet cset => let
		      val out = ref FINAL'
		      in
			{start = newNcset(cset, out), out = [out]}
		      end
		  | RE.Char c => let
		      val out = ref FINAL'
		      in
			{start = newChr(c, out), out = [out]}
		      end
		  | RE.Begin => raise Fail "Begin"
		  | RE.End => raise Fail "End"
		(* end case *))
	(* compile re? *)
	  and option re = let
		val f = reComp re
		val out = ref FINAL'
		val s = newSplit(ref(#start f), out)
		in
		  {start = s, out = out :: #out f}
		end
        (* compile re* *)
	  and closure re = let
		val f = reComp re
		val out = ref FINAL'
		val s = newSplit(ref(#start f), out)
		in
		  setOuts (f, s);
		  {start = s, out = [out]}
		end
        (* compile re+ *)
	  and posClosure re = let
		val f = reComp re
		val out = ref FINAL'
		val s = newSplit(ref(#start f), out)
		in
		  setOuts (f, s);
		  {start = #start f, out = [out]}
		end
	(* generate the intermediate state representation *)
	  val frag = reComp re
	  val _ = setOuts (frag, FINAL')
	(* convert the states to the final representation; note that we reverse the list
	 * so that the states are now in increasing order.
	 *)
	  val states = List.foldl (fn (s, l) => cvtState s :: l) [] (!states)
	  in
	    RE{ start = idOf(#start frag), states = Vector.fromList states }
	  end

  (* scan the stream for the first occurence of the regular expression *)
    fun scan (RE{start, states}, getc : (char,'a) StringCvt.reader, isFirst, strm : 'a) = let
	(* to make elimination of duplicates in a state set cheap, we map state IDs
	 * to a stamp of the last set that they were added to.
	 *)
	  val stamp = ref 0w1
	  val lastStamp = Array.array(Vector.length states, 0w0)
	  fun addState (stamp', stateList, id) =
		if (Array.sub(lastStamp, id) = stamp')
		  then stateList
		  else (
		    Array.update(lastStamp, id, stamp');
		    case Vector.sub(states, id)
		     of SPLIT(out1, out2) =>
			  addState (stamp', addState (stamp', stateList, out1), out2)
		      | state => state :: stateList
		    (* end case *))
	  fun startState () = addState(!stamp, [], start)
	  fun isMatch stamp' = (Array.sub(lastStamp, 0) = stamp')
	  fun find' startPos = let
		fun scan (_, _, lastAccepting, []) = lastAccepting
		  | scan (n, strm, lastAccepting, nfaState) = (case getc strm
		       of NONE => if isMatch (!stamp)
			    then SOME(n, startPos)
			    else lastAccepting
			| SOME(c, strm') => let
			    val stamp' = !stamp
			    val _ = (stamp := stamp' + 0w1)
			    fun test ([], nextStates) = nextStates
			      | test (s::r, nextStates) = (case s
				   of CHR(c', out) => if (c = c')
					then addState (stamp', nextStates, out)
					else nextStates
				    | CSET(cset, out) => if CSet.member(cset, c)
					then addState (stamp', nextStates, out)
					else nextStates
				    | NCSET(cset, out) => if CSet.member(cset, c)
					then nextStates
					else addState (stamp', nextStates, out)
				    | _ => nextStates
				  (* end case *))
			    val nextNfaState = test (nfaState, [])
			    val lastAccepting = if isMatch stamp'
				  then SOME(n+1, startPos)
				  else lastAccepting
			    in
			      scan (n+1, strm', lastAccepting, nextNfaState)
			    end
		      (* end case *))
		in
		  case scan (0, startPos, NONE, startState())
		   of NONE => (case getc startPos
			 of SOME(_, strm) => find' strm
			  | NONE => NONE
			(* end case *))
		    | SOME(n, strm) => SOME(M.Match({pos=startPos, len=n}, []), strm)
		  (* end case *)
		end
	  in
	    find' strm
	  end

	fun find re getc stream = let
	      fun loop (isFirst, s) = (case (scan (re, getc, isFirst, s))
		     of NONE => (case (getc s)
			   of SOME(_, s') => loop (false, s')
			    | NONE => NONE
			  (* end case *))
		      | SOME v => SOME v
		    (* end case *))
	      in
		loop (true, stream)
	      end

	fun prefix re getc strm = scan (re, getc, true, strm)

	fun match [] = (fn getc => fn strm => NONE)
	  | match l = let
	    (* compile the REs *)
	      val l = List.map (fn (re, act) => (compile re, act)) l
	      fun match' getc strm = let
		  (* find the longest SOME *)
		    fun loop ([], max, _) = max
		      | loop ((re, act)::r, max, maxLen) = (case scan(re, getc, true, strm)
			   of NONE => loop (r, max, maxLen)
			    | SOME(m as MatchTree.Match({len, ...}, _), cs) =>
				if (len > maxLen) 
				  then loop (r, SOME(m, act, cs), len)
				  else loop (r, max, maxLen)
			  (* end case *))
		    in
		      case loop (l, NONE, ~1) 
		       of NONE => NONE
			| SOME(m, act, cs) => SOME(act m, cs)
		      (* end case *)
		    end
	      in
		match'
	      end
  end
