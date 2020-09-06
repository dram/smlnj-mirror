(* normalize-cluster.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Code to normalize a cluster, which means guarantee that it satisfies the
 * following properties:
 *
 *	1) exactly one entry function (i.e., a `CONT` or `ESCAPE` function)
 *
 *	2) there are no internal jumps to the entry function.
 *
 * Note that this pass is run _before_ we do limit-check analysis (see limit.sml)
 *
 * TODO:
 *	- add label as argument to new cluster entry functions, when the function
 *	  will need the base pointer
 *	- reorder parameters to new cluster-entry functions to reduce register
 *	  shuffling; i.e., match the standard-function calling convention.
 *)

structure NormalizeCluster : sig

    type cluster = CPS.function list

  (* `transform cluster` normalizes `cluster`, if necessary, and returns a list of
   * normalized clusters.
   *)
    val transform : cluster * cluster list -> cluster list

  end = struct

    structure C = CPS
    structure LV = LambdaVar
    structure LTbl = LV.Tbl

    fun error msg = ErrorMsg.impossible ("NormalizeCluster: " ^ msg)

    fun for (start, stop) f = let
	  fun lp i = if (i < stop) then (f i; lp (i+1)) else ()
	  in
	    lp start
	  end

    type cluster = C.function list

(*DEBUG*)
val say = Control.Print.say
fun prCluster (fn1::fns) = (
      say "***** CLUSTER START *****\n";
      PPCps.printcps0 fn1;
      List.app (fn f => (say "***** FRAG *****\n"; PPCps.printcps0 f)) fns;
      say "***** CLUSTER END *****\n")
(*DEBUG*)

  (***** cluster info *****)

    structure Info : sig
	type t
	val mk : C.function list * C.function list -> t
	val nEntries : t -> int
	val nOther : t -> int
	val nFuncs : t -> int
	val idOf : t -> LV.lvar -> int
	val labelOf : t -> int -> LV.lvar
	val funcOf : t -> int -> C.function
	val succs : t -> int -> int list
      (* `dfsApp info f id` does a DFS traversal of the graph starting at node `id`
       * and applies the function `f` to each reachable node as it is visited.
       *)
	val dfsApp : t -> (int -> unit) -> int -> unit
(*DEBUG*)
	val dump : t -> unit
(* DEBUG*)
      end = struct

      (* information about a group of CPS functions *)
	type t = {
	    nEntries : int,			(* the number of entries *)
	    nOther : int,			(* the number of non-entry functions *)
	    funcToID : LV.lvar -> int,		(* mapping from lvars to function IDs *)
	    idToFunc : C.function array,	(* mapping from IDs to functions *)
	    callGraph : int list array		(* representation of call graph *)
	  }

	fun mk (entries, frags) : t = let
	      val nEntries = List.length entries
	      val nOther = List.length frags
	      val numFuncs = nEntries + nOther
	    (* mapping of function names to a dense integer range *)
	      val funcToIdTbl : int LTbl.hash_table = LTbl.mkTable(numFuncs, Fail "func->id")
	      val funcToID = LTbl.lookup funcToIdTbl
	    (* mapping of ids to functions *)
	      val idToFuncTbl = let
		    val tbl = Array.array(numFuncs, hd frags)
		    val add = LTbl.insert funcToIdTbl
		    in
		      List.appi
			(fn (id, func as (_,f,_,_,_)) => (
			    add(f, id); Array.update(tbl, id, func)))
			  entries;
		      List.appi
			(fn (i, func as (_,f,_,_,_)) => let
			    val id = nEntries + i
			    in
			      add(f, id); Array.update(tbl, id, func)
			    end)
			  frags;
		      tbl
		    end
	    (* create a call graph for the functions, represented as an array `g` of integer
	     * IDs, where `Array.sub(g, id)` is the list of function IDs that the function
	     * with ID `id` calls.
	     *)
	      val graph = Array.array(numFuncs, [])
	      fun addEdges ((_, g, _, _, body), nodes) = let
		    fun addEdge (f, succs) = let
			  val id = funcToID f
			  fun add ([], ids) = id :: ids
			    | add (id' :: r, ids) = if (id <> id')
				then add(r, id'::ids)
				else succs
			  in
			    add (succs, [])
			  end
		    fun calls (e, succs) = (case e
			   of C.APP(C.LABEL l, _) => addEdge (l, succs)
			    | C.APP _ => succs
			    | C.RECORD(_, _, _, e) => calls (e, succs)
			    | C.SELECT(_, _, _, _, e) => calls (e, succs)
			    | C.OFFSET(_, _, _, e) => calls (e, succs)
			    | C.SWITCH(_, _, es) => List.foldl calls succs es
			    | C.BRANCH(_, _, _, e1, e2) => calls (e2, calls (e1, succs))
			    | C.SETTER(_, _, e) => calls (e, succs)
			    | C.LOOKER(_, _, _, _, e) => calls (e, succs)
			    | C.ARITH(_, _, _, _, e) => calls (e, succs)
			    | C.PURE(_, _, _, _, e) => calls (e, succs)
			    | C.RCC(_, _, _, _, _, e) => calls (e, succs)
			    | C.FIX _ => error "unexpected FIX"
			  (* end case *))
		    in
		      calls (body, []) :: nodes
		    end (* addEdges *)
	      val graph = Array.fromList (Array.foldr addEdges [] idToFuncTbl)
	      in {
		nEntries = nEntries,
		nOther = nOther,
		funcToID = funcToID,
		idToFunc = idToFuncTbl,
		callGraph = graph
	      } end

	fun nEntries (info : t) = #nEntries info
	fun nOther (info : t) = #nOther info
	fun nFuncs (info : t) = #nEntries info + #nOther info
	fun idOf (info : t) = #funcToID info
	fun funcOf (info : t) id = Array.sub (#idToFunc info, id)
	fun labelOf info id = #2(funcOf info id)
	fun succs (info : t) id = Array.sub (#callGraph info, id)

	fun dfsApp (info : t) f rootId = let
	      val numFuncs = #nEntries info + #nOther info
	      val visited = Array.array(numFuncs, false)
	      fun dfs id = if Array.sub(visited, id)
		    then ()
		    else (
		      Array.update(visited, id, true);
		      f id;
		      List.app dfs (Array.sub (#callGraph info, id)))
	      in
		dfs rootId
	      end

(*DEBUG*)
	fun dump ({nEntries, nOther, idToFunc, callGraph, ...} : t) = let
	      fun id2s id = let val (_, f, _, _, _) = Array.sub(idToFunc, id)
		    in
		      if (id < nEntries) then "*" ^ LV.lvarName f else LV.lvarName f
		    end
	      fun sayNd (id, ids) = say(String.concat[
		      "#### ", id2s id, " --> {",
		      String.concatWithMap "," id2s ids,
		      "}\n"
		    ])
	      in
		say (concat[
		    "### CLUSTER INFO (", Int.toString nEntries, "/",
		    Int.toString(nEntries+nOther), " entries)\n"
		  ]);
		Array.appi sayNd callGraph;
		say "###\n"
	      end
(* DEBUG*)

      end (* structure Info *)

  (***** reachability info *****)

  (* we define a "signature" for an internal fragment to be the set of entry functions
   * that can trace a path of direct jumps to the fragment.  We represent a signature
   * as a pair <n, S>.  Let b_i = 1 if there is a path from the i'th entry to the
   * fragment, and 0 otherwise.  Then n = SUM(b_i) and S =  SUM(b_i * 2^i) for
   * i in [0..nEntries-1].
   *)
    structure FSig : sig
	type t
      (* the empty signature *)
	val empty : t
      (* the singleton signature for the entry with the given ID *)
	val singleton : int -> t
      (* does a signature include the given entry? *)
	val canReach : int -> t -> bool
      (* add a bit to a signature, where we assume that the bit was not previously set *)
	val add : int -> t -> t
      (* are two signatures the same? *)
	val same : t * t -> bool
      (* finite maps keyed by signatures *)
	structure Map : ORD_MAP where type Key.ord_key = t
      end = struct
        datatype t = FSIG of int * IntInf.int
	val empty = FSIG(0, 0)
	fun singleton id = FSIG(1, IntInf.<<(1, Word.fromInt id))
      (* does a signature include the given entry? *)
	fun canReach id = let
	      val s = IntInf.<<(1, Word.fromInt id)
	      in
		fn (FSIG(_, s')) => (IntInf.andb(s, s') <> 0)
	      end
      (* add a bit to a signature, where we assume that the bit was not previously set *)
	fun add id = let
	      val s = IntInf.<<(1, Word.fromInt id)
	      in
		fn (FSIG(n, s')) => FSIG(n+1, IntInf.orb(s, s'))
	      end
	fun same (FSIG(n1, sig1), FSIG(n2, sig2)) = (sig1 = sig2)
	fun compareSig (FSIG(n1, sig1), FSIG(n2, sig2)) = (case Int.compare(n1, n2)
	       of EQUAL => IntInf.compare(sig1, sig2)
		| order => order
	      (* end case *))

      (* finite maps keyed by signatures *)
	structure Map = RedBlackMapFn (
	  struct
	    type ord_key = t
	    val compare = compareSig
	  end)

      end (* structure FSig *)

  (***** Normailzation *****)

  (* check if there is a back edge to the entry function; if so, we split the entry into
   * a trivial header function and a second function that will be the target of the back
   * edges.
   *)
    fun checkForHeader (entry, frags) = let
	  val (fk, entryLab, params, tys, body) = entry
	  fun hasBackEdge e = (case e
		 of C.APP(C.LABEL l, _) => (l = entryLab)
		  | C.APP _ => false
		  | C.RECORD(_, _, _, e) => hasBackEdge e
		  | C.SELECT(_, _, _, _, e) => hasBackEdge e
		  | C.OFFSET(_, _, _, e) => hasBackEdge e
		  | C.SWITCH(_, _, es) => List.exists hasBackEdge es
		  | C.BRANCH(_, _, _, e1, e2) => (hasBackEdge e1 orelse hasBackEdge e2)
		  | C.SETTER(_, _, e) => hasBackEdge e
		  | C.LOOKER(_, _, _, _, e) => hasBackEdge e
		  | C.ARITH(_, _, _, _, e) => hasBackEdge e
		  | C.PURE(_, _, _, _, e) => hasBackEdge e
		  | C.RCC(_, _, _, _, _, e) => hasBackEdge e
		  | C.FIX _ => error "unexpected FIX"
		(* end case *))
	  in
	    if hasBackEdge body orelse List.exists (fn (_, _, _, _, e) => hasBackEdge e) frags
	      then let
		val hdrLab = LV.mkLvar()
		val params' = List.map (fn _ => LV.mkLvar()) params
	      (* the new entry function *)
		val entry' =
		     (fk, entryLab, params', tys, C.APP(C.LABEL hdrLab, List.map C.VAR params'))
	      (* rewrite jumps to `entryLab` with jumps to `hdrLab` *)
		fun rewrite e = (case e
		       of C.APP(C.LABEL l, args) => if (l = entryLab)
			    then C.APP(C.LABEL hdrLab, args)
			    else e
			| C.APP _ => e
			| C.RECORD(rk, args, lv, e) => C.RECORD(rk, args, lv, rewrite e)
			| C.SELECT(i, v, lv, ty, e) => C.SELECT(i, v, lv, ty, rewrite e)
			| C.OFFSET(i, v, lv, e) => C.OFFSET(i, v, lv, rewrite e)
			| C.SWITCH(v, id, es) => C.SWITCH(v, id, List.map rewrite es)
			| C.BRANCH(p, vs, id, e1, e2) => C.BRANCH(p, vs, id, rewrite e1, rewrite e2)
			| C.SETTER(p, vs, e) => C.SETTER(p, vs, rewrite e)
			| C.LOOKER(p, vs, lv, ty, e) => C.LOOKER(p, vs, lv, ty, rewrite e)
			| C.ARITH(p, vs, lv, ty, e) => C.ARITH(p, vs, lv, ty, rewrite e)
			| C.PURE(p, vs, lv, ty, e) => C.PURE(p, vs, lv, ty, rewrite e)
			| C.RCC(b, cc, proto, vs, lvs, e) => C.RCC(b, cc, proto, vs, lvs, rewrite e)
			| C.FIX _ => error "unexpected FIX"
		      (* end case *))
		in
		  entry' :: (C.KNOWN, hdrLab, params, tys, rewrite body)
		    :: List.map (fn (fk, lab, xs, tys, e) => (fk, lab, xs, tys, rewrite e)) frags
		end
	      else entry::frags
	  end

  (* `normalize (entries, frags)` takes a cluster that has been partitioned into entry
   * functions and non-entry functions, where `entries` has at least two members, and
   * returns a list of normalized clusters that includes one cluster per entry function,
   * plus additional shared clusters.
   *)
    fun normalize (entries, frags) = let
val _ = say "## NormalizeCluster.normalize\n"
	  val info = Info.mk (entries, frags)
	  val nEntries = Info.nEntries info
	  val nFuncs = Info.nFuncs info
val _ = Info.dump info
	(* a mapping from "other" functions to their signatures *)
	  val idToSig = Array.array(nFuncs, FSig.empty)
	(* starting with the given entry function, do a DFS traversal to identify those
	 * fragments that are reachable from the entry and update their signatures.
	 *)
	  fun dfs entryId = let
		val add = FSig.add entryId
		fun visit id = Array.update(idToSig, id, add(Array.sub(idToSig, id)))
		in
		  Info.dfsApp info visit entryId
		end
	(* compute the signatures for the fragments *)
	  val _ = for (0, nEntries) dfs
	(* partition the fragments by signature.  Note that the number of elements
	 * in `sigMap` is a lower bound on the number of clusters.
	 *)
val _ = say "### compute sigMap\n"
	  val sigMap = let
		fun ins (id, sign, sMap) = let
		      val ids = (case FSig.Map.find(sMap, sign)
			     of SOME ids =>  id::ids
			      | NONE => [id]
			    (* end case *))
		      in
			FSig.Map.insert(sMap, sign, ids)
		      end
		in
		  Array.foldli ins FSig.Map.empty idToSig
		end
	(* mark the entries, which are those nodes that have at least one predecessor with
	 * a different signature.
	 *)
	  val isEntry = Array.array(nFuncs, false)
	  val visited = Array.array(nFuncs, false)
	  val entries = ref []
	  fun walk predSig id = let
		val idSig = Array.sub(idToSig, id)
		in
		(* if the signature changes, then we mark the node as an entry, since there
		 * is another path from an entry to it.
		 *)
		  if FSig.same(predSig, idSig) orelse Array.sub(isEntry, id)
		    then ()
		    else ( (* first entry edge for the node *)
		      entries := id :: !entries;
		      Array.update(isEntry, id, true));
		  if Array.sub(visited, id)
		    then ()
		    else (
		      Array.update(visited, id, true);
		      List.app (walk idSig) (Info.succs info id))
		end
val _ = say "### mark entries\n"
	  val _ = for (0, nEntries) (walk FSig.empty)
	(* form new clusters *)
	  fun mkCluster ids = let
		fun mk ([], [], _) = error ".mkCluster: no entry function"
		  | mk ([], [entry], frags) =
		   (* check if we need to add a header node *)
		      [checkForHeader (entry, frags)]
		  | mk ([], entries, frags) =
		    (* recursively normalize the cluster *)
		      normalize (entries, frags)
		  | mk (id::ids, entries, frags) = let
		      val func = Info.funcOf info id
		      in
			if Array.sub(isEntry, id)
			  then mk (ids, func::entries, frags)
			  else mk (ids, entries, func::frags)
		      end
		in
		  mk (ids, [], [])
		end
	  in
say "### clusters:\n";
FSig.Map.app (fn ids => let
    fun id2s id = let val lab = LV.lvarName(Info.labelOf info id)
	  in
	    if Array.sub(isEntry, id) then "*" ^ lab else lab
	  end
    in
      say(concat["#### ", String.concatWithMap "," id2s ids, "\n"])
    end)
  sigMap;
	    FSig.Map.foldl (fn (ids, clusters) => mkCluster ids @ clusters) [] sigMap
	  end

(*DEBUG*)
val normalize = fn arg => let val clusters = normalize arg
in
  say "## NormalizeCluster.normalize result:\n";
  List.app prCluster clusters;
  say "## done\n";
  clusters
end
(*DEBUG*)

  (***** Main function *****)

    fun isEscaping (f : C.function) = (case #1 f
	   of C.CONT => true
	    | C.ESCAPE => true
	    | _ => false
	  (* end case *))
    val partition = List.partition isEscaping

    fun transform ([frag], clusters) = [frag] :: clusters
      | transform (cluster, clusters) = (case partition cluster
	   of ([], _) => error "NormalizeCluster.transform: no entry fragment"
	    | ([entry], frags) => (entry::frags) :: clusters
	    | (ents, frags) => normalize (ents, frags) @ clusters
	  (* end case *))
(*DEBUG*)
val transform = fn (frags, clusters) => (
say "## NormalizeCluster.transform:\n"; prCluster frags;
transform (frags, clusters))
(*DEBUG*)

  end (* NormalizeCluster *)
