(*
 * Build the dependency graph for one group/library.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
signature BUILDDEPEND = sig
    type impexp = DependencyGraph.impexp

    val build :
	{ imports: impexp SymbolMap.map,
	  gimports: impexp SymbolMap.map,
	  smlfiles: SmlInfo.info list,
	  localdefs: SmlInfo.info SymbolMap.map,
	  subgroups: (AbsPath.t * GroupGraph.group) list,
	  reqpriv: GroupGraph.privileges }
	* SymbolSet.set option		(* filter *)
	* (string -> unit)		(* error *)
	* GeneralParams.info
	->
	impexp SymbolMap.map		(* exports *)
	* GroupGraph.privileges		(* required privileges (aggregate) *)
end

structure BuildDepend :> BUILDDEPEND = struct

    structure S = Symbol
    structure SS = SymbolSet
    structure SM = SymbolMap
    structure SK = Skeleton
    structure DG = DependencyGraph
    structure DE = DAEnv
    structure EM = GenericVC.ErrorMsg
    structure SP = GenericVC.SymPath

    type impexp = DG.impexp

    fun look otherwise DE.EMPTY s = otherwise s
      | look otherwise (DE.BINDING (s', v)) s =
	if S.eq (s, s') then v else otherwise s
      | look otherwise (DE.LAYER (e, e')) s = look (look otherwise e') e s
      | look otherwise (DE.FCTENV looker) s =
	(case looker s of NONE => otherwise s | SOME v => v)
      | look otherwise (DE.FILTER (ss, e)) s =
	if SymbolSet.member (ss, s) then look otherwise e s else otherwise s

    (* get the description for a symbol *)
    fun symDesc (s, r) =
	S.nameSpaceToString (S.nameSpace s) :: " " :: S.name s :: r

    fun build (coll, fopt, error, gp) = let
	val { imports, gimports, smlfiles, localdefs, subgroups, reqpriv } =
	    coll

	(* the "blackboard" where analysis results are announced *)
	(* (also used for cycle detection) *)
	val bb = ref SmlInfoMap.empty
	fun lock i = bb := SmlInfoMap.insert (!bb, i, NONE)
	fun release (i, r) = (bb := SmlInfoMap.insert (!bb, i, SOME r); r)
	fun fetch i = SmlInfoMap.find (!bb, i)

	(* - get the result from the blackboard if it is there *)
	(* - otherwise trigger analysis *)
	(* - detect cycles using locking *)
	(* - maintain root set *)
	fun getResult (i, history) =
	    case fetch i of
		NONE => (lock i; release (i, analyze (i, history)))
	      | SOME (SOME r) => r
	      | SOME NONE => let	(* cycle found --> error message *)
		    val f = SmlInfo.sourcepath i
		    fun pphist pps = let
			fun recur (_, []) = () (* shouldn't happen *)
			  | recur (n'', (s, i') :: r) = let
				val f' = SmlInfo.sourcepath i'
				val n' = AbsPath.spec f'
				val _ =
				    if SmlInfo.eq (i, i') then ()
				    else recur (n', r)
				val l =
				    n' :: " refers to " ::
				    symDesc (s, [" defined in ", n''])
			    in
				app (PrettyPrint.add_string pps) l;
				PrettyPrint.add_newline pps
			    end
		    in
			PrettyPrint.add_newline pps;
			recur (AbsPath.spec f, history)
		    end
		in
		    SmlInfo.error gp i EM.COMPLAIN
		         "cyclic ML dependencies" pphist;
		    release (i, (DG.SNODE { smlinfo = i,
					    localimports = [],
					    globalimports = [] },
				 DE.EMPTY))
		end

	(* do the actual analysis of an ML source and generate the
	 * corresponding node *)
	and analyze (i, history) = let
	    val li = ref []
	    val gi = ref []

	    (* register a local import *)
	    fun localImport n =
		if List.exists (fn n' => DG.seq (n, n')) (!li) then ()
		else li := n :: !li

	    (* register a global import, maintain filter sets *)
	    fun globalImport (f, n) = let
		fun sameN (_, n') = DG.sbeq (n, n')
	    in
		case List.find sameN (!gi) of
		    NONE => gi := (f, n) :: !gi (* brand new *)
		  | SOME (NONE, n') => () (* no filter -> no change *)
		  | SOME (SOME f', n') => let
			(* there is a filter...
			 *  calculate "union", see if there is a change,
			 *  and if so, replace the filter *)
			fun replace filt =
			    gi := (filt, n) :: List.filter (not o sameN) (!gi)
		    in
			case f of
			    NONE => replace NONE
			  | SOME f =>
				if SS.equal (f, f') then ()
				else replace (SOME (SS.union (f, f')))
		    end
	    end

	    val f = SmlInfo.sourcepath i
	    fun isSelf i' = SmlInfo.eq (i, i')

	    (* lookup function for things not defined in the same ML file.
	     * As a side effect, this function registers local and
	     * global imports. *)
	    fun lookimport s = let
		fun internalError s =
		    EM.impossible "build/lookimport/lookfar"
		fun lookfar () =
		    case SM.find (imports, s) of
			SOME (farn, e) => (globalImport farn;
					   look internalError e s)
		      | NONE =>
			    (* We could complain here about an undefined
			     * name.  However, since CM doesn't have the
			     * proper source locations available, it is
			     * better to handle this case silently and
			     * have the compiler catch the problem later. *)
			    DE.EMPTY
	    in
		case SM.find (localdefs, s) of
		    SOME i' =>
			if isSelf i' then lookfar ()
			else let
			    val (n, e) = getResult (i', (s, i) :: history)
			in
			    localImport n;
			    e
			end
		  | NONE => lookfar ()
	    end

	    (* build the lookup function for DG.env *)
	    val lookup = look lookimport

	    fun lookSymPath e (SP.SPATH []) = DE.EMPTY
	      | lookSymPath e (SP.SPATH (p as (h :: t))) = let
		    (* again, if we don't find it here we just ignore
		     * the problem and let the compiler catch it later *)
		    val lookup' = look (fn _ => DE.EMPTY)
		    fun loop (e, []) = e
		      | loop (e, h :: t) = loop (lookup' e h, t)
		in
		    loop (lookup e h, t)
		end

	    (* "eval" -- compute the export environment of a skeleton *)
	    fun eval sk = let
		fun evalDecl e (SK.Bind (name, def)) =
		    DE.BINDING (name, evalModExp e def)
		  | evalDecl e (SK.Local (d1, d2)) =
		    evalDecl (DE.LAYER (evalDecl e d1, e)) d2
		  | evalDecl e (SK.Seq l) = evalSeqDecl e l
		  | evalDecl e (SK.Par []) = DE.EMPTY
		  | evalDecl e (SK.Par (h :: t)) =
		    foldl (fn (x, r) => DE.LAYER (evalDecl e x, r))
		          (evalDecl e h) t
		  | evalDecl e (SK.Open s) = evalModExp e s
		  | evalDecl e (SK.Ref s) =
		    (SS.app (ignore o lookup e) s; DE.EMPTY)

		and evalSeqDecl e [] = DE.EMPTY
		  | evalSeqDecl e (h :: t) = let
			fun one (d, e') =
			    DE.LAYER (evalDecl (DE.LAYER (e', e)) d, e')
		    in
			foldl one (evalDecl e h) t
		    end

		and evalModExp e (SK.Var sp) = lookSymPath e sp
		  | evalModExp e (SK.Decl l) = evalSeqDecl e l
		  | evalModExp e (SK.Let (d, m)) =
		    evalModExp (DE.LAYER (evalSeqDecl e d, e)) m
		  | evalModExp e (SK.Ign1 (m1, m2)) =
		    (ignore (evalModExp e m1); evalModExp e m2)
	    in
		evalDecl DE.EMPTY sk
	    end

	    val e = case SmlInfo.skeleton gp i of
		SOME sk => eval sk
	      | NONE => DE.EMPTY

	    val n = DG.SNODE { smlinfo = i,
			       localimports = !li,
			       globalimports = !gi }
	in
	    (n, e)
	end

	(* run the analysis on one ML file -- causing the blackboard
	 * to be updated accordingly *)
	fun doSmlFile i = ignore (getResult (i, []))

	(* converting smlinfos to sbnodes * env *)
	fun i2sbn i = let
	    val (sn, e) = valOf (valOf (fetch i))
	in
	    (DG.SB_SNODE sn, e)
	end

	(* run the analysis *)
	val _ = app doSmlFile smlfiles

	fun addDummyFilt (sbn, e) = ((NONE, sbn), e)

	(* First we make a map of all locally defined symbols to
	 * the local "far sb node"
	 * but with only a dummy filter attached.
	 * This makes it consistent with the current state
	 * of "imports" and "gimports" where there can be filters, but
	 * where those filters are not yet strengthened according to fopt *)
	val localmap = SM.map (addDummyFilt o i2sbn) localdefs

	val exports =
	    case fopt of
		NONE =>
		    (* There is no filter -- so we are in an ordinary
		     * group and should export all gimports as well as
		     * all local definitions.
		     * No filter strengthening is necessary. *)
		    SM.unionWith #1 (localmap, gimports)
	      | SOME ss => let
		    (* There is a filter.
		     * We export only the things in the filter.
		     * They can be taken from either localmap or else from
		     * imports.  In either case, it is necessary to strengthen
		     * the filter attached to each node. *)
		    fun strengthen ((fopt', sbn), e) = let
			val new_fopt =
			    case fopt' of
				NONE => fopt
			      | SOME ss' => SOME (SS.intersection (ss, ss'))
		    in
			((new_fopt, sbn), DE.FILTER (ss, e))
		    end
		    val availablemap = SM.unionWith #1 (localmap, imports)
		    fun addNodeFor (s, m) =
			case SM.find (availablemap, s) of
			    SOME n => SM.insert (m, s, strengthen n)
			  | NONE => (error 
				      (concat ("exported " ::
					       symDesc (s, [" not defined"])));
				     m)
		in
		    SS.foldl addNodeFor SM.empty ss
		end
    in
	(exports, reqpriv)
    end
end
