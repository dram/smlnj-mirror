(*
 * Get the set of reachable SNODEs in a given dependency graph.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
signature REACHABLE = sig
    val reachable' : DependencyGraph.impexp SymbolMap.map -> SrcPathSet.set
    val reachable : GroupGraph.group -> SrcPathSet.set
    val snodeMap : GroupGraph.group -> DependencyGraph.snode SrcPathMap.map
end

structure Reachable :> REACHABLE = struct
    structure DG = DependencyGraph

    local
	fun reach ops (exports: DG.impexp SymbolMap.map) = let
	    val { add, member, empty } = ops
	    fun snode (x as DG.SNODE n, known) = let
		val { smlinfo, localimports = l, globalimports = g } = n
		val p = SmlInfo.sourcepath smlinfo
	    in
		if member (known, p) then known
		else foldl farsbnode (foldl snode (add (known, p, x)) l) g
	    end
	
	    and farsbnode ((_, n), known) = sbnode (n, known)
		
	    and sbnode (DG.SB_BNODE _, known) = known
	      | sbnode (DG.SB_SNODE n, known) = snode (n, known)
		
	    fun impexp ((n, _), known) = farsbnode (n, known)
	in
	    SymbolMap.foldl impexp empty exports
	end

	fun snodeMap' (exports, acc) =
	    reach { add = SrcPathMap.insert,
		    member = SrcPathMap.inDomain,
		    empty = acc } exports
    in
	val reachable' =
	    reach { add = fn (s, x, _) => SrcPathSet.add (s, x),
		    member = SrcPathSet.member,
		    empty = SrcPathSet.empty }

	fun reachable (GroupGraph.GROUP { exports, ... }) = reachable' exports

	fun snodeMap g = let
	    fun snm (g, (a, seen)) = let
		val GroupGraph.GROUP { exports, sublibs, grouppath, ... } = g
	    in
		if SrcPathSet.member (seen, grouppath) then (a, seen)
		else foldl (fn ((_, g), x) => snm (g, x))
		           (snodeMap' (exports, a),
			    SrcPathSet.add (seen, grouppath))
			   sublibs
	    end
	in
	    #1 (snm (g, (SrcPathMap.empty, SrcPathSet.empty)))
	end
    end
end
