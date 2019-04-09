(*
 * analysis/traversal.sml:
 *   Generic traversal of dependency DAG.
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
functor DagTraversalFun (SysDag: SYS_DAG): DAG_TRAVERSAL = struct

    structure SysDag = SysDag
    structure GroupDag = SysDag.GroupDag
    structure ModuleName = SysDag.ModuleName

    fun traversal { seen_before = seen, pre = p, extern = e,
		    cross = c, intern = i, combine = com } = let

	fun pc (ce as SysDag.CE { dag, ... }) = c (ce, tr dag)
	and pi dag = i (dag, tr dag)

	and tr_regardless (dag as GroupDag.DAG { intern, extern, ... }) = let
	    val SysDag.INFO { cross, extern } = extern
	    val p_r = p dag
	    val e_r = map e (ModuleName.makelist extern)
	    val c_r = map pc (Set.makelist cross)
	    val i_r = map pi (Set.makelist intern)
	in
	    com (p_r, e_r, c_r, i_r, dag)
	end
	and tr (dag as GroupDag.DAG { marked = ref true, ... }) = seen dag
	  | tr (dag as GroupDag.DAG { marked, ... }) =
	    (marked := true; tr_regardless dag)

	fun full_traversal dagl = let
	    val r = map tr_regardless dagl
	in
	    SysDag.reset_marks dagl; r
	end

    in
	full_traversal o Set.makelist
    end

end
