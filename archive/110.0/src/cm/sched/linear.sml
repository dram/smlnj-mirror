(*
 * sched/linear.sml:
 *   Linearize a dependency graph
 *   (topological ordering --
 *    all ancestors of a node come before the node itself)
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
functor LinearSchedFun (DagTraversal: DAG_TRAVERSAL): LINEAR_SCHED = struct

    structure SysDag = DagTraversal.SysDag

    fun flatten l = foldr (op @) l

    val tr = let
	fun seen_before _ = []
	fun pre _ = ()
	fun extern _ = ()
	fun cross (_, l) = l
	fun intern (_, l) = l
	fun combine (_, _, c, i, dag) = flatten (flatten [dag] i) c
	val trav = DagTraversal.traversal
    in
	trav { seen_before = seen_before, pre = pre, extern = extern,
	       cross = cross, intern = intern, combine = combine }
    end

    fun linearize f (SysDag.AE { roots, ...}) = map f (List.concat (tr roots))

end
