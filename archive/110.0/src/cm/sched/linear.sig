(*
 * sched/linear.sig:
 *   Linearize a dependency graph
 *   (topological ordering --
 *    all ancestors of a node come before the node itself)
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
signature LINEAR_SCHED = sig

    structure SysDag: SYS_DAG

    val linearize: (SysDag.dag -> 'a) -> SysDag.analyzed_entity -> 'a list

end
