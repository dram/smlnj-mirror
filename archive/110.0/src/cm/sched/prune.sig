(*
 * sched/prune.sig:
 *   Find transitive closure of dependency relation starting from an
 *   `a priory' subset of the sources and prune dependency graph
 *   accordingly.
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
signature PRUNE = sig

    structure SD: SYS_DAG

    val prune: (SD.dag -> bool) -> SD.analyzed_entity -> SD.analyzed_entity
end
