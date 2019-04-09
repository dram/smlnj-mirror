(*
 * dot/dot.sig: generate input for DOT (draw the dependency graph)
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
signature DOT = sig

    structure SysDag: SYS_DAG

    (* dot: `writer' * dag -> unit *)
    val dot: (string -> unit) * SysDag.analyzed_entity -> unit

end
