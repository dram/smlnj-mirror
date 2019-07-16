(*
 * dot/gen-dot.sig: interface to DOT generator
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
signature GEN_DOT = sig

    structure SysDag: SYS_DAG

    (* genDot: name-of-root-description-file * output-file-name -> unit *)
    val genDot: SysDag.desc * string -> unit

end
