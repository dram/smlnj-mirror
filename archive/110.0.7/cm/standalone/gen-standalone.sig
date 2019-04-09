(*
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
signature GEN_SALONE = sig

    structure SysDag: SYS_DAG

    (* genStandAlone: name-of-root-descr-file * output-file-name -> unit *)
    val genStandAlone: SysDag.desc * string -> unit

end
