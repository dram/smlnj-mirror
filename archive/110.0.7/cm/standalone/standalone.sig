(*
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
signature SALONE = sig

    structure SysDag: SYS_DAG

    (* standalone: `writer' * dag -> unit *)
    val standAlone: (string -> unit) * SysDag.analyzed_entity -> unit

end
