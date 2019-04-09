(*
 * sched/lists.sig: Make various topologically sorted name lists
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
signature LISTS = sig

    structure SysDag: SYS_DAG

    val smls: SysDag.analyzed_entity -> SysDag.GroupDag.SmlSource.t list
    val names: SysDag.analyzed_entity -> string list
    val binfiles: SysDag.analyzed_entity -> string list
    val strings: SysDag.analyzed_entity -> string list

end
