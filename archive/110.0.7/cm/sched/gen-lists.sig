(*
 * sched/gen-lists.sig:
 *   Make various topologically sorted name lists
 *   (given the root description file)
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
signature GEN_LISTS = sig

    structure SysDag: SYS_DAG

    val smls: SysDag.desc -> SysDag.GroupDag.SmlSource.t list
    val names: SysDag.desc -> string list
    val binfiles: SysDag.desc -> string list
    val strings: SysDag.desc -> string list

    val mkusefile: SysDag.desc * string -> unit

end
