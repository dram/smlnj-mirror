(*
 * sched/lists.sml: Make various topologically sorted name lists
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
functor ListsFun (LinearSched: LINEAR_SCHED): LISTS = struct

    structure SysDag = LinearSched.SysDag
    structure GroupDag = SysDag.GroupDag
    structure SmlSource = GroupDag.SmlSource

    fun d2sml (GroupDag.DAG { smlsource, ... }) = smlsource

    val sml2name = AbsPath.elab o SmlSource.name
    val sml2binfile = AbsPath.elab o SmlSource.binfile
    val sml2string = SmlSource.makestring

    fun d2name d = sml2name (d2sml d)
    fun d2binfile d = sml2binfile (d2sml d)
    fun d2string d = sml2string (d2sml d)

    val smls = LinearSched.linearize d2sml
    val names = LinearSched.linearize d2name
    val binfiles = LinearSched.linearize d2binfile
    val strings = LinearSched.linearize d2string

end
