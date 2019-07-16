(*
 * iid.sig -- Interface to `Import Identifiers'
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)

signature IID = sig

    structure Compiler: COMPILER
    type senv = Compiler.Environment.staticEnv
    type pid = Compiler.PersStamps.persstamp

    type t
    type set = t Set.set

    val eq: t * t -> bool
    val lt: t * t -> bool

    val new: { senv: pid, lambda: pid } -> t
    val aug: t * senv * senv -> t

    val staticPid: t -> pid
    val lambdaPid: t -> pid

    val makeset: t list -> set
    val union: set * set -> set
    val isSubset: set * set -> bool

    val toHex: set * string list -> string list

end
