(* lambda-var.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Dummy implementation of LambdaVar structure (and pickler) for testing.
 *)

structure LambdaVar : sig

    eqtype lvar

    val toId : lvar -> int
    val fromId : int -> lvar

  end = struct

    datatype lvar = LV of int

    fun toId (LV id) = id
    val fromId = LV

  end
