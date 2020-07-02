(* cfg-util.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure CFGUtil : sig

  (* does a cluster contain any raw C calls? *)
    val hasRCC : CFG.cluster -> bool

    val tyToString : CFG.ty -> string

  end = struct

    structure C = CFG

    fun hasRCC (C.Cluster({hasRCC, ...}, _, _)) = hasRCC

    fun tyToString (CFG.NUMt sz) = "i" ^ Int.toString sz
      | tyToString (CFG.FLTt sz) = "f" ^ Int.toString sz
      | tyToString CFG.PTRt = "ptr"
      | tyToString CFG.FUNt = "fun"
      | tyToString CFG.CNTt = "cont"

  end
