(* dump.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Dump a CFG comp_unit
 *)

structure Dump =
  struct

    fun dump = ASDLFilePickle.toFile CFGFilePickle.write_comp_unit

  end
