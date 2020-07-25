(* load.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Load a CFG comp_unit from a file
 *)

structure Load =
  struct

    val load = ASDLFilePickle.fromFile CFGFilePickle.read_comp_unit

  end
