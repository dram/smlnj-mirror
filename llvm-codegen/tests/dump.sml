(* dump.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Dump a CFG comp_unit
 *)

structure Dump =
  struct

    val dump = ASDLFilePickle.toFile CFGFilePickle.write_comp_unit

    fun dumpAll () = List.app dump [
	    ("ex1.pkl", Ex1.cu),
	    ("ex2.pkl", Ex2.cu),
	    ("ex3.pkl", Ex3.cu),
	    ("ex4.pkl", Ex4.cu)
	  ]

  end
