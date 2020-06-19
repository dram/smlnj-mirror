(* cfg-regions.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This module is basically a copy of the CPSRegions module used in the
 * old code generator.
 *)

structure CFGRegions : sig

    structure PT : POINTS_TO = PointsTo

    type region = PT.region

    val stack     : region
    val spill     : region
    val readonly  : region
    val memory    : region
    val storelist : region
    val real      : region

    val toString  : region -> string

    val reset     : unit -> unit

  end = struct

    structure PT = PointsTo
    structure C  = CellsBasis

    type region = PT.region

    val memoryCell    = PT.TOP{id=C.mem 128, name="rw", mutable=true}
    val readonlyCell  = PT.TOP{id=C.mem 129, name="ro", mutable=false}
    val stackCell     = PT.TOP{id=C.mem 130, name="stack", mutable=true}
    val spillCell     = PT.TOP{id=C.mem 131, name="spill", mutable=true}
    val realCell      = PT.TOP{id=C.mem 132, name="real", mutable=false}
    val storelistCell = PT.TOP{id=C.mem 133, name="storelist", mutable=true}

    val memory     = ref memoryCell
    val readonly   = ref readonlyCell
    val stack      = ref stackCell
    val spill      = ref spillCell
    val real       = ref realCell
    val storelist  = ref storelistCell

    fun reset() =
	(memory    := memoryCell;
	 readonly  := readonlyCell;
	 stack     := stackCell;
	 spill     := spillCell;
	 real      := realCell;
	 storelist := storelistCell
	)

    val toString   = PT.toString

  end
