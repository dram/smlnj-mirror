(* bc-spec.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

structure ByteCodeSpec : MACH_SPEC =
  struct

    open DefaultMachSpec
    val architecture = "bc"
    val numRegs = 14
    val numFloatRegs = 16
    val bigEndian = true
    val spillAreaSz = 0
    val startgcOffset = 0
    val pseudoRegOffset = 0
  end

(*
 * $Log: bc-spec.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:43  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.1.1.1  1997/01/14 01:38:28  george
 *   Version 109.24
 *
 *)
