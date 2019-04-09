(* x86spec.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

structure X86Spec : MACH_SPEC =
  struct
    open DefaultMachSpec
    val architecture = "x86"

    val numRegs= 17                (* 5 real/12 virtual *)

    val numFloatRegs = 7
    val numFloatCalleeSaves = 0

    val bigEndian = false
    val spillAreaSz = 0
    val startgcOffset = 32
    val pseudoRegOffset = 0

end

(*
 * $Log: x86spec.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:49  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.1.1.1  1997/01/14 01:38:51  george
 *   Version 109.24
 *
 *)
