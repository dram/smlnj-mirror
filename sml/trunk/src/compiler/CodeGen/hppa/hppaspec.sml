(* machspec.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

structure HppaSpec : MACH_SPEC = 
struct

    open DefaultMachSpec

    val architecture	= "hppa"
    val spillAreaSz	= 4000
    val numRegs		= 19	(* length HppaCpsRegs.miscregs + 3 *)
    val numFloatRegs	= 25
      (* length HppaCpsRegs.floatregs + length HppaCpsRegs.savedfpregs *)
    val bigEndian	= true
    val startgcOffset	= ~28
    val pseudoRegOffset = ~36
    val constBaseRegOffset = 8192
end

(*
 * $Log: hppaspec.sml,v $
 * Revision 1.2  1998/05/08 10:52:25  george
 *   The exhausted register has been made optional -- leung
 *
 * Revision 1.1.1.1  1998/04/08 18:39:55  george
 * Version 110.5
 *
 *)
