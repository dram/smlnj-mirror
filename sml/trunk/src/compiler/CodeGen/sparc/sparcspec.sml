(* sparcspec.sml
 *
 * COPYRIGHT (c) 1998 AT&T Bell Laboratories.
 *
 *)

structure SparcSpec : MACH_SPEC = 
struct

    open DefaultMachSpec

    val architecture	= "sparc"
    val numRegs		= 18 
    val numFloatCalleeSaves = 0 
    val numCalleeSaves = 3
    val numFloatRegs	= 16
    val bigEndian	= true
    val spillAreaSz	= 3800
    val startgcOffset	= 100
    val constBaseRegOffset = 4096
end

(*
 * $Log: sparcspec.sml,v $
 * Revision 1.1.1.1  1998/08/05 19:37:50  george
 *   Release 110.7.4
 *
 *)
