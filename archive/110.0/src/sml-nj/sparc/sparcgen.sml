(* sparcgen.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

structure SparcMC = CPScodeGenerator(
    structure SparcC = Coder(structure M = SparcInstr and E = SparcMCEmit)
    structure Gen=CPSgen(structure M = SparcCM(structure C = SparcC)
			 structure MachSpec = SparcSpec)
    fun collect() = (SparcC.finish(); SparcMCode.getCodeString())
)


(*
 * $Log: sparcgen.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:48  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.1.1.1  1997/01/14 01:38:45  george
 *   Version 109.24
 *
 *)
