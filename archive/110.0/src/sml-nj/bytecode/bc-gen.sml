(* bc-gen.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

structure ByteCodeMC = CPScodeGenerator(
  structure Gen = CPSgen(structure M = ByteCode (BCMC.Coder)
			 structure MachSpec = ByteCodeSpec)
  fun collect() = BCMC.finish()
)  

(*
 * $Log: bc-gen.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:43  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.1.1.1  1997/01/14 01:38:28  george
 *   Version 109.24
 *
 *)
