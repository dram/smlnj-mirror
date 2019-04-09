(* x86gen.sml
 * by Yngvi Guttesen (ysg@id.dth.dk) and Mark Leone (mleone@cs.cmu.edu)
 *
 * Copyright 1989 by	  Department of Computer Science, 
 *			  The Technical University of Denmak
 *			  DK-2800 Lyngby 
 *
 *)

structure X86MC = CPScodeGenerator(
  structure MachineCoder = X86MCode(X86Jumps)
  structure Gen = CPSgen(structure M = X86CM(MachineCoder)
			 structure MachSpec = X86Spec)
  val collect = MachineCoder.finish
)

(*
 * $Log: x86gen.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:49  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.1.1.1  1997/01/14 01:38:50  george
 *   Version 109.24
 *
 *)
