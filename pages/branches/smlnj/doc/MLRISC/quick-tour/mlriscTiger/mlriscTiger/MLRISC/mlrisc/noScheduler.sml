(* noSchedule.sml --- do nothing with the instructions.
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

functor NoScheduler(Instructions : INSTRUCTIONS) : SCHEDULER =
struct
  structure I = Instructions
  fun schedule(instrs, _) = instrs
end
 


(*
 * $Log$
 * Revision 1.1  2001/10/11 09:52:26  macqueen
 * Initial revision
 *
 * Revision 1.1.1.1  1998/04/08 18:39:02  george
 * Version 110.5
 *
 *)
