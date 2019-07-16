(* command-line.sml
 *
 * COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies.
 *)

structure CommandLine : COMMAND_LINE =
  struct
    val name = SMLofNJ.getCmdName
    val arguments = SMLofNJ.getArgs
  end;

(*
 * $Log: command-line.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:37  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.1  1997/06/02 19:15:00  jhr
 *   SML'97 Basis Library changes (phase 2)
 *
 *)

