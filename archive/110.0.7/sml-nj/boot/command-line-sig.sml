(* command-line-sig.sml
 *
 * COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies.
 *)

signature COMMAND_LINE =
  sig

    val name : unit -> string
    val arguments : unit -> string list

  end;

(*
 * $Log: command-line-sig.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:37  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.1  1997/06/02 19:14:59  jhr
 *   SML'97 Basis Library changes (phase 2)
 *
 *)

