(* os2-process.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

(* os2-process.sml
 *
 * This is the OS/2 process interface.
 *
 * Peter Bertelsen, August 1995.
 *)

structure OS2_Process : OS2_PROCESS =
  struct

    fun os2Func x = CInterface.c_function "SMLNJ-OS2" x
 
    val system  : string -> int           = os2Func "system"
    val exit    : int -> 'a               = os2Func "exit"
    val scanEnv : string -> string option = os2Func "scanEnv"

  end (* structure OS2_Process *)


(*
 * $Log: os2-process.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:41  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.1.1.1  1997/01/14 01:38:22  george
 *   Version 109.24
 *
 *)
