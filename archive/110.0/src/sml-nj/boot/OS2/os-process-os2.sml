(* os-process-os2.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * This is the OS/2 implementation of the generic OS.Process structure.
 *
 * Peter Bertelsen, August 1995.
 *
 *)

structure OS_Process : OS_PROCESS =
  struct

    structure CU = CleanUp
    structure OS2P = OS2.Process

    type status = OS.Process.status (* int *)

    val success = 0
    val failure = 1

    val system    = OS2P.system
    val terminate = OS2P.exit
    val getEnv    = OS2P.scanEnv

    local
      val hooks = ref ([] : (unit -> unit) list)
      val _ = CU.addCleaner ( "OS.Process", [CU.AtExit],
              fn _ => List.app (fn f => (f ()) handle _ => ()) (! hooks))
    in
      fun atExit hook = (hooks:= hook :: !hooks)
    end

    fun exit sts = (CU.clean CU.AtExit; terminate sts)

  end

(*
 * $Log: os-process-os2.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:40  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.1.1.1  1997/01/14 01:38:21  george
 *   Version 109.24
 *
 *)
