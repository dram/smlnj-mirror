(* win32-glue.sml
 *
 * COPYRIGHT (c) 1998 Bell Labs, Lucent Technologies.
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 * COPYRIGHT (c) 1989-1991 John H. Reppy * 
 *
 * The glue for the Win32 version of CML.
 *)

structure Win32Glue : OS_GLUE =
  struct

    fun init () = TimeOut.reset()

    fun pollOS () = (
	  TimeOut.pollTime();
	  IOManager.pollIO();
	  ProcManager.pollProcs()) 

    fun pause () = (case TimeOut.anyWaiting()
	   of NONE => if (IOManager.anyWaiting() orelse ProcManager.anyWaiting())
		then (Signals.pause(); true)
		else false
(** NOTE: eventually, we should just go to sleep for the specified time **)
	    | (SOME t) => (Signals.pause(); true)
	  (* end case *))

    fun shutdown () = TimeOut.reset()

  end;

