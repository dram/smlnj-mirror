(* shareglue.sml
 *
 * Copyright 1989 by AT&T Bell Laboratories
 *
 * shareglue: a functor application common to all architectures.
 *)

functor IntShare (structure VC : VISCOMP) : sig end = 
  struct
    structure BootEnv = BootEnvF(VC)

  (* environment initializations *)
    val _ = (#set VC.EnvRef.pervasive (SCEnv.SC (BootEnv.makePervEnv())))

  (* establish default signal handlers *)
    local
      fun handleINT _ = !Unsafe.topLevelCont
      fun handleTERM _ = OS.Process.exit OS.Process.failure
      fun ifSignal (sigName, handler) = (case (Signals.fromString sigName)
	     of (SOME s) => (
		  Signals.overrideHandler (s, Signals.HANDLER handler); ())
	      | _ => ()
	    (* end case *))
    in
    val _ = (
	Signals.overrideHandler (Signals.sigINT, Signals.HANDLER handleINT);
	Signals.overrideHandler (Signals.sigTERM, Signals.HANDLER handleTERM);
	ifSignal ("QUIT", handleTERM))
    end

  (* launch interactive loop *)
    val _ = (
	Control.Print.say "Go for it\n";
	VC.Interact.interact())

  end

(*
 * $Log: shareglue.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:44  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.2  1997/06/30 19:37:17  jhr
 *   Removed System structure; added Unsafe structure.
 *
 * Revision 1.1.1.1  1997/01/14  01:38:32  george
 *   Version 109.24
 *
 *)
