(*
 * util/interrupt.sml: turning SMLofNJ signals into exceptions
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
structure Interrupt: INTERRUPT = struct

    exception Interrupt

    structure Sig = Signals

    val sigINT = Sig.sigINT
    val inqHandler = Sig.inqHandler
    val setHandler = Sig.setHandler

    fun guarded thunk = let
	val oh = inqHandler sigINT
	fun reset () = ignore (setHandler (sigINT, oh))
	fun thunk' () = thunk () handle exn => (reset (); raise exn)
	val callcc = SMLofNJ.Cont.callcc
	val throw = SMLofNJ.Cont.throw
    in
	callcc (fn exitK =>
		(callcc (fn intK =>
			 (setHandler (sigINT, Sig.HANDLER (fn _ => intK));
			  throw exitK (thunk' () before reset ())));
		 reset ();
		 raise Interrupt))
    end
end
