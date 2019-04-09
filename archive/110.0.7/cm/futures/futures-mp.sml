(*
 * futures/futures-mp.sml:
 *   Futures implemented on top of MP package.
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
structure Futures:> FUTURES = struct

    exception FuturesBug

    local

	structure MP = MP ()
	structure SPC = SafePreCo (MP)
	structure ST: SAFETHREADS = SPC
	structure T = ST.Thread

	type lock = MP.spin_lock

	fun new_lock () = MP.spin_lock ()

	fun lock s = let
	    fun spin () =
		if MP.try_lock s then
		    ()
		else
		    (T.yield (); spin ())
	in
	    spin ()
	end

	fun unlock s = MP.unlock s

	datatype 'a state =
	    FLYING of lock
	  | LANDED of 'a

    in
		
	type 'a future = 'a state ref

	fun future f = let
	    val semaphore = new_lock ()
	    val fut = ref (FLYING semaphore)
	    fun work () = (fut := LANDED (f ()); unlock semaphore)
	in
	    lock semaphore; T.fork work; fut
	end

	fun get (ref (LANDED v)) = v
	  | get (fut as ref (FLYING semaphore)) =
	    (lock semaphore;
	     case !fut of
		 LANDED v => (unlock semaphore; v)
	       | _ => raise FuturesBug)

	type semaphore = lock

	val semaphore = new_lock

	fun sequential ((), f) x =
	    (lock semaphore; f x before unlock semaphore)

    end

end
