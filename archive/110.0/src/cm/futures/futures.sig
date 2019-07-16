(*
 * futures/futures.sig:
 *   rudimentary MP support based on simple `future' metaphor.
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
signature FUTURES = sig

    type 'a future

    val future: (unit -> 'a) -> 'a future

    val get: 'a future -> 'a

    type semaphore

    val semaphore: unit -> semaphore

    val sequential: semaphore * ('a -> 'b) -> 'a -> 'b

end
