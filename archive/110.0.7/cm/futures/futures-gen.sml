(*
 * futures/futures-gen.sml:
 *   Default implementation of futures -- no parallelism, computation
 *   proceeds immediately.
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
structure Futures:> FUTURES = struct

    type 'a future = 'a

    fun future prog = prog ()

    fun get r = r

    type semaphore = unit

    fun semaphore () = ()

    fun sequential ((), f) = f

end
