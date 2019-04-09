(*
 * cm/cleanup.sig: registering a function to initialize control state
 *                 during startup
 *
 *   Copyright (c) 1997 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
signature CLEANUP = sig

    val init: unit -> unit
    val uninit: unit -> unit

end
