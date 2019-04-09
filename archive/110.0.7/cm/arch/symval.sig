(*
 * arch/symval.sig: CM preprocessor symbols
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
signature SYMVAL = sig

    val lookup: string -> int option
    val define: string * int -> unit
    val undef: string -> unit
    val undefall: unit -> unit
    val default: { conf: Arch.conf, version: int list } -> unit

end
