(*
 * driver/control.sig: Controlling levels of verbosity, etc.
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
signature CONTROL = sig

    val verbose: bool option -> bool
    val debug: bool option -> bool
    val keep_going: bool option -> bool
    val show_exports: bool option -> bool

    val say: string -> unit
    val vsay: string -> unit
    val dsay: string -> unit

    val parse_caching: int option -> int

end
