(*
 * cm/complain.sig: Guard functions with exception handling/error reporting
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
signature COMPLAIN = sig

    val complaining: ('a -> 'b) -> ('a -> 'b)
    val warning: ('a -> unit) -> ('a -> unit)

end
