(*
 * util/table.sig:
 *   Lookup tables -- the unfanciest kind imaginable
 *    (If this really turns out to be a bottleneck, then I will
 *     replace it with something more efficient.)
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
signature TABLE = sig

    type ('a, 'b) table

    val create: ('a * 'a -> bool) -> ('a, 'b) table
    val enter: ('a, 'b) table * 'a * 'b -> unit
    val find: ('a, 'b) table * 'a -> 'b option
    val fold: ('a * 'b * 'accu -> 'accu) -> ('a, 'b) table -> 'accu -> 'accu
    val clear: ('a, 'b) table -> unit

end
