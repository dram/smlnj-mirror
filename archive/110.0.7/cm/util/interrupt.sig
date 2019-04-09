(*
 * util/interrupt.sig: turning SMLofNJ signals into exceptions
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
signature INTERRUPT = sig

    exception Interrupt

    (*
     * guarded: thunk -> 'a
     *  - run thunk () and return the result
     *  - if the thunk gets interrupted then raise Interrupt
     *)
    val guarded: (unit -> 'a) -> 'a

end
