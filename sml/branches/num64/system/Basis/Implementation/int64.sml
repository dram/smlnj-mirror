(* int64.sml
 *
 *   64-bit integers
 *
 * Copyright (c) 2004 by The Fellowship of SML/NJ
 *
 * Author: Matthias Blume (blume@tti-c.org)
 *)

structure Int64 : sig

    type int

    val extern : int -> Word32.word * Word32.word
    val intern : Word32.word * Word32.word -> int

  end = struct

    type int = Int64.int

    val extern = InlineT.Int64.extern
    val intern = InlineT.Int64.intern

  end
