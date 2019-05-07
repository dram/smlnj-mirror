(* word64.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure Word64 : sig

    type word

    val extern : word -> Word32.word * Word32.word
    val intern : Word32.word * Word32.word -> word

  end = struct

    type word = Word64.word

    val extern = InlineT.Word64.extern
    val intern = InlineT.Word64.intern

  end
