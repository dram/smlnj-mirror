(* pack-word-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

signature PACK_WORD =
  sig

    val bytesPerElem : int

    val isBigEndian : bool

    val subVec : (Word8Vector.vector * int) -> LargeWord.word
    val subVecX : (Word8Vector.vector * int) -> LargeWord.word

    val subArr : (Word8Array.array * int) -> LargeWord.word
    val subArrX : (Word8Array.array * int) -> LargeWord.word

    val update : (Word8Array.array * int * LargeWord.word) -> unit

  end;

(*
 * $Log: pack-word-sig.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:38  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.1.1.1  1997/01/14 01:38:16  george
 *   Version 109.24
 *
 *)
