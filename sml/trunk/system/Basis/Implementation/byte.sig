(* byte.sig
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

signature BYTE = sig

    val byteToChar : Word8.word -> char
    val charToByte : char -> Word8.word

    val bytesToString : Word8Vector.vector -> string
    val stringToBytes : string -> Word8Vector.vector

    val unpackStringVec : Word8VectorSlice.slice -> string
    val unpackString    : Word8ArraySlice.slice -> string
    val packString      : Word8Array.array * int * Substring.substring -> unit

end
