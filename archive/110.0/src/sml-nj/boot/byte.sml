(* byte.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

structure Byte : BYTE =
  struct
    val chr     : Word8.word -> char = InlineT.cast
    val ord     : char -> Word8.word = InlineT.cast
    val vectorToString : (Word8Vector.vector * int * int) -> string
	  = InlineT.cast CharVector.extract
    val arrayToString : (Word8Array.array * int * int) -> string
	  = InlineT.cast CharArray.extract

    val byteToChar : Word8.word -> char = InlineT.cast
    val charToByte : char -> Word8.word = InlineT.cast

    val bytesToString : Word8Vector.vector -> string = InlineT.cast
    val stringToBytes : string -> Word8Vector.vector = InlineT.cast

    val unpackStringVec : (Word8Vector.vector * int * int option) -> string
	 = InlineT.cast Word8Vector.extract
    val unpackString  : (Word8Array.array * int * int option) -> string
	 = InlineT.cast Word8Array.extract

    local
    (* the substring type is abstract, so we use a cast to an equivalent type
     * to get around this problem.
     *)
      datatype substring' = SS of (string * int * int)
      val toSS : Substring.substring -> substring' = InlineT.cast
      structure A = InlineT.Word8Array
      structure V = InlineT.CharVector
    in
    fun packString (arr, i, ss) = let
	  val SS(src, srcStart, srcLen) = toSS ss
	  val dstLen = A.length arr
	  fun cpy (_, _, 0) = ()
	    | cpy (srcIndx, dstIndx, n) = (
		A.update (arr, dstIndx, InlineT.cast(V.sub(src, srcIndx)));
		cpy (srcIndx+1, dstIndx+1, n-1))
	  in
	    if (i < 0) orelse (i > dstLen-srcLen) then raise Subscript else ();
	    cpy (srcStart, i, srcLen)
	  end
    end

  end


(*
 * $Log: byte.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:37  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.3  1997/05/29 14:44:19  jhr
 *   SML'97 Basis Library changes (phase 1)
 *
 * Revision 1.2  1997/02/11  15:15:35  george
 * got rid of structure rebinding, since inlining is now preserved
 *
 * Revision 1.1.1.1  1997/01/14  01:38:13  george
 *   Version 109.24
 *
 *)
