(* pointer.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Implementation of raw runtime-system pointers for 32-bit targets.
 *)

structure PointerImp =
  struct

    structure AddrWord = Word32Imp

    type c_pointer = c_pointer

    val sizeofPointer = 0w4

  (* convert a pointer to its bit representation *)
    val toWord = InlineT.Pointer.toWord

  (* convert a pointer to its bit representation in the LargeWord type *)
    val toLargeWord = Word32Imp.toLarge o InlineT.Pointer.toWord

  (* compare two pointers *)
    fun compare (p, q) = if (p = q) then EQUAL
	  else if (InlineT.Pointer.toWord p < InlineT.Pointer.toWord q) then LESS
	  else GREATER

  end
