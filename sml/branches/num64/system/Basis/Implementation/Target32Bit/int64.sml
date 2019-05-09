(* int64.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Implementation of Word64 for 32-bit targets.
 *)

structure Int64Imp : sig

(*
    include INTEGER
*)

    type int

    val extern : int -> Word32.word * Word32.word
    val intern : Word32.word * Word32.word -> int

  end = struct

    structure I64 = InlineT.Int64

    type int = Int64.int	(* from Basis/TypesOnly *)

    val extern = I64.extern
    val intern = I64.intern

    val precision = SOME 64
    val minInt = SOME(~0x4000000000000000 : int)
    val maxInt = SOME(0x7fffffffffffffff : int)

  end
