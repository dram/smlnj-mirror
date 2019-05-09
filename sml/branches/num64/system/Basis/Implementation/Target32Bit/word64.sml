(* word64.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Implementation of Word64 for 32-bit targets.
 *)

structure Word64Imp : sig

    include WORD

    val extern : word -> Word32.word * Word32.word
    val intern : Word32.word * Word32.word -> word

  end = struct

    structure W64 = InlineT.Word64
    structure W32 = Word32Imp		(* 64BIT: FIXME *)

    type word = Word64.word	(* from Basis/TypesOnly *)

    fun unimplemented _ = raise Fail "unimplemented"

    val extern = W64.extern
    val intern = W64.intern

    val wordSize = 64

    val toLarge  = unimplemented	(* W64.toLarge *)
    val toLargeX = unimplemented	(* W64.fromLargeX *)
    val fromLarge = unimplemented	(* W64.fromLarge *)

  (* same as above, but deprecated *)
    val toLargeWord = toLarge
    val toLargeWordX = toLargeX
    val fromLargeWord = fromLarge

    val toInt = W64.toInt
    val toIntX = W64.toIntX
    val fromInt = W64.fromInt

    val toLargeInt = W64.toLargeInt
    val toLargeIntX = W64.toLargeIntX
    val fromLargeInt = W64.fromLargeInt

    val op * : word * word -> word = W64.*
    val op + : word * word -> word = W64.+
    val op - : word * word -> word = W64.-
    val op div : word * word -> word = W64.div
    val op mod : word * word -> word = W64.mod

    val orb  : word * word -> word = W64.orb
    val xorb : word * word -> word = W64.xorb
    val andb : word * word -> word = W64.andb
    val notb : word -> word = W64.notb

    val <<   = W64.chkLshift
    val >>   = W64.chkRshiftl
    val ~>>  = W64.chkRshift

    fun compare (w1, w2) =
	  if (W64.<(w1, w2)) then LESS
	  else if (W64.>(w1, w2)) then GREATER
	  else EQUAL

    val op > : word * word -> bool = W64.>
    val op >= : word * word -> bool = W64.>=
    val op < : word * word -> bool = W64.<
    val op <= : word * word -> bool = W64.<=

    val ~   : word -> word = ~
    val min : word * word -> word = W64.min
    val max : word * word -> word = W64.max

    fun toString w = (case extern w
	   of (0w0, lo) => W32.toString lo
	    | (hi, lo) => W32.toString hi ^ (StringCvt.padLeft #"0" 8 (W32.toString lo))
	  (* end case *))

    fun fmt StringCvt.BIN w = let
	  val fmt32Bin = W32.fmt StringCvt.BIN
	  in
	    case extern w
	     of (0w0, lo) => fmt32Bin lo
	      | (hi, lo) => fmt32Bin hi ^ (StringCvt.padLeft #"0" 32 (fmt32Bin lo))
	    (* end case *)
	  end
      | fmt StringCvt.HEX w = toString w
      | fmt radix w = IntInfImp.fmt radix (toLargeInt w)

(* 64BIT: we should add Word64 support to NumFormat and NumScan
    val fmt = NumFormat.fmtWord
    val toString = fmt StringCvt.HEX

    val scan = NumScan.scanWord
    val fromString = PreBasis.scanString (scan StringCvt.HEX)
*)
    val scan = unimplemented
    val fromString = unimplemented

  (* added for Basis Library proposal 2016-001 *)

    fun popCount w = let (* 64BIT: FIXME *)
	  val (hi, lo) = extern w
	  in
	    InlineT.Int.fast_add(W32PopCount.popCount hi, W32PopCount.popCount lo)
	  end

  end
