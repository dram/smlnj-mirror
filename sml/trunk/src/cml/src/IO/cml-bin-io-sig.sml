(* cml-bin-io-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 * COPYRIGHT (c) 1991 John H. Reppy.
 *
 * This extends the SMLBL BIN_IO interface with event-valued operations.
 *)

signature CML_BIN_IO =
  sig
    include CML_IMPERATIVE_IO
(*
      where type StreamIO.vector = Word8Vector.vector
      where type StreamIO.elem = Word8.word
      where type StreamIO.reader = BinPrimIO.reader
      where type StreamIO.writer = BinPrimIO.writer
      where type StreamIO.pos = BinPrimIO.pos = Position.int
*)

    val openIn     : string -> instream
    val openOut    : string -> outstream
    val openAppend : string -> outstream
  end
    where type StreamIO.vector = Word8Vector.vector
    where type StreamIO.elem = Word8.word
    where type StreamIO.reader = BinPrimIO.reader
    where type StreamIO.writer = BinPrimIO.writer
    where type StreamIO.pos = BinPrimIO.pos
