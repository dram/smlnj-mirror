(* cml-imperative-io-sig.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 * COPYRIGHT (c) 1991 John H. Reppy.
 *
 * This extends the SMLBL IMPERATIVE_IO interface with event-valued operations.
 *)

signature CML_IMPERATIVE_IO =
  sig

  (* include IMPERATIVE_IO *)

    type vector
    type elem

    type instream
    type outstream

    val input    : instream -> vector
    val input1   : instream -> elem option
    val inputN   : (instream * int) -> vector
    val inputAll : instream -> vector
    val canInput : (instream * int) -> int option
    val lookahead : instream -> elem option
    val closeIn : instream -> unit
    val endOfStream : instream -> bool

    val output   : (outstream * vector) -> unit
    val output1  : (outstream * elem) -> unit
    val flushOut : outstream -> unit
    val closeOut : outstream -> unit

    structure StreamIO : CML_STREAM_IO
      sharing type vector = StreamIO.vector
      sharing type elem = StreamIO.elem

    val mkInstream  : StreamIO.instream -> instream
    val getInstream : instream -> StreamIO.instream
    val setInstream : (instream * StreamIO.instream) -> unit

    val getPosOut    : outstream -> StreamIO.out_pos
    val setPosOut    : (outstream * StreamIO.out_pos) -> unit
    val mkOutstream  : StreamIO.outstream -> outstream
    val getOutstream : outstream -> StreamIO.outstream
    val setOutstream : (outstream * StreamIO.outstream) -> unit

    val input1Evt   : instream -> elem option CML.event
    val inputNEvt   : (instream * int) -> vector CML.event
    val inputEvt    : instream -> vector CML.event
    val inputAllEvt : instream -> vector CML.event

  end;
