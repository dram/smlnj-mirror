(* imperative-io.sig
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

signature IMPERATIVE_IO =
  sig
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

    structure StreamIO : STREAM_IO
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

  end;

(*
 * $Log: imperative-io.sig,v $
 * Revision 1.2  1998/10/16 17:26:29  jhr
 *   Implemented multiple EOF support; changes to basis I/O API
 *
 * Revision 1.1.1.1  1998/04/08 18:39:55  george
 * Version 110.5
 *
 *)
