(* prim-io.sig
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

signature PRIM_IO =
  sig
    type elem
    type vector
    type vector_slice
    type array
    type array_slice

    eqtype pos

    val compare : (pos * pos) -> order

    datatype reader = RD of {
	name      : string, 
	chunkSize : int,
	readVec   : (int -> vector) option,
        readArr   : (array_slice -> int) option,
	readVecNB : (int -> vector option) option,
	readArrNB : (array_slice -> int option) option,
	block     : (unit -> unit) option,
	canInput  : (unit -> bool) option,
	avail     : unit -> int option,
	getPos    : (unit -> pos) option,
	setPos    : (pos -> unit) option,
        endPos    : (unit -> pos) option,
	verifyPos : (unit -> pos) option,
	close     : unit -> unit,
	ioDesc    : OS.IO.iodesc option
      }

    datatype writer = WR of {
	name       : string,
	chunkSize  : int,
	writeVec   : (vector_slice -> int) option,
	writeArr   : (array_slice -> int) option,
	writeVecNB : (vector_slice -> int option) option,
	writeArrNB : (array_slice -> int option) option,
	block      : (unit -> unit) option,
	canOutput  : (unit -> bool) option,
	getPos     : (unit -> pos) option,
	setPos     : (pos -> unit) option,
        endPos     : (unit -> pos) option,
	verifyPos  : (unit -> pos) option,
	close      : unit -> unit,
	ioDesc     : OS.IO.iodesc option
      }

    val augmentReader : reader -> reader
    val augmentWriter : writer -> writer

  end


