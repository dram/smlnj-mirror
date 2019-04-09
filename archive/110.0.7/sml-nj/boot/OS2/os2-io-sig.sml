(* os2-io-sig.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

(* os2-io-sig.sml
 *
 * Signature for the OS/2 I/O interface.
 *
 * Peter Bertelsen, September 1995.
 *)

signature OS2_IO =
  sig

    eqtype file_hdl
    val fhToIOD : file_hdl -> OS.IO.iodesc
    val iodToFH : OS.IO.iodesc -> file_hdl option

    (* Flags and modes for openf and createf *)
    datatype open_mode = O_RDONLY | O_WRONLY | O_RDWR
    datatype locality  = LOC_NONE | LOC_SEQ | LOC_RAN | LOC_SEQRAN   
      (* Hint to OS/2 about the intended access pattern *)
    datatype shr_mode  = DENY_RDWR | DENY_WR | DENY_RD | DENY_NONE
      (* Specifies whether other processes will be allowed to read/write *)
    datatype cr_mode   = CR_FAIL | CR_OPEN | CR_TRUNC
      (* Action taken (by createf) if the file already exists *)

    val openf   : (string * open_mode * locality * shr_mode) -> file_hdl
    val createf : (string * open_mode * locality * shr_mode * cr_mode) 
                                                             -> file_hdl
    val close   : file_hdl -> unit

    val readBinVec  : (file_hdl * int) -> Word8Vector.vector
    val readBinArr  : 
      (file_hdl * {data: Word8Array.array, first: int, nelems: int}) -> int
    val writeBinVec : 
      (file_hdl * {data: Word8Vector.vector, first: int, nelems: int}) -> int
    val writeBinArr : 
      (file_hdl * {data: Word8Array.array, first: int, nelems: int}) -> int

    val readTxtVec  : (file_hdl * int) -> CharVector.vector
    val readTxtArr  :
      (file_hdl * {data: CharArray.array, first: int, nelems: int}) -> int
    val writeTxtVec :
      (file_hdl * {data: CharVector.vector, first: int, nelems: int}) -> int
    val writeTxtArr :
      (file_hdl * {data: CharArray.array, first: int, nelems: int}) -> int

    datatype whence = F_BEGIN | F_CURRENT | F_END
    eqtype offset
      sharing type offset = Offset.int

    val setFilePtr : (file_hdl * offset * whence) -> offset

    val queryFileSize : file_hdl -> offset
    (* NOTE:
     * Maybe it'd make sense to have all the queryFile* and setFile*
     * operations too, like queryPath* and setPath* in OS2_FileSys...?
     *)

    val stdin  : file_hdl
    val stdout : file_hdl
    val stderr : file_hdl

  end (* signature OS2_IO *)


(*
 * $Log: os2-io-sig.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:40  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.1.1.1  1997/01/14 01:38:21  george
 *   Version 109.24
 *
 *)
