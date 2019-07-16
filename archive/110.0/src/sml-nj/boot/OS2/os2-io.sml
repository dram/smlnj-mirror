(* os2-io.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

(* os2-io.sml
 *
 * The OS/2 I/O interface.
 *
 * Peter Bertelsen, September 1995.
 *)

structure OS2_IO : OS2_IO =
  struct

    val ++ = SysWord.orb
    infix ++

    type word   = SysWord.word
    type s_int  = SysInt.int
    type offset = OS2_FileSys.offset

    fun os2Func x = CInterface.c_function "SMLNJ-OS2" x

    datatype file_hdl = FH of s_int
    fun fhToIOD (FH fh) = PreBasis.IODesc fh
    fun iodToFH (PreBasis.IODesc fh) = SOME(FH fh)

    (* The following conversions to word values are temporary solutions; 
       we should probably use something like osVal instead... *)

    datatype open_mode = O_RDONLY | O_WRONLY | O_RDWR
    fun omodeToWord omode = (case omode of
      O_RDONLY => 0w0 | O_WRONLY => 0w1 | O_RDWR => 0w2
      (* endcase *)) : word

    datatype locality = LOC_NONE | LOC_SEQ | LOC_RAN | LOC_SEQRAN
    fun locToWord loc = (case loc of
      LOC_NONE => 0w0 | LOC_SEQ => 0w256 | LOC_RAN => 0w512 | LOC_SEQRAN => 0w768
      (* endcase *)) : word

    datatype shr_mode = DENY_RDWR | DENY_WR | DENY_RD | DENY_NONE
    fun shrToWord shr = (case shr of
      DENY_RDWR => 0w16 | DENY_WR => 0w32 | DENY_RD => 0w48 | DENY_NONE => 0w64
      (* endcase *)) : word

    datatype cr_mode = CR_FAIL | CR_OPEN | CR_TRUNC
    fun crToWord cr = (case cr of 
      CR_FAIL => 0w0 | CR_OPEN => 0w1 | CR_TRUNC => 0w2
      (* endcase *)) : word

    val openf' : (string * int * int) -> s_int = os2Func "open"
    fun openf (name, omode, loc, shr) = let
      val flags = 1   (* open file, it must exist *)
      val mode  = SysWord.wordToInt 
                    (omodeToWord omode ++ locToWord loc ++ shrToWord shr)
      in
        FH (openf'(name, flags, mode))
      end
    fun createf (name, omode, loc, shr, cr) = let
      val flag_create = 0w16 : word   (* create file if it doesn't exist *)
      val flags = SysWord.wordToInt (flag_create ++ crToWord cr)
      val mode  = SysWord.wordToInt 
                    (omodeToWord omode ++ locToWord loc ++ shrToWord shr)
      in
        FH (openf'(name, flags, mode))
      end

    val close' : s_int -> unit = os2Func "close"
    fun close (FH fh) = close' fh

    fun checkBounds ({data, first, nelems}, len: int) =
      if (0 <= first) andalso (0 <= nelems) andalso (first+nelems <= len)
        then ()
        else raise Subscript
 
    (* Binary I/O *)
    val readBinVec' : (s_int * int) -> Word8Vector.vector = 
      os2Func "readBinVec"
    fun readBinVec (FH fh, count) = if count >= 0
      then readBinVec'(fh, count)
      else raise Subscript

    val readBinArr' : (s_int * Word8Array.array * int * int) -> int =
      os2Func "readBinArr"
    fun readBinArr (FH fh, arg as {data,first,nelems}) =
      (checkBounds(arg, Word8Array.length data);
       readBinArr'(fh, data, first, nelems))

    val writeBinVec' : (s_int * Word8Vector.vector * int * int) -> int =
      os2Func "writeBinBuf"
    fun writeBinVec (FH fh, arg as {data,first,nelems}) =
      (checkBounds(arg, Word8Vector.length data);
       writeBinVec'(fh, data, first, nelems))

    val writeBinArr' : (s_int * Word8Array.array * int * int) -> int =
      os2Func "writeBinBuf"
    fun writeBinArr (FH fh, arg as {data,first,nelems}) =
      (checkBounds(arg, Word8Array.length data);
       writeBinArr'(fh, data, first, nelems))

    (* Text I/O *)
    val readTxtVec' : (s_int * int) -> CharVector.vector =
      os2Func "readTxtVec"
    fun readTxtVec (FH fh, count) = if (count >= 0)
      then readTxtVec'(fh, count)
      else raise Subscript

    val readTxtArr' : (s_int * CharArray.array * int * int) -> int =
      os2Func "readTxtArr"
    fun readTxtArr (FH fh, arg as {data,first,nelems}) =
      (checkBounds(arg, CharArray.length data);
       readTxtArr'(fh, data, first, nelems))

    val writeTxtVec' : (s_int * CharVector.vector * int * int) -> int =
      os2Func "writeTxtBuf"
    fun writeTxtVec (FH fh, arg as {data,first,nelems}) =
      (checkBounds(arg, CharVector.length data);
       writeTxtVec'(fh, data, first, nelems))

    val writeTxtArr' : (s_int * CharArray.array * int * int) -> int =
      os2Func "writeTxtBuf"
    fun writeTxtArr (FH fh, arg as {data,first,nelems}) =
      (checkBounds(arg, CharArray.length data);
       writeTxtArr'(fh, data, first, nelems))

    datatype whence = F_BEGIN | F_CURRENT | F_END
    fun whToInt F_BEGIN   = 0
      | whToInt F_CURRENT = 1
      | whToInt F_END     = 2

    val setFilePtr' : (s_int * offset * int ) -> offset = os2Func "setFilePtr"
    fun setFilePtr (FH fh, offs, wh) = setFilePtr' (fh, offs, whToInt wh)

    val queryFileSize' : s_int -> offset = os2Func "queryFileSize"
    fun queryFileSize (FH fh) = queryFileSize' fh

    val stdin  = FH 0
    val stdout = FH 1
    val stderr = FH 2
 
  end (* structure OS2_IO *)


(*
 * $Log: os2-io.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:40  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.1.1.1  1997/01/14 01:38:21  george
 *   Version 109.24
 *
 *)
