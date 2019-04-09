(* os2-bin-prim-io.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * This implements the OS/2 version of the OS specific binary primitive
 * IO structure.
 *
 * Peter Bertelsen, October 1995.
 *
 *)

structure OS2BinPrimIO : OS_PRIM_IO =
  struct

    structure PrimIO = BinPrimIO
    structure Vec    = Word8Vector
    structure OS2IO  = OS2.IO

    type file_desc = OS2IO.file_hdl

    val toFPI = FilePosInt.fromDefault  (* temporary *)

    fun announce s x y = ((*print "OS/2: "; print (s:string); print "\n"; *)  x y)

  (* The buffer size is chosen as the largest string allocated in the
   * nursery.
   *)
    val bufferSzB = 2048

    fun mkReader {fd, name, initPos, initBlkMode} = let
      val closed = ref false
      val pos = ref initPos
      fun incPos k = pos:= FilePosInt.+(!pos, toFPI k)
      fun readVec n = let val v = announce "readBinVec" OS2IO.readBinVec(fd, n)
        in
          incPos (Vec.length v); v
        end
      fun readArr arg = let 
        val k = announce "readBinArr" OS2IO.readBinArr(fd, arg)
        in
          incPos k; k
        end

      fun ensureOpen () = if !closed then raise ClosedStream else ()
      fun readv n = (ensureOpen(); readVec n)
      fun reada arg = (ensureOpen(); readArr arg)
      fun close () = if !closed
        then ()
        else (closed:=true; announce "close" OS2IO.close fd)
      fun getPos () = !pos
      fun findPos ({data,first,nelems}, p) = FilePosInt.+(p, toFPI nelems)
      fun setPos p = (ensureOpen(); 
        pos:= announce "setFilePtr" OS2IO.setFilePtr(fd, p, OS2IO.F_BEGIN))
      fun endPos () = (ensureOpen(); OS2IO.queryFileSize fd)
      in
        if initBlkMode
          then PrimIO.Rd{
            readaNoBlock = NONE,
            readNoBlock = NONE,
            readaBlock = SOME(reada),
            readBlock = SOME(readv),
            block     = NONE,
            canInput  = NONE,
            name      = name,
            chunkSize = bufferSzB,
            close     = close,
            getPos    = getPos,
            findPos   = findPos,
            setPos    = setPos,
            endPos    = endPos,
            desc      = SOME(OS2IO.fhToIOD fd)
          }
          else raise Fail "unblocking I/O not supported"
      end

    fun openRd name = mkReader{
      fd = announce "openf" 
             OS2IO.openf(name, OS2IO.O_RDONLY, OS2IO.LOC_NONE, OS2IO.DENY_WR),
      name        = name,
      initPos     = toFPI 0,
      initBlkMode = true
    }

                                       (* ignored *)
    fun mkWriter {fd, name, initBlkMode, appendMode, chunkSize, lineBuf} = let
      val closed = ref false
      fun ensureOpen () = if !closed then raise ClosedStream else ()
      fun writeVec arg = (ensureOpen(); 
        announce "writeBinVec" OS2IO.writeBinVec(fd, arg))
      fun writeArr arg = (ensureOpen();
        announce "writeBinArr" OS2IO.writeBinArr(fd, arg))
      fun close () = if !closed 
        then ()
        else (closed:=true; announce "close" OS2IO.close fd)
      fun getPos p = (ensureOpen(); 
        announce "setFilePtr" OS2IO.setFilePtr(fd, toFPI 0, OS2IO.F_CURRENT))
      fun setPos p = (ensureOpen(); 
        announce "setFilePtr" OS2IO.setFilePtr(fd, p, OS2IO.F_BEGIN); ())
      fun endPos () = (ensureOpen(); OS2IO.queryFileSize fd)
      in
        if initBlkMode then
          PrimIO.Wr{
            writeaNoBlock = NONE,
            writeNoBlock = NONE,
            writeaBlock = SOME(writeArr),
            writeBlock = SOME(writeVec),
            block      = NONE,
            canOutput  = NONE,
            name       = name,
            chunkSize  = chunkSize,
            lineBuf    = lineBuf,
            close      = close,
            getPos     = getPos,
            setPos     = setPos,
            endPos     = endPos,
            desc       = SOME(OS2IO.fhToIOD fd)
          }
          else raise Fail "unblocking I/O not supported"
      end

    fun openWr name = mkWriter{
      fd = announce "createf"
             OS2IO.createf(name, OS2IO.O_WRONLY, OS2IO.LOC_NONE, 
                           OS2IO.DENY_RDWR, OS2IO.CR_TRUNC),
      name        = name,
      initBlkMode = true,
      appendMode  = false,
      lineBuf     = NONE,
      chunkSize   = bufferSzB
    }

    fun openApp name = let
      val fh = announce "createf"
                 OS2IO.createf(name, OS2IO.O_WRONLY, OS2IO.LOC_SEQ,
                               OS2IO.DENY_RDWR, OS2IO.CR_OPEN)
      val _ = OS2IO.setFilePtr(fh, toFPI 0, OS2IO.F_END)  (* seek to eof *)
      in 
        mkWriter{
          fd          = fh,
          name        = name,
          initBlkMode = true,
          appendMode  = true,
          lineBuf     = NONE,
          chunkSize   = 1	(* don't want to buffer these streams!! *)
        }
      end

  end; (* structure OS2BinPrimIO *)


(*
 * $Log: os2-bin-prim-io.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:40  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.1.1.1  1997/01/14 01:38:21  george
 *   Version 109.24
 *
 *)
