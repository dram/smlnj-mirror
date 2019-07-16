(* os2-prim-io.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * This is the OS/2 specific implementation of the I/O stack.
 *
 * Peter Bertelsen, August 1995.
 *
 *)

structure OS2PrimIO : OS_PRIM_IO =
  struct

    structure BinPrimIO  = BinPrimIO
    structure TextPrimIO = TextPrimIO
    structure OS2IO      = OS2.IO

    type file_desc = OS2IO.file_hdl

    val toFPI = FilePosInt.fromDefault  (* temporary *)

    fun appOpt f  NONE    = NONE
      | appOpt f (SOME x) = SOME(f x)

    fun  announce s x y = ((* print "OS/2: "; print (s:string); print "\n"; *)
                           x y)

    val NoUnblockingIO = Fail "unblocking I/O not supported"

    val bufSize = 2048  (* largest size string allocated in allocation-space
                           by SML/NJ runtime system, June 1995 *)


    fun mkReader {fd, name, initPos, initBlkMode} = let
      val closed = ref false
      val pos = ref initPos
      fun ensureOpen arg = if !closed then raise ClosedStream else arg
      fun incPos k = pos:= FilePosInt.+(!pos, toFPI k)
      fun readVec n = let val v = announce "readVec" OS2IO.readVec(fd, n)
        in 
          incPos (Word8Vector.length v);  v
        end
      fun readArr {data, first, nelems} = let
        val k = announce "readArr"
                  OS2IO.readArr(fd, {buf=data, i=first, sz=SOME nelems})
        in 
          incPos k;  k
        end
      fun close () = if !closed then ()
        else (closed:= true; announce "close" OS2IO.close fd)
      fun getPos () = !pos
      fun findPos ({data, first, nelems}, p) = FilePosInt.+(p, toFPI nelems)
      fun setPos p = (pos:= announce "setFilePtr"
                              OS2IO.setFilePtr(fd, p, OS2IO.F_BEGIN))
      fun endPos () = OS2IO.queryFileSize fd
    in
      if initBlkMode
        then BinPrimIO.Rd{
          readaNoBlock = NONE,
          readNoBlock = NONE,
          readaBlock = SOME(readArr o ensureOpen),
          readBlock = SOME(readVec o ensureOpen),
          block     = NONE,
          canInput  = NONE,
          name      = name,
          chunkSize = bufSize,
          close     = close,
          getPos    = getPos,
          findPos   = findPos,
          setPos    = (setPos o ensureOpen),
          endPos    = (endPos o ensureOpen),
          desc      = SOME(OS2IO.fhToIOD fd)
        }
        else raise NoUnblockingIO
    end

    fun openRd name = mkReader{
      fd = announce "openf"
             OS2IO.openf(name, OS2IO.O_RDONLY, OS2IO.LOC_NONE, OS2IO.DENY_RD),
      name=name, initPos=toFPI 0, initBlkMode=true
    }

    fun stdIn() = mkReader{
      fd=OS2IO.stdin, name="<stdIn>", initPos=toFPI 0, initBlkMode=true
    }
 

    fun translateIn (rd0 as BinPrimIO.Rd{
      readBlock, readNoBlock, readaBlock, readaNoBlock, block, canInput, name, 
      chunkSize, close, getPos, findPos, setPos, endPos, desc
    }) = let
      fun skipCR #"\013" = ""
        | skipCR    c    = String.str c
      val removeCRs = String.translate skipCR

      fun retranslate (_, 0, pos) = pos
        | retranslate (readV, nelems, pos) = let
            val s = readV nelems
            val len = size s
            fun loop (i, n) = if i=len
              then retranslate(readV, n, pos+i)
              else if n=0 then pos+i
                else if CharVector.sub(s,i) = #"\013"
                  then loop(i+1, n)
                  else loop(i+1, n-1)
            in
              loop (0, nelems)
            end

      fun findPos' readV ({data, first, nelems}, pos) = let
        val p0 = getPos()
        val p1 = (setPos pos; retranslate(readV, nelems, pos))
        in
          setPos p0;  p1
        end

      val textFindPos = (case BinPrimIO.augmentIn rd0 of
            BinPrimIO.Rd{readBlock=SOME readV, ...} => findPos' readV
          | _ => (fn _ => raise Fail "cannot findPos")
          (* endcase *))
      in
        TextPrimIO.Rd{
          readaNoBlock = appOpt (fn f => appOpt removeCRs o f) readaNoBlock,
          readNoBlock = appOpt (fn f => appOpt removeCRs o f) readNoBlock,
          readaBlock = appOpt (fn f => removeCRs o f) readaBlock,
          readBlock = appOpt (fn f => removeCRs o f) readBlock,
          block     = block,
          canInput  = canInput,
          name      = name,
          chunkSize = chunkSize,
          close     = close,
          getPos    = getPos,
          findPos   = textFindPos,
          setPos    = setPos,
          endPos    = endPos,
          desc      = desc
        }
      end (* translateIn *)


    fun mkWriter {fd, name, initBlkMode, appendMode, chunkSize, lineBuf} =
    let                                (* ignored *)
      val closed = ref false
      fun ensureOpen arg = if !closed then raise ClosedStream else arg
      fun writeVec {data, first, nelems} = announce "writeVec"
        OS2IO.writeVec(fd, {buf=data, i=first, sz=SOME nelems})
      fun writeArr {data, first, nelems} = announce "writeArr"
        OS2IO.writeArr(fd, {buf=data, i=first, sz=SOME nelems})
      fun close() = if !closed then ()
        else (closed:= true; announce "close" OS2IO.close fd)
      fun getPos p = announce "setFilePtr" 
                       OS2IO.setFilePtr(fd, toFPI 0, OS2IO.F_CURRENT)
      fun setPos p = (announce "setFilePtr" 
                        OS2IO.setFilePtr(fd, p, OS2IO.F_BEGIN); ())
      fun endPos () = OS2IO.queryFileSize fd
    in
      if initBlkMode 
        then BinPrimIO.Wr{
          writeaNoBlock = NONE,
          writeNoBlock = NONE,
          writeaBlock = SOME(writeArr o ensureOpen),
          writeBlock = SOME(writeVec o ensureOpen),
          block     = NONE,
          canOutput = NONE,
          name      = name,
          chunkSize = chunkSize,
          lineBuf   = lineBuf,
          close     = close,
          getPos    = (getPos o ensureOpen),
          setPos    = (setPos o ensureOpen),
          endPos    = (endPos o ensureOpen),
          desc      = SOME(OS2IO.fhToIOD fd)
        }
        else raise NoUnblockingIO
    end

    fun openWr name = mkWriter{
      fd = announce "createf" 
             OS2IO.createf(name, OS2IO.O_WRONLY, OS2IO.LOC_NONE, 
                           OS2IO.DENY_RDWR, OS2IO.CR_TRUNC),
      name=name, initBlkMode=true, appendMode=false, lineBuf=NONE, 
      chunkSize=bufSize
    }

    fun openApp name = let
      val fh = announce "createf"
                 OS2IO.createf(name, OS2IO.O_WRONLY, OS2IO.LOC_SEQ,
                               OS2IO.DENY_RDWR, OS2IO.CR_OPEN)
      val _ = OS2IO.setFilePtr(fh, toFPI 0, OS2IO.F_END)  (* seek to eof *)
      in
        mkWriter{
          fd=fh, name=name, initBlkMode=true, appendMode=true, lineBuf=NONE,
          chunkSize=1
        }
      end

    fun stdOut() = mkWriter{
      fd=OS2IO.stdout, name="<stdOut>", initBlkMode=true, appendMode=false,
      lineBuf=SOME(fn 10 => true | _ => false), chunkSize=bufSize
    }

    fun stdErr() = mkWriter{
      fd=OS2IO.stderr, name="<stdErr>", initBlkMode=true, appendMode=false,
      lineBuf=NONE, chunkSize=1
    }


    fun translateOut (BinPrimIO.Wr{
      writeBlock, writeNoBlock, writeaBlock, writeaNoBlock, block, canOutput,
      name, chunkSize, lineBuf, close, getPos, setPos, endPos, desc
    }) = let
        fun toCRLF #"\n" = "\013\n"
          | toCRLF   c   = String.str c
        val insertCRs = String.translate toCRLF
      in
        TextPrimIO.Wr{
          writeaNoBlock = appOpt (fn f => appOpt insertCRs o f) writeaNoBlock,
          writeNoBlock = appOpt (fn f => appOpt insertCRs o f) writeNoBlock,
          writeaBlock = appOpt (fn f => insertCRs o f) writeaBlock,
          writeBlock = appOpt (fn f => insertCRs o f) writeBlock,
          block     = block,
          canOutput = canOutput,
          name      = name,
          chunkSize = chunkSize,
          lineBuf   = lineBuf,
          close     = close,
          getPos    = getPos,
          findPos   = textFindPos,
          setPos    = setPos,
          endPos    = endPos,
          desc      = desc
        }
      end (* translateOut *)

  end  (* OS2PrimIO *)


structure OS2TextBinIO = PackageIO(OS2PrimIO)

structure BinIO  = OS2TextBinIO.BinIO
structure TextIO = OS2TextBinIO.TextIO


(*
 * $Log: os2-prim-io.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:40  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.1.1.1  1997/01/14 01:38:22  george
 *   Version 109.24
 *
 *)
