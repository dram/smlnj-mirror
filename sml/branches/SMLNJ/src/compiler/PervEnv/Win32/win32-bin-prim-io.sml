(* win32-bin-prim-io.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 * Implementation of Win32 binary primitive IO.
 *
 *)

structure Win32BinPrimIO : OS_PRIM_IO = 
    struct
	structure PrimIO = BinPrimIO

	structure W32FS = Win32.FileSys
	structure W32IO = Win32.IO
	structure W32G = Win32.General

	structure V = Word8Vector

	type file_desc = W32G.hndl

	val pfi = Position.fromInt
	val pti = Position.toInt
	val pfw = Position.fromInt o W32G.Word.toInt
	val ptw = W32G.Word.fromInt o Position.toInt
	    
	val say = W32G.logMsg

	fun announce s x y = (
(**	    say "Win32BinPrimIO: "; say (s:string); say "\n";  **)
	    x y)

	val bufferSzB = 4096

	val seek = pfw o W32IO.setFilePointer'

	fun posFns iod = 
	    if (OS.IO.kind iod = OS.IO.Kind.file) then 
		let val pos : Position.int ref = ref(pfi 0)
		    fun getPos () : Position.int = !pos
		    fun setPos p = 
			pos := announce "setPos:seek" 
			         seek (W32FS.IODToHndl iod,
				       ptw p,
				       W32IO.FILE_BEGIN)
		    fun endPos () : Position.int = 
			(case W32FS.getLowFileSize (W32FS.IODToHndl iod) of
			     SOME w => pfw w
			   | _ => raise OS.SysErr("endPos: no file size", NONE))
		    fun verifyPos () = 
			(pos := announce "verifyPos:seek"
			          seek (W32FS.IODToHndl iod,
					0wx0,
					W32IO.FILE_CURRENT);
			 !pos)
		in
		    ignore (verifyPos());
		    { pos=pos,
		      getPos=SOME getPos,
		      setPos=SOME setPos,
		      endPos=SOME endPos,
		      verifyPos=SOME verifyPos
		    }
		end
	    else { pos=ref(pfi 0),
		   getPos=NONE,setPos=NONE,endPos=NONE,verifyPos=NONE
		 }

	fun addCheck f (SOME g) = SOME (f g)
	  | addCheck _ NONE = NONE

	fun mkReader {initBlkMode=false,...} = 
	    raise IO.NonblockingNotSupported
	  | mkReader {fd,name,initBlkMode} = 
	    let val closed = ref false
		fun ensureOpen f x = 
		    if !closed then raise IO.ClosedStream else f x
		val blocking = ref initBlkMode
		val iod = W32FS.hndlToIOD fd
		val {pos,getPos,setPos,endPos,verifyPos} = posFns iod
		fun incPos k = pos := Position.+(!pos,pfi k)
		fun readVec n = 
		    let	val v = announce "read" 
			          W32IO.readVec(W32FS.IODToHndl iod,n)
		    in  incPos (V.length v); v
		    end
		fun readArr arg = 
		    let val k = announce "readBuf" 
			          W32IO.readArr(W32FS.IODToHndl iod,arg)
		    in	incPos k; k
		    end
		fun close () = 
		    if !closed then ()
		    else (closed:=true; announce "close" 
			                  W32IO.close (W32FS.IODToHndl iod))
		fun avail () = 
		    if !closed then SOME 0
		    else (case W32FS.getLowFileSize (W32FS.IODToHndl iod) of
			      SOME w => SOME(Position.-(pfw w,!pos))
			    | NONE => NONE
			 )
	    in
		PrimIO.RD{
		    name = name,
		    chunkSize = bufferSzB,
		    readVec = SOME(ensureOpen readVec),
		    readArr = SOME(ensureOpen readArr),
		    readVecNB = NONE,
		    readArrNB = NONE,
		    block = NONE,
		    canInput = NONE,
		    avail = avail,
		    getPos = getPos,
		    setPos = addCheck ensureOpen setPos,
		    endPos = addCheck ensureOpen endPos,
		    verifyPos = addCheck ensureOpen verifyPos,
		    close = close,
		    ioDesc = SOME iod
		}
	    end

	val shareAll = W32G.Word.orb(W32IO.FILE_SHARE_READ,
				     W32IO.FILE_SHARE_WRITE)

	fun checkHndl name h = 
	    if W32G.isValidHandle h then h
	    else 
		raise OS.SysErr ("win32-bin-prim-io:checkHndl: "^name^": failed",NONE)

	fun openRd name = 
	    mkReader{
	        fd = checkHndl "openRd" 
		               (announce ("openRd:createFile:"^name)
				         W32IO.createFile{
				             name=name,
					     access=W32IO.GENERIC_READ,
					     share=shareAll,
					     mode=W32IO.OPEN_EXISTING,
					     attrs=0wx0
				         }),
		name = name,
		initBlkMode = true
	    }

	fun mkWriter {initBlkMode=false,...} =
	    raise IO.NonblockingNotSupported
	  | mkWriter {fd,name,initBlkMode,appendMode,chunkSize} = 
	    let val closed = ref false
		val blocking = ref initBlkMode
		fun ensureOpen f x = 
		    if !closed then raise IO.ClosedStream else f x
		val iod = W32FS.hndlToIOD fd
		val {pos,getPos,setPos,endPos,verifyPos} = posFns iod
		fun incPos k = pos := Position.+(!pos,pfi k)
		fun writeVec v = 
		    let val k = announce "writeVec" 
			          W32IO.writeVec (W32FS.IODToHndl iod,v)
		    in  incPos k; k
		    end
		fun writeArr v = 
		    let val k = announce "writeArr" 
			          W32IO.writeArr (W32FS.IODToHndl iod,v)
		    in  incPos k; k
		    end
		fun close () = 
		    if !closed then ()
		    else (closed:=true; 
			  announce "close" 
			    W32IO.close (W32FS.IODToHndl iod))
	  in
	    PrimIO.WR{
		name = name,
		chunkSize = chunkSize,
		writeVec = SOME(ensureOpen writeVec),
		writeArr = SOME(ensureOpen writeArr),
		writeVecNB = NONE,
		writeArrNB = NONE,
		block = NONE,
		canOutput = NONE,
		getPos = getPos,
		setPos = addCheck ensureOpen setPos,
		endPos = addCheck ensureOpen endPos,
		verifyPos = addCheck ensureOpen verifyPos,
		close = close,
		ioDesc = SOME iod
	      }
	  end

	fun openWr name = 
	    mkWriter{
	        fd = checkHndl "openWr" 
		               (announce ("openWr:createFile:"^name)
				         W32IO.createFile{
					     name=name,
					     access=W32IO.GENERIC_WRITE,
					     share=shareAll,
					     mode=W32IO.CREATE_ALWAYS,
					     attrs=W32FS.FILE_ATTRIBUTE_NORMAL
					 }),
		name = name,
		initBlkMode = true,
		appendMode = false,
		chunkSize = bufferSzB
	    }

	fun openApp name = 
	    let val h = checkHndl "openApp" 
		                  (announce ("openApp:createFile:"^name)
				            W32IO.createFile{
					        name=name,
						access=W32IO.GENERIC_WRITE,
					        share=shareAll,
					        mode=W32IO.OPEN_EXISTING,
					        attrs=W32FS.FILE_ATTRIBUTE_NORMAL
					    })
		val _ = announce "setFilePointer'"
		                 W32IO.setFilePointer' (h,0wx0,W32IO.FILE_END)
	    in
		mkWriter{
		    fd = h,
		    name = name,
		    initBlkMode = true,
		    appendMode = true,
		    chunkSize = bufferSzB
	        }
	    end

    end

(*
 * $Log: win32-bin-prim-io.sml,v $
 * Revision 1.1.1.1  1997/01/14 01:38:26  george
 *   Version 109.24
 *
 *)
