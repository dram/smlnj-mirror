(* win32-text-prim-io.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 * Implementation of Win32 text primitive IO.
 *
 *)

local
    structure Word32 = Word32Imp
    structure OS = OSImp
    structure String = StringImp
    structure Int = IntImp
in
structure Win32TextPrimIO : sig
                                include OS_PRIM_IO

                                val stdIn  : unit -> PrimIO.reader
				val stdOut : unit -> PrimIO.writer
				val stdErr : unit -> PrimIO.writer

				val strReader : string -> PrimIO.reader
			    end = 
    struct
	structure PrimIO = TextPrimIO

	structure W32FS = Win32.FileSys
	structure W32IO = Win32.IO
	structure W32G = Win32.General

	structure V = CharVector

	type file_desc = W32G.hndl

	val say = W32G.logMsg

	fun announce s x y = (
(**	    say "announce Win32TextPrimIO: "; say (s:string); say "\n"; **)
	     x y)

	val bufferSzB = 4096

	fun mkReader {initBlkMode=false,...} = 
	    raise IO.NonblockingNotSupported
	  | mkReader {fd,name,initBlkMode} = 
	    let val closed = ref false
		fun ensureOpen f x = 
		    if !closed then raise IO.ClosedStream else f x
		val blocking = ref initBlkMode
		val iod = W32FS.hndlToIOD fd
		fun readVec n = announce "readVecTxt" 
		                  W32IO.readVecTxt(W32FS.IODToHndl iod,n)
		fun readArr arg = announce "readArrTxt" 
		                    W32IO.readArrTxt(W32FS.IODToHndl iod,arg)
		fun close () = 
		    if !closed then ()
		    else (closed:=true; announce "close" 
			                  W32IO.close (W32FS.IODToHndl iod))
	    in
		PrimIO.RD{
		    name = name,
		    chunkSize = bufferSzB,
		    readVec = SOME (ensureOpen readVec),
		    readArr = SOME (ensureOpen readArr),
		    readVecNB = NONE,
		    readArrNB = NONE,
		    block = NONE,
		    canInput = NONE,
		    avail = fn () => NONE,
		    getPos = NONE,
		    setPos = NONE,
		    endPos = NONE,
		    verifyPos = NONE,
		    close = close,
	            ioDesc = SOME iod
		}
	    end

	val shareAll = Word32.orb(W32IO.FILE_SHARE_READ,
				  W32IO.FILE_SHARE_WRITE)

	fun checkHndl name h = 
	    if W32G.isValidHandle h then h
	    else raise OS.SysErr ("Win32TextPrimIO:"^name^": failed",NONE)

	fun openRd name = 
	    mkReader{
	        fd = checkHndl "openRd" 
		               (announce "createFile" 
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
		fun ensureOpen () = 
		    if !closed then raise IO.ClosedStream else ()
		val iod = W32FS.hndlToIOD fd
		fun writeVec v = announce "writeVec" 
		                   W32IO.writeVecTxt (W32FS.IODToHndl iod,v)
		fun writeArr v = announce "writeArr" 
		                   W32IO.writeArrTxt (W32FS.IODToHndl iod,v)
		fun close () = 
		    if !closed then ()
		    else (closed:=true; 
			  announce "close" 
			    W32IO.close (W32FS.IODToHndl iod))
	    in
		PrimIO.WR{
			  name		= name,
			  chunkSize	= chunkSize,
			  writeVec	= SOME writeVec,
			  writeArr	= SOME writeArr,
			  writeVecNB	= NONE,
			  writeArrNB	= NONE,
			  block		= NONE,
			  canOutput	= NONE,
			  getPos	= NONE,
			  setPos	= NONE,
			  endPos	= NONE,
			  verifyPos	= NONE,
			  close		= close,
			  ioDesc	= SOME iod
			 }
	    end

	fun openWr name = 
	    mkWriter{
	        fd = checkHndl "openWr" 
		               (announce "createFile" 
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
		                  (announce "createFile" 
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

	fun stdIn () = 
	    let val h = W32IO.getStdHandle(W32IO.STD_INPUT_HANDLE)
	    in
		if W32G.isValidHandle h then
		    mkReader{fd = h,
			     name = "<stdIn>",
			     initBlkMode = true}
	
		else
		    raise OS.SysErr("Win32TextPrimIO: can't get stdin",NONE)
	    end

	fun stdOut () = 
	    let val h = W32IO.getStdHandle(W32IO.STD_OUTPUT_HANDLE)
	    in
		if W32G.isValidHandle h then
		    mkWriter{fd = h,
			     name = "<stdOut>",
			     initBlkMode = true,
			     appendMode = true,
			     chunkSize = bufferSzB}
		else
		    raise OS.SysErr("Win32TextPrimIO: can't get stdout",NONE)
	    end

	fun stdErr () = 
	    let val h = W32IO.getStdHandle(W32IO.STD_ERROR_HANDLE)
	    in
		if W32G.isValidHandle h then
		    mkWriter{fd = h,
			     name = "<stdErr>",
			     initBlkMode = true,
			     appendMode = true,
			     chunkSize = bufferSzB}
		else
		    raise OS.SysErr("Win32TextPrimIO: can't get stderr",NONE)
	    end

	
	fun strReader src = (* stolen wholesale from posix-text-prim-io.sml *)
	    let val pos = ref 0
		val closed = ref false
		fun checkClosed () = if !closed then raise IO.ClosedStream else ()
		val len = String.size src
		fun avail () = (len - !pos)
		fun readV n = 
		    let val p = !pos
			val m = Int.min(n, len-p)
		    in
			checkClosed ();
			pos := p+m;
			(** NOTE: could use unchecked operations here **)
			String.substring (src, p, m)
		    end
		fun readA {buf, i, sz} = 
		    let	val p = !pos
			val m = 
			    (case sz
				 of NONE => Int.min(CharArray.length buf-i, len-p)
			       | (SOME n) => Int.min(n, len-p)
		            )
		    in
			checkClosed ();
			pos := p+m;
			CharArray.copyVec{src=src,si=p,len=SOME m,dst=buf,di=i};
			m
		    end
		fun getPos () = (checkClosed(); !pos)
	    in
		PrimIO.RD{
		    name      = "<string>", 
		    chunkSize = len,
		    readVec   = SOME readV,
        	    readArr   = SOME readA,
		    readVecNB = SOME(SOME o readV),
		    readArrNB = SOME(SOME o readA),
		    block     = SOME checkClosed,
		    canInput  = SOME(fn () => (checkClosed(); true)),
		    avail     = SOME o avail,
		    getPos    = SOME getPos,
		    setPos    = SOME(fn i => (checkClosed();
					      if (i < 0) orelse (len < i)
						  then raise Subscript
					      else ();
					      pos := i)),
		    endPos    = SOME(fn () => (checkClosed(); len)),
		    verifyPos = SOME getPos,
		    close     = fn () => closed := true,
		    ioDesc    = NONE
		}
	    end

    end
end

