(* win32-io.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 * Hooks to Win32 IO system.
 *
 *)

structure Win32_IO : WIN32_IO = 
    struct
	structure W32G = Win32_General
	type hndl = W32G.hndl

	type word = W32G.word

	type offset = Position.int

	fun cf name = W32G.cfun "WIN32-IO" name

	val setFilePointer' : (hndl * word * word) -> word =
	    cf "set_file_pointer"

	val cc = W32G.getConst "FILE"
	val FILE_BEGIN : word = cc "BEGIN"
	val FILE_CURRENT : word = cc "CURRENT"
        val FILE_END : word = cc "END"

	val readVec' : hndl * int -> Word8Vector.vector = cf "read_vec"
	val readArr' : (hndl * Word8Array.array * int * int)
	              -> int = cf "read_arr"

	val readVecTxt' : hndl * int -> CharVector.vector = cf "read_vec_txt"
	val readArrTxt' : (hndl * CharArray.array * int * int)
	    -> int = cf "read_arr_txt"

	fun vecF f (h,i) = 
	    if i < 0 then raise Subscript else f(h,i)

	fun bufF (f,lenF) (h,{buf,i,sz=NONE}) =
	    let val alen = lenF buf
	    in	if 0 <= i andalso i <= alen then 
		    f(h,buf,alen-i,i)
		else raise Subscript
	    end
	  | bufF (f,lenF) (h,{buf,i,sz=SOME sz}) = 
	    let val alen = lenF buf
	    in  if 0 <= i andalso 0 <= sz andalso i + sz <= alen then
		    f(h,buf,sz,i)
		else raise Subscript
	    end

	val readVec = vecF readVec'
	val readArr = bufF (readArr',Word8Array.length)
	val readVecTxt = vecF readVecTxt'
	val readArrTxt = bufF (readArrTxt',CharArray.length)

	val close : hndl -> unit = cf "close"

	val cc = W32G.getConst "GENERIC"
	val GENERIC_READ : word = cc "READ"
	val GENERIC_WRITE : word = cc "WRITE"

	val cc = W32G.getConst "FILE_SHARE"
	val FILE_SHARE_READ : word = cc "READ"
	val FILE_SHARE_WRITE : word = cc "WRITE"

	val cc = W32G.getConst "FILE_FLAG"
	val FILE_FLAG_WRITE_THROUGH : word = cc "WRITE_THROUGH"
	val FILE_FLAG_OVERLAPPED : word = cc "OVERLAPPED"
	val FILE_FLAG_NO_BUFFERING : word = cc "NO_BUFFERING"
	val FILE_FLAG_RANDOM_ACCESS : word = cc "RANDOM_ACCESS"
	val FILE_FLAG_SEQUENTIAL_SCAN : word = cc "SEQUENTIAL_SCAN"
	val FILE_FLAG_DELETE_ON_CLOSE : word = cc "DELETE_ON_CLOSE"
	val FILE_FLAG_BACKUP_SEMANTICS : word = cc "BACKUP_SEMANTICS"
	val FILE_FLAG_POSIX_SEMANTICS : word = cc "POSIX_SEMANTICS"

	val cc = W32G.getConst "FILE_MODE"
	val CREATE_NEW : word = cc "CREATE_NEW"
	val CREATE_ALWAYS : word = cc "CREATE_ALWAYS"
	val OPEN_EXISTING : word = cc "OPEN_EXISTING"
	val OPEN_ALWAYS : word = cc "OPEN_ALWAYS"
	val TRUNCATE_EXISTING : word = cc "TRUNCATE_EXISTING"

	                   (* name, access, share, mode, attrs *)
	val createFile' : (string * word * word * word * word) -> hndl =
	    cf "create_file"

	fun createFile {name:string,
		        access:word,share:word,mode:word,attrs:word} = 
	    createFile'(name,access,share,mode,attrs)

	val writeVec' : (hndl * Word8Vector.vector * int * int) -> int = 
	    cf "write_vec"
	val writeArr' : (hndl * Word8Array.array * int * int) -> int =
	    cf "write_arr"

	val writeVecTxt' : (hndl * CharVector.vector * int * int) -> int =
	    cf "write_vec_txt"
	val writeArrTxt' : (hndl * CharArray.array * int * int) -> int = 
	    cf "write_arr_txt"

	val writeVec = bufF (writeVec',Word8Vector.length)
	val writeArr = bufF (writeArr',Word8Array.length)
	val writeVecTxt = bufF (writeVecTxt',CharVector.length)
	val writeArrTxt = bufF (writeArrTxt',CharArray.length)

	val cc = W32G.getConst "STD_HANDLE"
	val STD_INPUT_HANDLE : word = cc "INPUT"
	val STD_OUTPUT_HANDLE : word = cc "OUTPUT"
	val STD_ERROR_HANDLE : word = cc "ERROR"

	val getStdHandle : Win32_General.word -> hndl = cf "get_std_handle"
    end

