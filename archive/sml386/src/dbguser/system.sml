(* Rebind the cd function to update currentWD when called.
 * Also make cd available at top-level. *)

structure System =
    struct open System
    structure Directory =
	struct open Directory
	    fun cd s = (Directory.cd s;
			UserDebug.currentWD :=
			  absolute s (!UserDebug.currentWD))
	end
    end

val cd = System.Directory.cd  (* convenience *)

(* Rebind the use function to usedbg or uselive when appropriate. *)

structure IO =
    struct open IO
	fun use file =
	    case !(UserDebug.inUseDbg) of
		USE_DEBUG => UserDebug.usedbg file
	      | USE_LIVEDEBUG => UserDebug.uselive file
	      | USE_NODEBUG => IO.use file
    end

