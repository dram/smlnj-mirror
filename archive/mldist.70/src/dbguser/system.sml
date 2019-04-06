(* Modify System structure for outer environment:
 * (a) Rebind the cd function to update currentWD when called.
 *     Also make cd available at top-level. *)


structure System =
    struct open System
    structure Directory =
	struct open Directory
	    fun cd s = (Directory.cd s;
			UserDebugCommands.currentWD :=
			  absolute s (!UserDebugCommands.currentWD))
	end
    end

val cd = System.Directory.cd  (* convenience *)

(* Rebind the use function to usedbg or uselive when appropriate. 
 * this is probably garbage. *)

(*
structure IO =
    struct open IO
	fun use file =
	    case !(UserDebugCommands.inUseDbg) of
		USE_DEBUG => UserDebugCommands.usedbg file
	      | USE_LIVEDEBUG => UserDebugCommands.uselive file
	      | USE_NODEBUG => IO.use file
    end
*)
