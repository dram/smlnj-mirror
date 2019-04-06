(* Rebind the cd function to inform us when it has been called.
 * Also make cd available at top-level. *)

structure System =
    struct open System
    structure Directory =
	struct open Directory
	    fun cd s = (Directory.cd s;
			UserDebug.cdHook s)
	end
    end

val cd = System.Directory.cd  (* convenience *)
