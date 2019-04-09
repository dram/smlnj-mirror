(*
 * cm/cleanup.sml: registering a function to initialize control state
 *                 during startup
 *
 *   Copyright (c) 1997 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
functor CleanupFun (val slists:(string * char *
				(string list option -> string list)) list
		    val bools: (string * (bool option -> bool)) list
		    val strings: (string * string ref) list): CLEANUP =
struct

    structure CleanUp = SMLofNJ.Internals.CleanUp

    val cleanerLabel = "CM-getenv"

    fun setslist (var, c, setter) =
	case OS.Process.getEnv var of
	    NONE => ()
	  | SOME s =>
		ignore (setter (SOME (String.fields (fn cc => c = cc) s)))

    fun setbool (var, s) =
	case OS.Process.getEnv var of
	    SOME "true" => ignore (s (SOME true))
	  | SOME "false" => ignore (s (SOME false))
	  | _ => ()

    fun setstring (var, r) =
	case OS.Process.getEnv var of
	    SOME s => r := s
	  | _ => ()

    fun init () = let
	fun cleaner _ =
	    (app setslist slists;
	     app setbool bools;
	     app setstring strings)
    in
	ignore (CleanUp.addCleaner (cleanerLabel, [CleanUp.AtInit], cleaner))
    end

    fun uninit () = ignore (CleanUp.removeCleaner cleanerLabel)
end
