(* internals.sig
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *
 * This structure (SMLofNJ.Internals) is a gathering place for internal
 * features that need to be exposed outside the boot directory.
 *)

signature INTERNALS =
  sig

    structure CleanUp : CLEAN_UP
    structure ProfControl : PROF_CONTROL
    structure GC : GC

    val prHook : (string -> unit) ref
	(* this hook can be used to change the top-level print function *)

  (* Routines for managing the internal signal handler tables.  These are
   * for programs that must otherwise bypass the standard initialization
   * mechanisms.
   *)
    val initSigTbl : unit -> unit
    val clearSigTbl : unit -> unit
    val resetSigTbl : unit -> unit

  (* reset the total real and CPU time timers *)
    val resetTimers : unit -> unit

  (* back-tracing control (experimental; M.Blume, 06/2000) *)
    structure BTrace : sig
	exception BTrace of unit -> string list
	val install : { corefns: { save: unit -> unit -> unit,
				   push: unit -> unit -> unit,
				   add: int * int -> unit,
				   reserve: int -> int,
				   register: int * int * string -> unit,
				   report: unit -> unit -> string list },
			reset: unit -> unit }
		      -> unit
	val mode : bool option -> bool	(* turn annotation pass on/off *)
	val report : unit -> unit -> string list
	val trigger : unit -> 'a
	(* The following is needed in evalloop.sml (or any other module
	 * that explicitly handles the BTrace exception but hasn't itself
	 * been compiled with mode=true) to make sure that the call
	 * history is being unwound correctly. *)
	val save : unit -> unit -> unit
    end

  end;
