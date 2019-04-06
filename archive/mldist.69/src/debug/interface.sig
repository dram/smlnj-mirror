(* DebugInterface.
   This module ties the whole mess together. *)

(* This signature presents the debugger's interface to the interactive
	system (build/interact.sml). *)
signature DEBUGINTERFACE = 
sig
    datatype 'a result =
	NORMAL of 'a
      | EXCEPTION of exn
      | SUSPENDED
      | ABORTED 
      | INTERRUPTED
      | INTERPOLATION
    val init: (unit -> 'a) -> 'a result
    val abort: unit -> 'a
    val complete : unit -> 'a
    val commit: unit -> unit
    val rollback: unit -> unit
    val instrumAndInstall: string * Absyn.dec -> Absyn.dec
    val printDec: Basics.env -> Absyn.dec * (int->string*int*int) * int * int -> unit
    val sizereport: string -> unit
    val hideFile: string -> unit
    val env: Basics.env ref
    val looker: Symbol.symbol -> Basics.binding
    val blookup: int -> System.Unsafe.object option
    val bclear: unit -> unit
end

structure BogusDebug : DEBUGINTERFACE =
struct
    datatype 'a result =
	NORMAL of 'a
      | EXCEPTION of exn
      | SUSPENDED
      | ABORTED 
      | INTERRUPTED
      | INTERPOLATION
   fun init _ = ABORTED
   exception BogusDebug
   fun abort () = raise BogusDebug
   fun complete() = raise BogusDebug
   fun commit() = ()
   fun rollback() = ()
   fun instrumAndInstall (_,a) = a
   fun printDec _ _  = ()
   fun sizereport _ = ()
   fun hideFile _ = ()
   val env = ref(Env.empty: Basics.env)
   fun looker _ = raise Env.Unbound
   fun blookup _ = NONE
   fun bclear () = ()
end
