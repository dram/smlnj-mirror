signature DEBUGINTERFACE = 
sig
    datatype 'a result = NORMAL of 'a | EXCEPTION of exn | SUSPENDED | ABORTED | INTERRUPTED
    val init: (unit -> 'a) -> 'a result
    val Xabort: unit -> unit
    val Xcomplete : unit -> unit
    val commit: unit -> unit
    val instrumDec: string * Absyn.dec -> Absyn.dec
    val printDec: Absyn.dec * (int->string*int*int) * int * int -> unit
    val sizereport: string -> unit
    val hideFile: string -> unit
end

structure BogusDebug : DEBUGINTERFACE =
struct
   datatype 'a result = NORMAL of 'a | EXCEPTION of exn | SUSPENDED | ABORTED | INTERRUPTED
   fun init _ = ABORTED
   fun Xabort () = ()
   fun Xcomplete() = ()
   fun commit() = ()
   fun instrumDec(_,a) = a
   fun printDec _ = ()
   fun sizereport _ = ()
   fun hideFile _ = ()
end
