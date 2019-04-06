signature DEBUGINTERFACE = 
sig
    datatype 'a result = NORMAL of 'a | EXCEPTION of exn | SUSPENDED | ABORTED
    val init: (unit -> 'a) -> 'a result
    val abort: unit -> 'a
    val Xcomplete : unit -> unit
    val commit: unit -> unit
    val instrumDec: Absyn.dec * (int->string*int*int) -> Absyn.dec
    val printDec: Absyn.dec * (int->string*int*int) * int * int -> unit
    val sizereport: string -> unit
end

structure BogusDebug : DEBUGINTERFACE =
struct
   datatype 'a result = NORMAL of 'a | EXCEPTION of exn | SUSPENDED | ABORTED
   fun init _ = ABORTED
   fun abort _ = let exception BogusDebug in raise BogusDebug end
   fun Xcomplete() = ()
   fun commit() = ()
   fun instrumDec(a,_) = a
   fun printDec _ = ()
   fun sizereport _ = ()
end
