structure RealDebug : DEBUGINTERFACE =
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
