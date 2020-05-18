structure MDError : MD_ERROR =
struct

   val loc = ref SourceMap.dummyLoc
   val errorCount   = ref 0

   fun init() = (errorCount := 0; loc := SourceMap.dummyLoc)

   exception Error

   fun setLoc l = loc := l

   fun withLoc l f x =
   let val p = !loc
       (* val _ = print(SourceMap.toString l^"\n") *)
       val _ = setLoc l
       val y = f x
   in  setLoc p;
       y
   end
   fun log msg = (TextIO.output(TextIO.stdErr,msg^"\n"))
   fun error msg = (log(SourceMap.toString (!loc)^": "^msg); 
                    errorCount := !errorCount + 1)
   fun errorPos(l, msg) = (setLoc l; error msg)
   fun warning msg = (log(SourceMap.toString (!loc)^": warning: "^msg)) 
   fun warningPos(l, msg) = (setLoc l; warning msg)
   fun fail msg = (error msg; raise Error)
end
