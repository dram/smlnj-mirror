signature MLRISC_TIMING =
sig

    val timePhase : string -> ('a -> 'b) -> 'a -> 'b
end

structure MLRISC_Timing : MLRISC_TIMING =
struct

   structure C = MLRISC_Control

   fun timePhase name f =
   let val timing as ref {gc,usr,sys} = C.getTiming name
       fun run x = 
       let val timer = Timer.startCPUTimer()
           fun update timer = 
           let val {gc=gc',usr=usr',sys=sys'} = Timer.checkCPUTimer timer
           in  timing := {gc=Time.+(gc,gc'),
                          usr=Time.+(usr,usr'),
                          sys=Time.+(sys,sys')}
           end
           val y = f x handle e => (update timer; raise e)
       in  update timer; y
       end
   in  run end
  
end
