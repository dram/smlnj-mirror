(* COPYRIGHT (c) 1996 Bell Laboratories *)
(* intconfig.sml *)

structure IntConfig : CCONFIG = 
struct
  fun bug s = ErrorMsg.impossible ("IntConfig:" ^ s)
  exception SilentException = BatchConfig.SilentException
  
  type statenv = StaticEnv.staticEnv
  type scstatenv = StaticEnv.staticEnv
  val toSC : statenv -> scstatenv = fn x => x
  val fromSC : scstatenv -> statenv = fn x => x
   
  type pickle = unit
  type hash = unit
  type lvar = Access.lvar
  type pid = PersStamps.persstamp

  val topCount = ref 0
  val pickUnpick : scstatenv * statenv ->
                     {hash: hash, pickle: pickle, exportLvars: lvar list,
                      exportPid: pid option, newenv: statenv}
    = fn (compenv, newenv) =>
       let val _ = topCount := !topCount + 1
           val (newenv',hash,exportLvars,exportPid) = 
             PickMod.dontPickle(newenv,!topCount)
        in {hash=(),pickle=(),exportLvars=exportLvars,
	    exportPid=exportPid,newenv=newenv'}
       end

  val makePid : scstatenv * scstatenv -> pid 
    = fn _ => bug "unexpected call to makePid in IntConfig"

  val mkStamp = Stamps.new()
  val mkMkStamp : unit -> (unit -> Stamps.stamp) = fn () => mkStamp

end (* structure IntConfig *)