(* COPYRIGHT (c) 1996 Bell Laboratories *)
(* cconfig.sml *)

signature CCONFIG = 
sig
  exception SilentException
  
  type statenv = StaticEnv.staticEnv
  type scstatenv
  val toSC : statenv -> scstatenv
  val fromSC : scstatenv -> statenv
   
  type pickle
  type hash
  type lvar = Access.lvar
  type pid = PersStamps.persstamp

  val pickUnpick : scstatenv * statenv ->
                     {hash: hash, pickle: pickle, exportLvars: lvar list,
                      exportPid: pid option, newenv: statenv}

  val makePid : scstatenv * scstatenv -> pid
  val mkMkStamp : unit -> (unit -> Stamps.stamp)

end (* signature CCONFIG *)