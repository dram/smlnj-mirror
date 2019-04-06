structure RealDebug : DEBUGINTERFACE =
struct
  open DebugRun  (* to get datatype 'a result *)
  val init = DebugRun.init
  val Xabort = DebugCommands.Xabort
  val Xcomplete = DebugCommands.Xcomplete
  val commit = DebugInstrum.commit
  val instrumDec = DebugInstrum.instrumDec
  val printDec = DPrintAbsyn.printDec
  val sizereport = DebugUtil.sizereport
  val hideFile = DebugInstrum.hideFile
end
