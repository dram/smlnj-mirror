structure RealDbg : DEBUGINTERFACE =
struct
  open DbgRun  (* to get datatype 'a result *)
  val init = DbgRun.init
  val Xabort = DbgCom.Xabort
  val Xcomplete = DbgCom.Xcomplete
  val commit = DbgInstr.commit
  val instrumDec = DbgInstr.instrumDec
  val printDec = DPrtAbs.printDec
  val sizereport = DbgUtil.sizereport
  val hideFile = DbgInstr.hideFile
end
