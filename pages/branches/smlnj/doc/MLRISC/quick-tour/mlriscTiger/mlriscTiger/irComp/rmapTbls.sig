signature REGIGER_MAP_TABLES = sig
  val lookupTemp : Temp.temp -> int
  val lookupLabel: Symbol.symbol -> Label.label
  val reset : unit -> unit
end