(* For now this is the dummy region *)

structure TigerRegions : REGION = struct
  type region = unit
  val stack = ()
  val memory = ()
  val toString = fn _ => ""
end
