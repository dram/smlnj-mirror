signature IRCONSTANT = sig
  structure F : FRAME
  datatype const = 
      FRAMESIZE of F.frame
    | FRAMESLOT of {slot:int, frame:F.frame}
    | ARGSLOT of {slot:int, frame:F.frame}
  val valueOf : const -> int
  val toString : const -> string
end
